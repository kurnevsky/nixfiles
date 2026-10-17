// Listens on a Unix socket and relays each connection from the given uid to a
// new connection to the target socket, including file descriptors passed with
// SCM_RIGHTS, which Wayland relies on.
//
// Usage: unix-relay LISTEN TARGET UID
//
// Exits along with its parent, and so do the connections.

#define _GNU_SOURCE
#include <err.h>
#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

// The kernel's limit of descriptors in a single message (SCM_MAX_FD).
#define MAX_FDS 253

static struct sockaddr_un address(const char *path) {
  struct sockaddr_un addr = {.sun_family = AF_UNIX};
  if (strlen(path) >= sizeof(addr.sun_path))
    errx(2, "socket path is too long: %s", path);
  strcpy(addr.sun_path, path);
  return addr;
}

static void close_fds(struct msghdr *msg) {
  for (struct cmsghdr *c = CMSG_FIRSTHDR(msg); c; c = CMSG_NXTHDR(msg, c))
    if (c->cmsg_level == SOL_SOCKET && c->cmsg_type == SCM_RIGHTS) {
      int *fds = (int *)CMSG_DATA(c);
      size_t n = (c->cmsg_len - CMSG_LEN(0)) / sizeof(int);
      for (size_t i = 0; i < n; i++)
        close(fds[i]);
    }
}

// Forwards one chunk with its descriptors, returns false when done.
static int forward(int from, int to) {
  static char buf[65536];
  static char control[CMSG_SPACE(sizeof(int) * MAX_FDS)];
  struct iovec iov = {.iov_base = buf, .iov_len = sizeof(buf)};
  struct msghdr msg = {
      .msg_iov = &iov,
      .msg_iovlen = 1,
      .msg_control = control,
      .msg_controllen = sizeof(control),
  };
  ssize_t n;
  do
    n = recvmsg(from, &msg, MSG_CMSG_CLOEXEC);
  while (n < 0 && errno == EINTR);
  if (n <= 0)
    return 0;
  if (msg.msg_flags & MSG_CTRUNC) {
    close_fds(&msg);
    warnx("dropping a connection: too many file descriptors");
    return 0;
  }

  // The descriptors go with the first part, ahead of the data referring to
  // them, as the receiver expects.
  struct msghdr out = {
      .msg_iov = &iov,
      .msg_iovlen = 1,
      .msg_control = msg.msg_controllen ? control : NULL,
      .msg_controllen = msg.msg_controllen,
  };
  for (size_t sent = 0; sent < (size_t)n;) {
    iov.iov_base = buf + sent;
    iov.iov_len = n - sent;
    ssize_t w = sendmsg(to, &out, MSG_NOSIGNAL);
    if (w < 0) {
      if (errno == EINTR)
        continue;
      close_fds(&msg);
      return 0;
    }
    sent += w;
    out.msg_control = NULL;
    out.msg_controllen = 0;
  }
  close_fds(&msg);
  return 1;
}

static void relay(int client, const char *target) {
  struct sockaddr_un addr = address(target);
  int server = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
  if (server < 0)
    err(1, "socket");
  if (connect(server, (struct sockaddr *)&addr, sizeof(addr)) < 0)
    err(1, "connect to %s", target);

  struct pollfd fds[2] = {{.fd = client, .events = POLLIN},
                          {.fd = server, .events = POLLIN}};
  for (;;) {
    if (poll(fds, 2, -1) < 0) {
      if (errno == EINTR)
        continue;
      err(1, "poll");
    }
    for (int i = 0; i < 2; i++)
      if (fds[i].revents && !forward(fds[i].fd, fds[1 - i].fd))
        exit(0);
  }
}

int main(int argc, char *argv[]) {
  if (argc != 4)
    errx(2, "usage: %s LISTEN TARGET UID", argv[0]);
  const char *listen_path = argv[1], *target = argv[2];
  char *end;
  long uid = strtol(argv[3], &end, 10);
  if (*argv[3] == '\0' || *end != '\0' || uid < 0)
    errx(2, "invalid uid: %s", argv[3]);

  if (prctl(PR_SET_PDEATHSIG, SIGTERM) < 0)
    err(1, "prctl");
  if (getppid() == 1)
    return 0;
  // Connections are handled by children, which needn't be waited for.
  signal(SIGCHLD, SIG_IGN);

  struct sockaddr_un addr = address(listen_path);
  int listener = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
  if (listener < 0)
    err(1, "socket");
  if (bind(listener, (struct sockaddr *)&addr, sizeof(addr)) < 0)
    err(1, "bind to %s", listen_path);
  // Access is checked by uid on accepting instead.
  if (chmod(listen_path, 0666) < 0)
    err(1, "chmod %s", listen_path);
  if (listen(listener, 16) < 0)
    err(1, "listen");

  for (;;) {
    int client = accept4(listener, NULL, NULL, SOCK_CLOEXEC);
    if (client < 0) {
      if (errno == EINTR || errno == ECONNABORTED)
        continue;
      err(1, "accept");
    }
    struct ucred cred;
    socklen_t len = sizeof(cred);
    if (getsockopt(client, SOL_SOCKET, SO_PEERCRED, &cred, &len) < 0 ||
        cred.uid != (uid_t)uid) {
      close(client);
      continue;
    }
    pid_t pid = fork();
    if (pid < 0)
      err(1, "fork");
    if (pid == 0) {
      close(listener);
      if (prctl(PR_SET_PDEATHSIG, SIGTERM) < 0)
        err(1, "prctl");
      relay(client, target);
    }
    close(client);
  }
}
