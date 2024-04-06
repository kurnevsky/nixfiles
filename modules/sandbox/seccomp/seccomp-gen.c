#include <errno.h>
#include <fcntl.h>
#include <seccomp.h>
#include <unistd.h>

#define DENY_RULE(call) \
  { \
    rc = seccomp_rule_add(ctx, SCMP_ACT_KILL, SCMP_SYS(call), 0); \
    if (rc < 0) \
      goto out; \
  }
#define ALLOW_RULE(call) \
  { \
    rc = seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(call), 0); \
    if (rc < 0) \
      goto out; \
  }

int main(int argc, char *argv[])
{
  int fd, rc = -1;
  scmp_filter_ctx ctx;

  ctx = seccomp_init(SCMP_ACT_ALLOW);
  if (ctx == NULL)
    goto out;

  @rules@

  fd = open("seccomp.bpf", O_CREAT | O_WRONLY, 0644);
  if (fd == -1) {
    rc = -errno;
    goto out;
  }

  rc = seccomp_export_bpf(ctx, fd);
  if (rc < 0) {
    close(fd);
    goto out;
  }

  if (close(fd) == -1)
    rc = -errno;

 out:
  seccomp_release(ctx);
  return -rc;
}
