#include <errno.h>
#include <fcntl.h>
#include <seccomp.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define ADD_RULE(action, call) \
  do { \
    rc = seccomp_rule_add(ctx, action, SCMP_SYS(call), 0); \
    if (rc < 0) { \
      fprintf(stderr, "seccomp_rule_add(" #call "): %s\n", strerror(-rc)); \
      goto out; \
    } \
  } while (0)
#define DENY_RULE(call) ADD_RULE(SCMP_ACT_ERRNO(EPERM), call)

int main(void)
{
  int fd, rc = -1;
  scmp_filter_ctx ctx;

  ctx = seccomp_init(SCMP_ACT_ALLOW);
  if (ctx == NULL) {
    fprintf(stderr, "seccomp_init failed\n");
    goto out;
  }

#ifdef __x86_64__
  /* Without this the filter only covers the native ABI and libseccomp kills
     everything else (SCMP_FLTATR_ACT_BADARCH defaults to SCMP_ACT_KILL), so
     any 32-bit process would die on its first syscall.  Adding the ABI makes
     the rules below apply to its syscall numbers as well. */
  rc = seccomp_arch_add(ctx, SCMP_ARCH_X86);
  if (rc < 0 && rc != -EEXIST) {
    fprintf(stderr, "seccomp_arch_add(SCMP_ARCH_X86): %s\n", strerror(-rc));
    goto out;
  }
#endif

  @rules@

  fd = open("seccomp.bpf", O_CREAT | O_TRUNC | O_WRONLY, 0644);
  if (fd == -1) {
    rc = -errno;
    fprintf(stderr, "open(seccomp.bpf): %s\n", strerror(errno));
    goto out;
  }

  rc = seccomp_export_bpf(ctx, fd);
  if (rc < 0) {
    fprintf(stderr, "seccomp_export_bpf: %s\n", strerror(-rc));
    close(fd);
    goto out;
  }

  if (close(fd) == -1) {
    rc = -errno;
    fprintf(stderr, "close(seccomp.bpf): %s\n", strerror(errno));
  }

 out:
  seccomp_release(ctx);
  return -rc;
}
