/*
See local README.md on how to compile and run.
In short: gcc seccomp-gen.c -lseccomp -Wall -pedantic -o seccomp-gen
*/

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

  DENY_RULE(_sysctl);
  DENY_RULE(acct);
  DENY_RULE(add_key);
  DENY_RULE(adjtimex);
  DENY_RULE(clock_adjtime);
  DENY_RULE(create_module);
  DENY_RULE(delete_module);
  DENY_RULE(fanotify_init);
  DENY_RULE(finit_module);
  DENY_RULE(get_kernel_syms);
  DENY_RULE(get_mempolicy);
  DENY_RULE(init_module);
  DENY_RULE(io_cancel);
  DENY_RULE(io_destroy);
  DENY_RULE(io_getevents);
  DENY_RULE(io_setup);
  DENY_RULE(io_submit);
  DENY_RULE(ioperm);
  DENY_RULE(iopl);
  DENY_RULE(ioprio_set);
  DENY_RULE(kcmp);
  DENY_RULE(kexec_file_load);
  DENY_RULE(kexec_load);
  DENY_RULE(keyctl);
  DENY_RULE(lookup_dcookie);
  DENY_RULE(mbind);
  DENY_RULE(nfsservctl);
  DENY_RULE(migrate_pages);
  DENY_RULE(modify_ldt);
  DENY_RULE(mount);
  DENY_RULE(move_pages);
  DENY_RULE(perf_event_open);
  DENY_RULE(pivot_root);
  DENY_RULE(process_vm_readv);
  DENY_RULE(process_vm_writev);
  DENY_RULE(ptrace);
  DENY_RULE(reboot);
  DENY_RULE(remap_file_pages);
  DENY_RULE(request_key);
  DENY_RULE(set_mempolicy);
  DENY_RULE(swapoff);
  DENY_RULE(swapon);
  DENY_RULE(sysfs);
  DENY_RULE(syslog);
  DENY_RULE(tuxcall);
  DENY_RULE(umount2);
  DENY_RULE(uselib);
  DENY_RULE(vmsplice);

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
