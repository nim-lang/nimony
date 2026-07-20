#ifndef NIM_POSIX_EXECVE_H
#define NIM_POSIX_EXECVE_H

/* Minimal execve prototype for the libc-free backend. We intentionally
   do NOT `#include <unistd.h>`: it would pull in a full declaration graph
   we don't want in `-d:nimNativeIo` builds, and a mismatched bare
   `importc` binding triggers GCC's `-Wbuiltin-declaration-mismatch`
   against glibc's execve builtin. This signature matches POSIX/glibc
   exactly. */
int execve(const char *path, char *const argv[], char *const envp[]);

#endif
