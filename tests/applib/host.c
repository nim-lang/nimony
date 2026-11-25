/* A host of libapplib: it runs the library's top-level code with `NimMain`
   first, then calls into it, and flushes the library's buffered output. */
#include <stdlib.h>
void NimMain(void);
int lib_call(void);
int lib_env(void);
void nimFlushStdStreams(void);
int main(void) {
  setenv("APPLIB_ENV", "abc", 1);
  NimMain();
  int r = lib_call();
  int e = lib_env();
  nimFlushStdStreams();
  return r == 2 && e == 3 ? 0 : 1;
}
