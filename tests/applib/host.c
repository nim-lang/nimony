/* A host of libapplib: it runs the library's top-level code with `NimMain`
   first, then calls into it, and flushes the library's buffered output. */
void NimMain(void);
int lib_call(void);
void nimFlushStdStreams(void);
int main(void) {
  NimMain();
  int r = lib_call();
  nimFlushStdStreams();
  return r == 2 ? 0 : 1;
}
