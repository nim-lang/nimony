/* `sinkTake` of timportcsink.nim: takes the Nim string by value and ignores it. */
struct NimStr { long a; void* b; };
int sinkTake(struct NimStr s) { (void)s; return 5; }
