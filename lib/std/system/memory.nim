include mimalloc

func allocFixed*(size: int): pointer =
  ## Allocates `size` bytes of uninitialized memory.
  result = mi_malloc(size.csize_t)

func deallocFixed*(p: pointer) =
  ## Frees memory allocated by `allocFixed`.
  mi_free(p)

func c_memcpy(dest, src: pointer; size: csize_t) {.importc: "memcpy", header: "<string.h>".}
func c_memcmp(a, b: pointer; size: csize_t): cint {.importc: "memcmp", header: "<string.h>".}
func c_memset(dest: pointer; val: cint; size: csize_t) {.importc: "memset", header: "<string.h>".}

func copyMem*(dest, src: pointer; size: int) {.inline.} =
  ## Copies `size` bytes from `src` to `dest`. The regions must not overlap.
  c_memcpy(dest, src, csize_t size)

func cmpMem*(a, b: pointer; size: int): int {.inline.} =
  ## Lexicographically compares `size` bytes at `a` and `b`.
  result = c_memcmp(a, b, csize_t size)

func zeroMem*(dest: pointer; size: int) {.inline.} =
  ## Sets `size` bytes at `dest` to zero.
  c_memset(dest, 0, csize_t size)

var
  missingBytes {.threadvar.}: int

proc continueAfterOutOfMem*(size: int) {.nimcall.} =
  ## Default out-of-memory handler: accumulates missing bytes so runtime code can react gracefully.
  if missingBytes < high(int) - size:
    missingBytes = missingBytes + size
  else:
    missingBytes = high(int)

proc threadOutOfMem*(): bool =
  ## True if the current thread previously ran out of memory (recorded by the OOM handler).
  missingBytes > 0

var oomHandler: proc (size: int) {.nimcall.} = continueAfterOutOfMem

proc setOomHandler*(handler: proc (size: int) {.nimcall.}) {.inline.} =
  ## Installs the procedure invoked when allocation fails (`size` is the attempted allocation).
  # XXX needs atomic store here
  oomHandler = handler
