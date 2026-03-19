include mimalloc

func allocFixed*(size: int): pointer =
  result = mi_malloc(size.csize_t)

func deallocFixed*(p: pointer) = mi_free(p)

func c_memcpy(dest, src: pointer; size: csize_t) {.importc: "memcpy", header: "<string.h>".}
func c_memcmp(a, b: pointer; size: csize_t): cint {.importc: "memcmp", header: "<string.h>".}
func c_memset(dest: pointer; val: cint; size: csize_t) {.importc: "memset", header: "<string.h>".}

func copyMem*(dest, src: pointer; size: int) {.inline.} =
  c_memcpy(dest, src, csize_t size)

func cmpMem*(a, b: pointer; size: int): int {.inline.} =
  result = c_memcmp(a, b, csize_t size)

func zeroMem*(dest: pointer; size: int) {.inline.} =
  c_memset(dest, 0, csize_t size)

var
  missingBytes {.threadvar.}: int

proc continueAfterOutOfMem*(size: int) {.nimcall.} =
  if missingBytes < high(int) - size:
    missingBytes = missingBytes + size
  else:
    missingBytes = high(int)

proc threadOutOfMem*(): bool = missingBytes > 0

var oomHandler: proc (size: int) {.nimcall.} = continueAfterOutOfMem

proc setOomHandler*(handler: proc (size: int) {.nimcall.}) {.inline.} =
  # XXX needs atomic store here
  oomHandler = handler
