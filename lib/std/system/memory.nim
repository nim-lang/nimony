
proc alloc*(size: int): pointer {.importc: "mi_malloc", header: "<mimalloc.h>".}
proc dealloc*(p: pointer) {.importc: "mi_free", header: "<mimalloc.h>".}
proc realloc*(p: pointer; size: int): pointer {.importc: "mi_realloc", header: "<mimalloc.h>".}


proc allocFixed*(size: int): pointer {.importc.}
proc deallocFixed*(p: pointer) {.importc.}

proc copyMem(dest, src: pointer; size: int) {.importc: "memcpy", header: "<string.h>".}
proc cmpMem(a, b: pointer; size: int): int {.importc: "memcmp", header: "<string.h>".}

proc allocatedSize*(p: pointer): int {.importc: "mi_usable_size", header: "<mimalloc.h>".}

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
