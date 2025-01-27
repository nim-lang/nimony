
proc alloc*(size: int): pointer {.importc.}
proc dealloc*(p: pointer) {.importc.}
proc realloc*(p: pointer; size: int): pointer {.importc.}


proc allocFixed*(size: int): pointer {.importc.}
proc deallocFixed*(p: pointer) {.importc.}

proc copyMem(dest, src: pointer; size: int) {.importc.}
proc cmpMem(a, b: pointer; size: int): int {.importc.}

proc allocatedSize*(p: pointer): int {.importc.}

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
