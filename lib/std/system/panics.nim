## Tiny support for basic error reporting.

proc die(value: int32) {.importc: "exit", header: "<stdlib.h>".}

# Yes, that is a tiny bit of duplication from syncio.nim. Get over it.
type
  RawCFile {.importc: "FILE", header: "<stdio.h>".} = object
var
  cstderr {.importc: "stderr", header: "<stdio.h>".}: ptr RawCFile

proc c_fwrite(buf: pointer; size, n: uint; f: ptr RawCFile): uint {.
  importc: "fwrite", header: "<stdio.h>".}
proc fprintf(f: ptr RawCFile; fmt: cstring) {.varargs, importc: "fprintf", header: "<stdio.h>".}

proc writeErr(x: int64) = fprintf(cstderr, cstring"%lld", x)
proc writeErr(x: uint64) = fprintf(cstderr, cstring"%llu", x)
proc writeErr(s: string) = discard c_fwrite(rawData(s), 1'u, s.len.uint, cstderr)
proc writeErr(s: cstring) = discard c_fwrite(s, 1'u, s.len.uint, cstderr)

proc panic*(s: string) {.noinline.} =
  writeErr s
  die 1'i32

type
  HasWriteErr = concept
    proc writeErr(x: Self)

proc raiseIndexError3[T: HasWriteErr](i, a, b: T) {.noinline.} =
  writeErr "index out of bounds: "
  writeErr i
  writeErr " notin "
  writeErr a
  writeErr ".."
  writeErr b
  writeErr "\n"
  die 1'i32

proc nimIcheckAB(i, a, b: int): int {.inline, exportc: "nimIcheckAB".} =
  if i >= a and i <= b:
    result = i-a
  else:
    result = 0
    raiseIndexError3(i, a, b)

proc nimIcheckB(i, b: int): int {.inline, exportc: "nimIcheckB".} =
  if i >= 0 and i <= b:
    result = i
  else:
    result = 0
    raiseIndexError3(i, 0, b)

proc nimUcheckAB(i, a, b: uint): uint {.inline, exportc: "nimUcheckAB".} =
  result = i-a
  if result > b:
    raiseIndexError3(i, a, b)

proc nimUcheckB(i, b: uint): uint {.inline, exportc: "nimUcheckB".} =
  result = i
  if result > b:
    raiseIndexError3(i, 0, b)

proc nimInvalidObjConv(name: string) {.inline, exportc: "nimInvalidObjConv".} =
  writeErr "invalid object conversion: "
  writeErr name
  writeErr "\n"
  die 1'i32

proc nimChckNilDisp(p: pointer) {.inline, exportc: "nimChckNilDisp".} =
  if p == nil:
    writeErr "cannot dispatch; dispatcher is nil\n"
    die 1'i32
