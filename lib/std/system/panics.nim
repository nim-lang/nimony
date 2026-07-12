## Tiny support for basic error reporting.

when defined(nimNativeIo):
  # Freestanding: error text goes straight to stderr and termination funnels
  # through `cExit` (see `system/exits`), so the crash path links no libc stdio.
  when defined(windows):
    # Windows stderr is a kernel32 `HANDLE`; write to it via `WriteFile`.
    type WinHandle = pointer
    const STD_ERROR_HANDLE = 0xFFFFFFF4'u32   # (DWORD)-12
    proc cGetStdHandle(nStdHandle: uint32): WinHandle {.stdcall,
      importc: "GetStdHandle".}
    proc cWriteErrFile(h: WinHandle; buf: pointer; n: uint32;
                       written: ptr uint32; overlapped: pointer): int32 {.
      stdcall, importc: "WriteFile".}

    proc writeErr(s: string) =
      var written: uint32 = 0
      discard cWriteErrFile(cGetStdHandle(STD_ERROR_HANDLE), readRawData(s),
                            s.len.uint32, addr written, nil)
    proc writeErr(s: cstring) =
      var n = 0
      let p = cast[ptr UncheckedArray[char]](s)
      while p[n] != '\0': inc n
      var written: uint32 = 0
      discard cWriteErrFile(cGetStdHandle(STD_ERROR_HANDLE), p, n.uint32,
                            addr written, nil)
    proc writeErr(x: int64) = writeErr($x)
    proc writeErr(x: uint64) = writeErr($x)
  else:
    # POSIX: error text goes straight to fd 2 via the `write` syscall.
    proc cWriteErr(fd: cint; buf: pointer; n: uint): int {.importc: "write".}

    proc writeErr(s: string) =
      discard cWriteErr(2'i32, readRawData(s), s.len.uint)
    proc writeErr(s: cstring) =
      var n = 0
      let p = cast[ptr UncheckedArray[char]](s)
      while p[n] != '\0': inc n
      discard cWriteErr(2'i32, p, n.uint)
    proc writeErr(x: int64) = writeErr($x)
    proc writeErr(x: uint64) = writeErr($x)

  proc die(value: int32) {.noreturn.} = cExit(value.int)
else:
  proc die(value: int32) {.importc: "exit", header: "<stdlib.h>".}

  # Yes, that is a tiny bit of duplication from syncio.nim. Get over it.
  type
    RawCFile {.importc: "FILE", header: "<stdio.h>".} = object
  when defined(macos) or defined(macosx):
    var
      cstderr {.importc: "__stderrp", header: "<stdio.h>".}: ptr RawCFile
  else:
    var
      cstderr {.importc: "stderr", header: "<stdio.h>".}: ptr RawCFile

  proc c_fwrite(buf: pointer; size, n: uint; f: ptr RawCFile): uint {.
    importc: "fwrite", header: "<stdio.h>".}
  proc fprintf(f: ptr RawCFile; fmt: cstring) {.varargs, importc: "fprintf", header: "<stdio.h>".}

  proc writeErr(x: int64) = fprintf(cstderr, cstring"%lld", x)
  proc writeErr(x: uint64) = fprintf(cstderr, cstring"%llu", x)
  proc writeErr(s: string) = discard c_fwrite(readRawData(s), 1'u, s.len.uint, cstderr)
  proc writeErr(s: cstring) = discard c_fwrite(s, 1'u, s.len.uint, cstderr)

proc panic*(s: string) {.noinline, noreturn.} =
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
