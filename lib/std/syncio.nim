
type
  CFile {.importc: "FILE", header: "<stdio.h>".} = object
  File* = ptr CFile ## The type representing a file handle.

  FileMode* = enum       ## The file mode when opening a file.
    fmRead,              ## Open the file for read access only.
                         ## If the file does not exist, it will not
                         ## be created.
    fmWrite,             ## Open the file for write access only.
                         ## If the file does not exist, it will be
                         ## created. Existing files will be cleared!
    fmReadWrite,         ## Open the file for read and write access.
                         ## If the file does not exist, it will be
                         ## created. Existing files will be cleared!
    fmReadWriteExisting, ## Open the file for read and write access.
                         ## If the file does not exist, it will not be
                         ## created. The existing file will not be cleared.
    fmAppend             ## Open the file for writing only; append data
                         ## at the end. If the file does not exist, it
                         ## will be created.

var
  stdin* {.importc: "stdin", header: "<stdio.h>".}: File
  stdout* {.importc: "stdout", header: "<stdio.h>".}: File
  stderr* {.importc: "stderr", header: "<stdio.h>".}: File

proc c_fwrite(buf: ptr UncheckedArray[char]; size, n: uint; f: File): uint {.
  importc: "fwrite", header: "<stdio.h>".}
proc c_fwrite(buf: ptr UncheckedArray[uint8]; size, n: uint; f: File): uint {.
  importc: "fwrite", header: "<stdio.h>".}
proc c_fputc(c: int32; f: File): int32 {.
  importc: "fputc", header: "<stdio.h>".}
proc c_fread(buf: ptr UncheckedArray[uint8]; size, n: uint; f: File): uint {.
  importc: "fread", header: "<stdio.h>".}

proc fprintf(f: File; fmt: cstring) {.varargs, importc: "fprintf", header: "<stdio.h>".}

proc write*(f: File; s: string) =
  discard c_fwrite(getData(s), 1'u, s.len.uint, f)

proc write*(f: File; b: bool) =
  if b: write f, "true"
  else: write f, "false"

proc write*(f: File; x: int64) =
  fprintf(f, cstring"%lld", x)

proc write*(f: File; x: uint64) =
  fprintf(f, cstring"%llu", x)

proc write*[T: enum](f: File; x: T) =
  write f, $x

proc write*(f: File; c: char) =
  discard c_fputc(int32(c), f)

proc write*(f: File; x: float) =
  fprintf(f, cstring"%g", x)

proc fclose(f: File): int32 {.importc: "fclose", header: "<stdio.h>".}

proc close*(f: File) = discard fclose(f)

proc fopen(filename, mode: cstring): File {.importc: "fopen", header: "<stdio.h>".}

proc writeBuffer*(f: File; buffer: ptr UncheckedArray[uint8]; size: int): int =
  result = cast[int](c_fwrite(buffer, 1'u, cast[uint](size), f))

proc readBuffer*(f: File; buffer: ptr UncheckedArray[uint8]; size: int): int =
  result = cast[int](c_fread(buffer, 1'u, cast[uint](size), f))

proc c_ferror(f: File): int32 {.
  importc: "ferror", header: "<stdio.h>".}

proc failed*(f: File): bool {.inline.} = c_ferror(f) != 0

proc c_setvbuf(f: File; buffer: nil ptr UncheckedArray[uint8]; mode: int32; size: uint): int32 {.
  importc: "setvbuf", header: "<stdio.h>".}

var IOFBF {.importc: "_IOFBF", header: "<stdio.h>".}: int32

proc open*(f: out File; filename: string;
           mode: FileMode = fmRead;
           bufSize: int = -1): bool =
  let m =
    case mode
    of fmRead: cstring"rb"
    of fmWrite: cstring"wb"
    of fmReadWrite: cstring"w+b"
    of fmReadWriteExisting: cstring"r+b"
    of fmAppend: cstring"ab"

  f = fopen(filename.toCString, m)
  if f != nil:
    result = true
    if bufSize >= 0:
      discard c_setvbuf(f, nil, IOFBF, cast[uint](bufSize))
  else:
    result = false

template echo*() {.varargs.} =
  for x in unpack():
    write stdout, x
  write stdout, '\n'

proc writeLine*(f: var File; s: string) =
  write f, s
  write f, '\n'

const bufsize = 80

proc fgets(str: out array[bufsize, char]; n: int32; f: File): cstring {.
  importc: "fgets", header: "<stdio.h>".}

proc addReadLine*(f: File; s: var string): bool =
  result = false
  var buf: array[bufsize, char]
  while fgets(buf, bufsize.int32, f) != nil:
    result = true
    var done = false
    for i in 0 ..< bufsize:
      if buf[i] == '\n':
        done = true
        break
      s.add buf[i]
    if done: break

proc readLine*(f: File; s: var string): bool =
  s.shrink 0
  addReadLine f, s

proc exit(value: int32) {.importc: "exit", header: "<stdlib.h>".}
proc quit*(value: int) = exit(value.int32)

template assert*(cond: untyped; msg = "") =
  if not cond:
    echo "[Assertion Failure] ", msg
    quit 1

proc quit*(msg: string) =
  echo msg
  quit 1

proc tryWriteFile*(file, content: string): bool =
  var f: File
  if open(f, file, fmWrite):
    f.write content
    result = not failed(f)
    if fclose(f) != 0'i32:
      result = false
  else:
    result = false
