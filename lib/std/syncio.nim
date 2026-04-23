{.feature: "lenientnils".}

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

  FileSeekPos* = enum    ## Position relative to which seek should happen.
                         # The values are ordered so that they match with stdio
                         # SEEK_SET, SEEK_CUR and SEEK_END respectively.
    fspSet               ## Seek to absolute value
    fspCur               ## Seek relative to current position
    fspEnd               ## Seek relative to end

var
  stdin* {.importc: "stdin", header: "<stdio.h>".}: File
  stdout* {.importc: "stdout", header: "<stdio.h>".}: File
  stderr* {.importc: "stderr", header: "<stdio.h>".}: File

proc c_fputc(c: int32; f: File): int32 {.
  importc: "fputc", header: "<stdio.h>".}
proc c_fwrite(buf: pointer; size, n: uint; f: File): uint {.
  importc: "fwrite", header: "<stdio.h>".}
proc c_fread(buf: pointer; size, n: uint; f: File): uint {.
  importc: "fread", header: "<stdio.h>".}

proc fprintf(f: File; fmt: cstring) {.varargs, importc: "fprintf", header: "<stdio.h>".}

proc write*(f: File; s: string) =
  discard c_fwrite(readRawData(s), 1'u, s.len.uint, f)

proc write*(f: File; b: bool) =
  if b: write f, "true"
  else: write f, "false"

proc write*(f: File; x: int64) =
  fprintf(f, cstring"%lld", x)

proc write*(f: File; x: int32) =
  write f, int64 x

proc write*(f: File; x: uint64) =
  fprintf(f, cstring"%llu", x)

proc write*(f: File; x: uint32) =
  write f, uint64 x

proc write*[T: enum](f: File; x: T) =
  write f, $x

proc write*(f: File; c: char) =
  discard c_fputc(int32(c), f)

proc write*(f: File; x: float) =
  fprintf(f, cstring"%g", x)

proc fclose(f: File): int32 {.importc: "fclose", header: "<stdio.h>".}

proc close*(f: File) = discard fclose(f)

proc fopen(filename, mode: cstring): File {.importc: "fopen", header: "<stdio.h>".}

proc writeBuffer*(f: File; buffer: pointer; size: int): int =
  result = cast[int](c_fwrite(buffer, 1'u, cast[uint](size), f))

proc readBuffer*(f: File; buffer: pointer; size: int): int =
  result = cast[int](c_fread(buffer, 1'u, cast[uint](size), f))

proc c_ferror(f: File): int32 {.
  importc: "ferror", header: "<stdio.h>".}

proc failed*(f: File): bool {.inline.} = c_ferror(f) != 0

proc c_setvbuf(f: File; buffer: pointer; mode: int32; size: uint): int32 {.
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

  # XXX: avoid a possible double copy when the string is allocated
  var tmpFilename = filename.terminatingZero()
  let rawFilename = cast[cstring](readRawData(tmpFilename))
  f = fopen(rawFilename, m)
  if f != nil:
    result = true
    if bufSize >= 0:
      discard c_setvbuf(f, nil, IOFBF, cast[uint](bufSize))
  else:
    result = false

proc open*(filename: string,
            mode: FileMode = fmRead, bufSize: int = -1): File =
  ## Opens a file named `filename` with given `mode`.
  ##
  ## Default mode is readonly. Raises an `IOError` if the file
  ## could not be opened.
  result = default(File)
  if not open(result, filename, mode, bufSize):
    # TODO: raise exception when it is supported.
    #raise newException(IOError, "cannot open: " & filename)
    quit "cannot open: " & filename

template echo*() {.varargs.} =
  for x in unpack():
    write stdout, x
  write stdout, '\n'

proc writeLine*(f: File; s: string) =
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

iterator lines*(filename: string): string {.sideEffect.} =
  ## Iterates over every line in `filename`. Silently returns no lines if the
  ## file cannot be opened.
  var f: File
  if open(f, filename, fmRead):
    var line = ""
    while readLine(f, line):
      yield line
    discard fclose(f)

iterator lines*(f: File): string {.sideEffect.} =
  ## Iterates over every remaining line of an already opened file.
  var line = ""
  while readLine(f, line):
    yield line

proc exit(value: int32) {.importc: "exit", header: "<stdlib.h>".}
proc quit*(value: int) {.noreturn.} = exit(value.int32)

const
  QuitSuccess* = 0
  QuitFailure* = 1

proc quit*(msg: string) {.noreturn.} =
  echo msg
  quit QuitFailure

proc quit*(msg: string; errorcode: int) {.noreturn.} =
  echo msg
  quit errorcode

const ReadBufSize = 4000

proc readAll*(f: File): string {.raises.} =
  result = ""
  var buffer = newString(ReadBufSize)
  while true:
    let bytesRead = readBuffer(f, beginStore(buffer, ReadBufSize), ReadBufSize)
    endStore(buffer)
    if bytesRead == ReadBufSize:
      result.add buffer
    else:
      buffer.setLen bytesRead
      result.add buffer
      break
  if failed(f): raise IOError

proc readFile*(filename: string): string {.raises.} =
  ## Opens a file named `filename`, reads its entire contents and closes the file.
  ## Raises `IOError` if the file cannot be opened.
  result = ""
  var f: File
  if open(f, filename):
    try:
      result = readAll(f)
    finally:
      close(f)
  else:
    raise IOError

proc writeFile*(filename, content: string) {.raises.} =
  ## Opens `filename` for writing, writes `content`, and closes the file.
  ## Raises `IOError` if the file cannot be opened.
  var f: File
  if open(f, filename, fmWrite):
    try:
      f.write content
    finally:
      close(f)
  else:
    raise IOError

proc tryWriteFile*(file, content: string): bool =
  var f: File
  if open(f, file, fmWrite):
    f.write content
    result = not failed(f)
    if fclose(f) != 0'i32:
      result = false
  else:
    result = false

proc flushFile*(f: File) {.importc: "fflush", header: "<stdio.h>".}

proc c_fgetc(stream: File): int32 {.
  importc: "fgetc", header: "<stdio.h>".}
proc c_ungetc(c: int32; f: File): int32 {.
  importc: "ungetc", header: "<stdio.h>".}

when defined(windows):
  when not defined(amd64):
    proc c_fseek(f: File; offset: int64; whence: int32): int32 {.
      importc: "fseek", header: "<stdio.h>".}
    proc c_ftell(f: File): int64 {.
      importc: "ftell", header: "<stdio.h>".}
  else:
    proc c_fseek(f: File; offset: int64; whence: int32): int32 {.
      importc: "_fseeki64", header: "<stdio.h>".}
    proc c_ftell(f: File): int64 {.
      importc: "_ftelli64", header: "<stdio.h>".}
else:
  proc c_fseek(f: File; offset: int64; whence: int32): int32 {.
    importc: "fseeko", header: "<stdio.h>".}
  proc c_ftell(f: File): int64 {.
    importc: "ftello", header: "<stdio.h>".}

proc endOfFile*(f: File): bool =
  ## Returns true if `f` is at the end.
  var c = c_fgetc(f)
  discard c_ungetc(c, f)
  result = c < 0'i32

proc getFilePos*(f: File): int64 {.raises.} =
  ## Retrieves the current position of the file pointer that is used to
  ## read from the file `f`. The file's first byte has the index zero.
  result = c_ftell(f)
  if result < 0: raise IOError

proc setFilePos*(f: File; pos: int64; relativeTo: FileSeekPos = fspSet) {.raises.} =
  ## Sets the position of the file pointer that is used for read/write
  ## operations. The file's first byte has the index zero.
  if c_fseek(f, pos, int32(relativeTo)) != 0'i32:
    raise IOError
