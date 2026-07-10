{.feature: "lenientnils".}

import std/formatfloat

type
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
  
  FilePermission* = enum   ## File access permission, modelled after UNIX.
    fpUserExec,            ## execute access for the file owner
    fpUserWrite,           ## write access for the file owner
    fpUserRead,            ## read access for the file owner
    fpGroupExec,           ## execute access for the group
    fpGroupWrite,          ## write access for the group
    fpGroupRead,           ## read access for the group
    fpOthersExec,          ## execute access for others
    fpOthersWrite,         ## write access for others
    fpOthersRead           ## read access for others

when defined(nimNativeIo):
  # Freestanding IO: a `File` is a small heap object holding the raw OS file
  # handle plus its own read/write buffers (`seq[char]`, so the allocator
  # manages the memory for us).
  #
  # On POSIX all IO goes through the `read`/`write`/`open`/`close`/`lseek`
  # syscalls, which arkham lowers to bare `syscall` instructions — so no libc
  # `FILE*`/stdio is linked and the result is a self-contained static binary.
  #
  # On Windows there is no stable syscall ABI, so the same buffering sits on top
  # of the kernel32 primitives (`ReadFile`/`WriteFile`/`CreateFileW`/
  # `CloseHandle`/`SetFilePointer`) instead. `<windows.h>`/`kernel32` is the OS
  # API, not the C runtime, so the build stays free of msvcrt's stdio — the
  # direct counterpart of the Linux syscall path.
  #
  # Float formatting now goes through the native dtoa in `system` (`addFloat`),
  # so it no longer needs libc either.
  when defined(windows):
    import std/windows/winlean
    from std/widestrs import newWideCString, toWideCString
    type OsFileHandle = Handle  ## Win32 `HANDLE` (pointer-sized).
  else:
    type OsFileHandle = cint    ## POSIX file descriptor.
  type
    FileFlag = enum
      ffReadable, ffWritable, ffEof, ffError
      ffUnbuf     ## flush after every write (stderr)
    FileObj = object
      fd: OsFileHandle
      flags: set[FileFlag]
      wbuf: seq[char]      ## write buffer (grows on demand; empty for read-only streams)
      rbuf: seq[char]      ## read buffer
      rpos: int            ## consume cursor into `rbuf`
    File* = ref FileObj    ## The type representing a file handle.
else:
  type
    CFile {.importc: "FILE", header: "<stdio.h>".} = object
    File* = ptr CFile ## The type representing a file handle.

when defined(nimNativeIo):
  const NativeBufSize = 8192   ## flush/refill granularity

  when defined(windows):
    # --- kernel32-backed raw primitives, normalized to the POSIX convention
    #     (bytes transferred; `0` read == EOF; negative == error) so the shared
    #     buffering below is identical on both platforms. ---------------------
    const ERROR_BROKEN_PIPE = 109'i32   ## writer end of a pipe was closed
    const FILE_APPEND_DATA = 0x00000004'u32

    proc sysWrite(fd: OsFileHandle; buf: pointer; n: uint): int =
      var written: int32 = 0
      if isSuccess(writeFile(fd, buf, int32(n), addr written, nil)):
        result = int(written)
      else:
        result = -1

    proc sysRead(fd: OsFileHandle; buf: pointer; n: uint): int =
      var got: int32 = 0
      if isSuccess(readFile(fd, buf, int32(n), addr got, nil)):
        result = int(got)               # 0 => end of file
      elif getLastError() == ERROR_BROKEN_PIPE:
        result = 0                       # writer closed the pipe: treat as EOF
      else:
        result = -1

    proc sysClose(fd: OsFileHandle): cint =
      if isSuccess(closeHandle(fd)): 0'i32 else: -1'i32

    proc sysLseek(fd: OsFileHandle; offset: int64; whence: cint): int64 =
      # `whence` 0/1/2 already lines up with FILE_BEGIN/FILE_CURRENT/FILE_END.
      # SetFilePointer treats the hi:lo pair as one signed 64-bit distance, so
      # splitting the two's-complement bits is correct for negative offsets too.
      var hi = cast[LONG](uint32(uint64(offset) shr 32))
      let lo = setFilePointer(fd, cast[LONG](uint32(uint64(offset) and 0xFFFFFFFF'u64)),
                              addr hi, DWORD(whence))
      if lo == INVALID_SET_FILE_POINTER and getLastError() != NO_ERROR:
        result = -1'i64
      else:
        result = (int64(uint32(hi)) shl 32) or int64(uint32(lo))

    proc newFile(fd: OsFileHandle; flags: set[FileFlag]): File =
      File(fd: fd, flags: flags)

    let
      stdin* = newFile(getStdHandle(STD_INPUT_HANDLE), {ffReadable})
        ## Standard input file handle.
      stdout* = newFile(getStdHandle(STD_OUTPUT_HANDLE), {ffWritable})
        ## Standard output file handle. Fully buffered; flushed on exit via the
        ## `system/exits` hook (and on `quit`/`abort`).
      stderr* = newFile(getStdHandle(STD_ERROR_HANDLE), {ffWritable, ffUnbuf})
        ## Standard error file handle. Unbuffered.
  else:
    # --- raw syscall wrappers (arkham lowers these to `syscall` instructions) -
    proc sysWrite(fd: OsFileHandle; buf: pointer; n: uint): int {.importc: "write".}
    proc sysRead(fd: OsFileHandle; buf: pointer; n: uint): int {.importc: "read".}
    proc sysOpen(path: cstring; flags, mode: cint): cint {.importc: "open".}
    proc sysClose(fd: OsFileHandle): cint {.importc: "close".}
    proc sysLseek(fd: OsFileHandle; offset: int64; whence: cint): int64 {.importc: "lseek".}

    when defined(macosx) or defined(macos) or defined(freebsd) or
         defined(openbsd) or defined(netbsd) or defined(dragonfly):
      const
        # BSD/Darwin open(2) flags (differ from Linux; O_RDONLY/WRONLY/RDWR match).
        O_RDONLY = 0x0000'i32
        O_WRONLY = 0x0001'i32
        O_RDWR   = 0x0002'i32
        O_CREAT  = 0x0200'i32
        O_TRUNC  = 0x0400'i32
        O_APPEND = 0x0008'i32
    else:
      const
        # Linux open(2) flags (stable across x86_64/arm64).
        O_RDONLY = 0'i32
        O_WRONLY = 1'i32
        O_RDWR   = 2'i32
        O_CREAT  = 0o100'i32
        O_TRUNC  = 0o1000'i32
        O_APPEND = 0o2000'i32

    proc newFile(fd: OsFileHandle; flags: set[FileFlag]): File =
      File(fd: fd, flags: flags)

    proc getFileHandle*(f: File): cint {.inline.} =
      ## The underlying OS file descriptor (libc-free posix build). Used e.g. by
      ## `std/terminal`'s `isatty`, which has no `fileno`/FILE* to go through.
      cint(f.fd)

    let
      stdin* = newFile(0, {ffReadable})
        ## Standard input file handle (fd 0).
      stdout* = newFile(1, {ffWritable})
        ## Standard output file handle (fd 1). Fully buffered; flushed on exit
        ## via the `system/exits` hook (and on `quit`/`abort`).
      stderr* = newFile(2, {ffWritable, ffUnbuf})
        ## Standard error file handle (fd 2). Unbuffered.

  proc rawWriteAll(fd: OsFileHandle; p: pointer; n: int): bool =
    ## Writes exactly `n` bytes, looping over short writes. Returns false on error.
    var off = 0
    while off < n:
      let k = sysWrite(fd, cast[pointer](cast[int](p) + off), uint(n - off))
      if k <= 0: return false
      off += k
    result = true

  proc flushImpl(f: File) =
    if f.wbuf.len > 0:
      if not rawWriteAll(f.fd, addr f.wbuf[0], f.wbuf.len):
        f.flags.incl ffError
      f.wbuf.setLen 0

  proc writeBytes(f: File; p: pointer; n: int) =
    if n <= 0: return
    let oldLen = f.wbuf.len
    f.wbuf.setLen(oldLen + n)
    copyMem(addr f.wbuf[oldLen], p, n)
    if ffUnbuf in f.flags or f.wbuf.len >= NativeBufSize:
      flushImpl f

  proc fillBuf(f: File): bool =
    ## Refills `rbuf` from the fd. Returns false on EOF/error (and records which).
    f.rbuf.setLen NativeBufSize
    let k = sysRead(f.fd, addr f.rbuf[0], uint(NativeBufSize))
    f.rpos = 0
    if k > 0:
      f.rbuf.setLen k
      result = true
    else:
      f.rbuf.setLen 0
      if k < 0: f.flags.incl ffError
      else: f.flags.incl ffEof
      result = false

  proc readByte(f: File): int =
    ## Returns the next byte as 0..255, or -1 at EOF/error.
    if f.rpos >= f.rbuf.len:
      if not fillBuf(f): return -1
    result = int(f.rbuf[f.rpos])
    inc f.rpos

  proc flushStdStreams() {.nimcall.} =
    ## Registered with `system/exits` so every process exit flushes the buffered
    ## standard streams (the C `main` epilogue and `quit`/`abort` all run it).
    flushImpl stdout
    flushImpl stderr

  setExitFlush flushStdStreams

  proc open*(f: var File; fd: OsFileHandle; mode: FileMode = fmRead): bool =
    ## Wraps an already-open OS file handle in a buffered `File` — the native
    ## replacement for libc `fdopen`. Used by `osproc` to turn a pipe end into a
    ## stream. Returns false for an invalid handle.
    when defined(windows):
      if fd == INVALID_HANDLE_VALUE or fd.isNil: return false
    else:
      if fd < 0'i32: return false
    var fileFlags: set[FileFlag]
    case mode
    of fmRead: fileFlags = {ffReadable}
    of fmWrite, fmAppend: fileFlags = {ffWritable}
    of fmReadWrite, fmReadWriteExisting: fileFlags = {ffReadable, ffWritable}
    f = newFile(fd, fileFlags)
    result = true
else:
  when defined(macos) or defined(macosx):
    var
      stdin* {.importc: "__stdinp", header: "<stdio.h>".}: File
        ## Standard input file handle.
      stdout* {.importc: "__stdoutp", header: "<stdio.h>".}: File
        ## Standard output file handle.
      stderr* {.importc: "__stderrp", header: "<stdio.h>".}: File
        ## Standard error file handle.
  else:
    var
      stdin* {.importc: "stdin", header: "<stdio.h>".}: File
        ## Standard input file handle.
      stdout* {.importc: "stdout", header: "<stdio.h>".}: File
        ## Standard output file handle.
      stderr* {.importc: "stderr", header: "<stdio.h>".}: File
        ## Standard error file handle.

  proc c_fputc(c: int32; f: File): int32 {.
    importc: "fputc", header: "<stdio.h>".}
  proc c_fwrite(buf: pointer; size, n: uint; f: File): uint {.
    importc: "fwrite", header: "<stdio.h>".}
  proc c_fread(buf: pointer; size, n: uint; f: File): uint {.
    importc: "fread", header: "<stdio.h>".}
  proc c_fileno(f: File): cint {.importc: "fileno", header: "<stdio.h>".}

  proc getFileHandle*(f: File): cint = c_fileno(f)

  proc fprintf(f: File; fmt: cstring) {.varargs, importc: "fprintf", header: "<stdio.h>".}

proc write*(f: File; s: string) =
  when defined(nimNativeIo):
    writeBytes(f, readRawData(s), s.len)
  else:
    discard c_fwrite(readRawData(s), 1'u, s.len.uint, f)

proc write*(f: File; b: bool) =
  if b: write f, "true"
  else: write f, "false"

proc write*(f: File; x: int64) =
  when defined(nimNativeIo):
    write f, $x
  else:
    fprintf(f, cstring"%lld", x)

proc write*(f: File; x: int32) =
  write f, int64 x

proc write*(f: File; x: uint64) =
  when defined(nimNativeIo):
    write f, $x
  else:
    fprintf(f, cstring"%llu", x)

proc write*(f: File; x: uint32) =
  write f, uint64 x

proc write*[T: enum](f: File; x: T) =
  write f, $x

proc write*(f: File; c: char) =
  when defined(nimNativeIo):
    var ch = c
    writeBytes(f, addr ch, 1)
  else:
    discard c_fputc(int32(c), f)

proc write*(f: File; x: float) =
  ## Writes data to a file (overloaded for different types).
  # `addFloat` (the bundled Schubfach/Dragonbox dtoa in `system`) renders the
  # shortest round-tripping decimal, so float output no longer touches libc
  # `snprintf` and matches `$x`.
  var s = ""
  s.addFloat x
  write f, s

when defined(nimNativeIo):
  proc close*(f: File) =
    ## Closes a file handle.
    if f != nil:
      flushImpl f
      discard sysClose(f.fd)

  proc fclose(f: File): int32 =
    result = 0'i32
    if f != nil:
      flushImpl f
      result = sysClose(f.fd)
else:
  proc fclose(f: File): int32 {.importc: "fclose", header: "<stdio.h>".}

  proc close*(f: File) =
    ## Closes a file handle.
    discard fclose(f)

  proc fopen(filename, mode: cstring): File {.importc: "fopen", header: "<stdio.h>".}

proc writeBuffer*(f: File; buffer: pointer; size: int): int =
  ## Writes raw bytes to a file.
  when defined(nimNativeIo):
    writeBytes(f, buffer, size)
    result = size
  else:
    result = cast[int](c_fwrite(buffer, 1'u, cast[uint](size), f))

proc readBuffer*(f: File; buffer: pointer; size: int): int =
  ## Reads raw bytes from a file.
  when defined(nimNativeIo):
    var off = 0
    let dest = cast[ptr UncheckedArray[char]](buffer)
    while off < size:
      if f.rpos >= f.rbuf.len:
        if not fillBuf(f): break
      let take = min(f.rbuf.len - f.rpos, size - off)
      copyMem(addr dest[off], addr f.rbuf[f.rpos], take)
      f.rpos += take
      off += take
    result = off
  else:
    result = cast[int](c_fread(buffer, 1'u, cast[uint](size), f))

when defined(nimNativeIo):
  proc failed*(f: File): bool {.inline.} =
    ## Checks if a file operation has failed.
    ffError in f.flags
else:
  proc c_ferror(f: File): int32 {.
    importc: "ferror", header: "<stdio.h>".}

  proc failed*(f: File): bool {.inline.} =
    ## Checks if a file operation has failed.
    c_ferror(f) != 0

  proc c_setvbuf(f: File; buffer: pointer; mode: int32; size: uint): int32 {.
    importc: "setvbuf", header: "<stdio.h>".}

  when defined(macos) or defined(macosx) or defined(linux) or defined(windows):
    const IOFBF = 0'i32
  else:
    var IOFBF {.importc: "_IOFBF", header: "<stdio.h>".}: int32

proc open*(f: out File; filename: string;
           mode: FileMode = fmRead;
           bufSize: int = -1): bool =
  ## Opens a file with specified mode and buffer size (`bufSize`; use `-1` for default buffering).
  ## Returns whether the open succeeded.
  when defined(nimNativeIo) and defined(windows):
    # `bufSize` is advisory only: the buffers are `seq[char]` and grow on demand.
    var desiredAccess, shareMode, disposition: DWORD
    var fileFlags: set[FileFlag]
    shareMode = FILE_SHARE_READ or FILE_SHARE_WRITE
    case mode
    of fmRead:
      desiredAccess = GENERIC_READ; disposition = OPEN_EXISTING
      fileFlags = {ffReadable}
    of fmWrite:
      desiredAccess = GENERIC_WRITE; disposition = CREATE_ALWAYS
      fileFlags = {ffWritable}
    of fmReadWrite:
      desiredAccess = GENERIC_READ or GENERIC_WRITE; disposition = CREATE_ALWAYS
      fileFlags = {ffReadable, ffWritable}
    of fmReadWriteExisting:
      desiredAccess = GENERIC_READ or GENERIC_WRITE; disposition = OPEN_EXISTING
      fileFlags = {ffReadable, ffWritable}
    of fmAppend:
      # FILE_APPEND_DATA makes every write land at end-of-file — the Win32
      # counterpart of POSIX `O_APPEND` (no initial seek needed).
      desiredAccess = FILE_APPEND_DATA; disposition = OPEN_ALWAYS
      fileFlags = {ffWritable}
    var tmpFilename = filename
    let h = createFileW(newWideCString(tmpFilename).toWideCString,
                        desiredAccess, shareMode, nil, disposition,
                        FILE_ATTRIBUTE_NORMAL, Handle 0)
    if h == INVALID_HANDLE_VALUE:
      f = nil
      result = false
    else:
      f = newFile(h, fileFlags)
      result = true
  elif defined(nimNativeIo):
    # `bufSize` is advisory only: the buffers are `seq[char]` and grow on demand.
    var flags: cint
    var fileFlags: set[FileFlag]
    case mode
    of fmRead:
      flags = O_RDONLY; fileFlags = {ffReadable}
    of fmWrite:
      flags = O_WRONLY or O_CREAT or O_TRUNC; fileFlags = {ffWritable}
    of fmReadWrite:
      flags = O_RDWR or O_CREAT or O_TRUNC; fileFlags = {ffReadable, ffWritable}
    of fmReadWriteExisting:
      flags = O_RDWR; fileFlags = {ffReadable, ffWritable}
    of fmAppend:
      flags = O_WRONLY or O_CREAT or O_APPEND; fileFlags = {ffWritable}
    var tmpFilename = filename.terminatingZero()
    let rawFilename = cast[cstring](readRawData(tmpFilename))
    let fd = sysOpen(rawFilename, flags, 0o666'i32)
    if fd >= 0'i32:
      f = newFile(fd, fileFlags)
      result = true
    else:
      f = nil
      result = false
  else:
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
  ## Prints arguments to stdout followed by a newline.
  for x in unpack():
    write stdout, x
  write stdout, '\n'

proc writeLine*(f: File; s: string) =
  ## Writes a string followed by a newline to a file.
  write f, s
  write f, '\n'

when defined(nimNativeIo):
  proc addReadLine*(f: File; s: var string): bool =
    ## Appends a line from a file to a string.
    result = false
    while true:
      let c = readByte(f)
      if c < 0: break
      result = true
      if char(c) == '\n': break
      s.add char(c)
else:
  const bufsize = 80

  proc fgets(str: out array[bufsize, char]; n: int32; f: File): cstring {.
    importc: "fgets", header: "<stdio.h>".}

  proc addReadLine*(f: File; s: var string): bool =
    ## Appends a line from a file to a string.
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
  ## Reads a line from a file into a string.
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

proc quit*(value: int) {.noreturn.} =
  ## Terminates the program with the given exit code, flushing the standard
  ## streams first (via `system/exits`).
  cExit(value)

const
  QuitSuccess* = 0
  QuitFailure* = 1

proc quit*(msg: string) {.noreturn.} =
  echo msg
  quit QuitFailure

proc quit*(msg: string; errorcode: int) {.noreturn.} =
  ## Exits the program with a status code or message.
  ##
  ## Use `quit(int)` to exit with a code only, `quit(string)` to print a message and exit
  ## with failure, or this overload to print a message and use a specific code.
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
  ## Attempts to write content to a file, returns whether the operation succeeded.
  var f: File
  if open(f, file, fmWrite):
    f.write content
    result = not failed(f)
    if fclose(f) != 0'i32:
      result = false
  else:
    result = false

when defined(nimNativeIo):
  proc flushFile*(f: File) =
    ## Flushes file buffers to the underlying device.
    flushImpl f

  proc endOfFile*(f: File): bool =
    ## Returns true if `f` is at the end.
    if f.rpos < f.rbuf.len: return false
    result = not fillBuf(f)

  proc getFilePos*(f: File): int64 {.raises.} =
    ## Retrieves the current position of the file pointer that is used to
    ## read from the file `f`. The file's first byte has the index zero.
    flushImpl f
    result = sysLseek(f.fd, 0'i64, 1'i32) - int64(f.rbuf.len - f.rpos)
    if result < 0: raise IOError

  proc setFilePos*(f: File; pos: int64; relativeTo: FileSeekPos = fspSet) {.raises.} =
    ## Sets the position of the file pointer that is used for read/write
    ## operations. The file's first byte has the index zero.
    flushImpl f
    var p = pos
    if relativeTo == fspCur:
      # `lseek` sits at the post-read position; account for still-buffered bytes.
      p -= int64(f.rbuf.len - f.rpos)
    f.rbuf.setLen 0
    f.rpos = 0
    if sysLseek(f.fd, p, int32(relativeTo)) < 0'i64:
      raise IOError
else:
  proc flushFile*(f: File) {.importc: "fflush", header: "<stdio.h>".}
    ## Flushes file buffers to the underlying device.

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

proc slurp*(path: string): string =
  ## Reads a file into a string. For compatibility with Nim 2.
  try:
    result = readFile(path)
  except:
    result = ""
