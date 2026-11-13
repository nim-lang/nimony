{.feature: "lenientnils".}
{.feature: "staticContracts".}

import std/formatfloat

import std/commonio
export commonio

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
    rpos: Natural        ## consume cursor into `rbuf`
  File* = ref FileObj    ## The type representing a file handle.

const NativeBufSize = 8192   ## flush/refill granularity

when defined(windows):
  # --- kernel32-backed raw primitives, normalized to the POSIX convention
  #     (bytes transferred; `0` read == EOF; negative == error) so the shared
  #     buffering below is identical on both platforms. ---------------------
  const ERROR_BROKEN_PIPE = 109'i32   ## writer end of a pipe was closed

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

  template interrupted(k: int): bool = false
    ## kernel32 IO has no EINTR.

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

  proc getFileHandle*(f: File): Handle {.inline.} =
    ## The underlying Win32 handle (libc-free build). Used e.g. by
    ## `std/terminal`'s `isatty`, which has no `_fileno`/`FILE*` to go through.
    f.fd

  let
    stdin* = newFile(getStdHandle(STD_INPUT_HANDLE), {ffReadable})
      ## Standard input file handle.
    stdout* = newFile(getStdHandle(STD_OUTPUT_HANDLE), {ffWritable})
      ## Standard output file handle. Fully buffered; flushed on exit via the
      ## `system/exits` hook (and on `quit`/`abort`).
    stderr* = newFile(getStdHandle(STD_ERROR_HANDLE), {ffWritable, ffUnbuf})
      ## Standard error file handle. Unbuffered.
elif defined(embedded):
  # --- bare metal: the console is the debug agent -------------------------
  # Every one of these is the honest answer for a target with no filesystem
  # and no OS, rather than a stub: semihosting has ONE console, so `fd` names
  # nothing, and a program that opens a file on a part with no storage should
  # be told so at the call rather than handed a handle that does nothing.
  proc sysWrite(fd: OsFileHandle; buf: pointer; n: uint): int =
    semihostWrite(buf, int(n))
  proc sysRead(fd: OsFileHandle; buf: pointer; n: uint): int = -1
  proc sysOpen(path: cstring; flags: cint): cint = -1
  proc sysClose(fd: OsFileHandle): cint = 0
  proc sysLseek(fd: OsFileHandle; offset: int64; whence: cint): int64 = -1
  template interrupted(k: int): bool = false
else:
  # --- raw syscall wrappers (arkham lowers these to `syscall` instructions) -
  proc sysWrite(fd: OsFileHandle; buf: pointer; n: uint): int {.importc: "write".}
  proc sysRead(fd: OsFileHandle; buf: pointer; n: uint): int {.importc: "read".}
  when defined(linux) and defined(arm64) and defined(nimNoLibc):
    # AArch64's asm-generic syscall table has no `open` — only `openat` (see
    # `linuxA64Raw` in std/posix, whose `nimNoLibc` gate this mirrors). Same
    # shuffle glibc's own `open` does; kept local so syncio stays independent
    # of std/posix.
    proc sysOpenat(dirfd: cint; path: cstring; flags: cint; mode: cint): cint {.
      importc: "openat".}
    proc sysOpen(path: cstring; flags: cint; mode: cint = 0o666'i32): cint {.inline.} =
      const AT_FDCWD = cint(-100)
      sysOpenat(AT_FDCWD, path, flags, mode)
  else:
    proc sysOpen(path: cstring; flags: cint): cint {.varargs, importc: "open".}
  proc sysClose(fd: OsFileHandle): cint {.importc: "close".}
  proc sysLseek(fd: OsFileHandle; offset: int64; whence: cint): int64 {.importc: "lseek".}

  # `interrupted(k)`: the `read`/`write` that returned `k` was cut short by a
  # signal handler installed without `SA_RESTART` (EINTR). That is no error and
  # no end of file -- the call is simply retried (nim-lang/nimony#2912). A raw
  # syscall reports it as `-EINTR`; with libc linked, `read`/`write` return -1
  # and set libc's errno. Same split as `rawSyscalls` in std/posix.
  const EINTR = 4'i32  ## the same on Linux, Darwin, FreeBSD and illumos
  when defined(nimNoLibc) and (defined(linux) or defined(freebsd)):
    template interrupted(k: int): bool = k == -EINTR
  elif defined(linux) or defined(osx) or defined(freebsd) or defined(illumos):
    when defined(android):
      proc errnoLocation(): ptr cint {.importc: "__errno", sideEffect.}
    elif defined(linux):
      proc errnoLocation(): ptr cint {.importc: "__errno_location", sideEffect.}
    elif defined(illumos):
      proc errnoLocation(): ptr cint {.importc: "___errno", sideEffect.}
    else:
      proc errnoLocation(): ptr cint {.importc: "__error", sideEffect.}
    template interrupted(k: int): bool = k == -1 and errnoLocation()[] == EINTR
  else:
    template interrupted(k: int): bool = false

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
    if k <= 0:
      if interrupted(k): continue
      return false
    off += k
  result = true

# The buffer work is done on the object behind the `ref`: a field reached
# through a `ref` can be written by any alias, so only a `var FileObj`
# parameter makes `wbuf`/`rbuf` locations whose length a check can bound.

proc flushImpl(fo: var FileObj) =
  if fo.wbuf.len > 0:
    if not rawWriteAll(fo.fd, addr fo.wbuf[0], fo.wbuf.len):
      fo.flags.incl ffError
    fo.wbuf.setLen 0

proc flushImpl(f: File) {.inline.} = flushImpl(f[])

proc writeBytes(fo: var FileObj; p: pointer; n: int) =
  if n <= 0: return
  let oldLen = fo.wbuf.len
  fo.wbuf.setLen(oldLen + n)
  if oldLen >= fo.wbuf.len:
    # Out of memory: `setLen` could not grow the buffer, so it kept its old
    # length and contents. `n` is positive, so a `setLen` that worked is
    # strictly longer than `oldLen` -- the length is the signal either way.
    fo.flags.incl ffError
    return
  copyMem(addr fo.wbuf[oldLen], p, n)
  if ffUnbuf in fo.flags or fo.wbuf.len >= NativeBufSize:
    flushImpl fo

proc writeBytes(f: File; p: pointer; n: int) {.inline.} = writeBytes(f[], p, n)

proc fillBuf(fo: var FileObj): bool =
  ## Refills `rbuf` from the fd. Returns false on EOF/error (and records which).
  fo.rbuf.setLen NativeBufSize
  fo.rpos = 0
  if fo.rbuf.len < NativeBufSize:
    # out of memory: `setLen` could not grow the buffer, so it is still short
    fo.flags.incl ffError
    return false
  # taken once: the read is a call that may write `fo` under another name
  let buf = addr fo.rbuf[0]
  var k = sysRead(fo.fd, buf, uint(NativeBufSize))
  while interrupted(k):
    k = sysRead(fo.fd, buf, uint(NativeBufSize))
  if k > 0:
    fo.rbuf.setLen k
    result = true
  else:
    fo.rbuf.setLen 0
    if k < 0: fo.flags.incl ffError
    else: fo.flags.incl ffEof
    result = false

proc fillBuf(f: File): bool {.inline.} = fillBuf(f[])

proc readByte(fo: var FileObj): int =
  ## Returns the next byte as 0..255, or -1 at EOF/error.
  result = -1
  while true:
    if fo.rpos < fo.rbuf.len:
      result = int(fo.rbuf[fo.rpos])
      inc fo.rpos
      break
    if not fillBuf(fo): break

proc readByte(f: File): int {.inline.} = readByte(f[])

proc readInto(fo: var FileObj; dest: ptr UncheckedArray[char]; size: int): int =
  var off: Natural = 0
  while off < size:
    if fo.rpos >= fo.rbuf.len:
      if not fillBuf(fo): break
      continue
    var take: Natural = fo.rbuf.len - fo.rpos
    if take > size - off: take = size - off
    copyMem(addr dest[off], addr fo.rbuf[fo.rpos], take)
    let newPos = fo.rpos + take
    let newOff = off + take
    # `take` is at most what is left of `rbuf` and of `dest`: neither sum
    # wraps
    {.assume: 0 <= newPos and 0 <= newOff.}
    fo.rpos = newPos
    off = newOff
  result = off

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

proc write*(f: File; s: string) =
  writeBytes(f, readRawData(s), s.len)

proc write*(f: File; b: bool) =
  if b: write f, "true"
  else: write f, "false"

proc write*(f: File; x: int64) =
  write f, $x

proc write*(f: File; x: int32) =
  write f, int64 x

proc write*(f: File; x: uint64) =
  write f, $x

proc write*(f: File; x: uint32) =
  write f, uint64 x

proc write*[T: enum](f: File; x: T) =
  write f, $x

proc write*(f: File; c: char) =
  var ch = c
  writeBytes(f, addr ch, 1)

proc write*(f: File; x: float) =
  ## Writes data to a file (overloaded for different types).
  # `addFloat` (the bundled Schubfach/Dragonbox dtoa in `system`) renders the
  # shortest round-tripping decimal, so float output no longer touches libc
  # `snprintf` and matches `$x`.
  var s = ""
  s.addFloat x
  write f, s

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

proc writeBuffer*(f: File; buffer: pointer; size: int): int =
  ## Writes raw bytes to a file.
  writeBytes(f, buffer, size)
  result = size

proc readBuffer*(f: File; buffer: pointer; size: int): int =
  ## Reads raw bytes from a file.
  result = readInto(f[], cast[ptr UncheckedArray[char]](buffer), size)

proc failed*(f: File): bool {.inline.} =
  ## Checks if a file operation has failed.
  ffError in f.flags

proc nativeFileFlags(mode: FileMode): set[FileFlag] =
  case mode
  of fmRead: {ffReadable}
  of fmWrite, fmAppend: {ffWritable}
  of fmReadWrite, fmReadWriteExisting: {ffReadable, ffWritable}

proc open*(f: out File; filename: string;
           mode: FileMode = fmRead;
           bufSize: int = -1): bool =
  ## Opens a file with specified mode and buffer size (`bufSize`; use `-1` for default buffering).
  ## Returns whether the open succeeded.
  when defined(windows):
    # `bufSize` is advisory only: the buffers are `seq[char]` and grow on demand.
    let (access, disposition) = win32OpenArgs(mode)
    let fileFlags = nativeFileFlags(mode)
    var tmpFilename = filename
    let h = createFileW(newWideCString(tmpFilename).toWideCString,
                        DWORD(access), FILE_SHARE_READ or FILE_SHARE_WRITE,
                        nil, DWORD(disposition),
                        FILE_ATTRIBUTE_NORMAL, Handle 0)
    if h == INVALID_HANDLE_VALUE:
      f = nil
      result = false
    else:
      f = newFile(h, fileFlags)
      result = true
  else:
    # `bufSize` is advisory only: the buffers are `seq[char]` and grow on demand.
    let flags = cint(posixOpenFlags(mode))
    let fileFlags = nativeFileFlags(mode)
    var tmpFilename = filename.terminatingZero()
    let rawFilename = cast[cstring](readRawData(tmpFilename))
    let fd = sysOpen(rawFilename, flags, 0o666'i32)
    if fd >= 0'i32:
      f = newFile(fd, fileFlags)
      result = true
    else:
      f = nil
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

proc addReadLine*(f: File; s: var string): bool =
  ## Appends a line from a file to a string.
  result = false
  while true:
    let c = readByte(f)
    if c < 0: break
    result = true
    if char(c) == '\n': break
    s.add char(c)

proc readLine*(f: File; s: var string): bool =
  ## Reads a line from a file into a string.
  s.shrink 0
  addReadLine f, s

proc readLine*(f: File): string {.raises.} =
  ## Reads a line from a file, without the line ending (`\n` or `\r\n`).
  ## Raises `EndOfStreamError` at the end of the file and `IOError` if the
  ## read fails.
  result = ""
  if not addReadLine(f, result):
    if failed(f): raise IOError
    raise EndOfStreamError
  if result.len > 0 and result[result.len - 1] == '\r':
    result.shrink result.len - 1

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
    let p = beginStore(buffer, ReadBufSize)
    if p == nil: raise OutOfMemError
    let bytesRead = readBuffer(f, p, ReadBufSize)
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

proc writeFile*(filename: string; content: openArray[byte]) {.raises.} =
  ## Opens `filename` for writing, writes the bytes of `content`, and closes
  ## the file. Raises `IOError` if the file cannot be opened or fully written.
  var f: File
  if open(f, filename, fmWrite):
    var written = content.len
    if content.len > 0:
      written = writeBuffer(f, addr content[0], content.len)
    close(f)
    if written != content.len:
      raise IOError
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

proc slurp*(path: string): string {.semantics: "slurp".} =
  ## Reads a file into a string. For compatibility with Nim 2.
  try:
    result = readFile(path)
  except:
    result = ""
