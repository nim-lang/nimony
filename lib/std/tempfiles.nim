#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module creates temporary files and directories.
##
## The random part of a name comes from the operating system's CSPRNG
## (`std/crypto/sysrand`); only if that cannot be read does it fall back to a
## `std/random` generator seeded from the monotonic clock. Either way a name
## is never trusted to be unique: the file or directory is created
## exclusively (`O_CREAT or O_EXCL` / `CREATE_NEW`, `mkdir`) and a fresh name
## is drawn when it already exists.
##
## Unlike Nim's `std/tempfiles`, which takes and returns `string`, the
## directory and the returned paths are `Path`, matching `std/files` and
## `std/dirs` (so `removeDir(createTempDir(...))` just works).
##
## **See also:**
## * `appdirs module <appdirs.html>`_ for `getTempDir`
## * `files module <files.html>`_ and `dirs module <dirs.html>`_ for cleanup

import std/paths  # separate import for better symbol resolution
import std/[syncio, appdirs, random, monotimes]
import std/crypto/sysrand

export paths.Path, paths.path, paths.`/`, paths.`$`

when defined(windows):
  import windows/winlean
  import std/dirs
  from widestrs import newWideCString, toWideCString
  import errorcodes / errorcodes_windows
else:
  import posix/posix
  import errorcodes / errorcodes_posix

  when defined(osx):
    proc createOpen(path: cstring; flags: cint): cint {.varargs, importc: "open", sideEffect.}
      ## `posix.open` with a variadic C prototype, like libc's. `posix.open` is
      ## fixed-arity, and on Apple arm64 a variadic argument goes on the stack
      ## rather than in a register, so libc would read a garbage `mode`.
  else:
    template createOpen(path: cstring; flags: cint; mode: Mode): cint = open(path, flags, mode)

const
  maxRetry = 10000
  letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  nimTempPathLength {.intdefine.} = 8

when defined(windows):
  const ERROR_FILE_EXISTS = 80'i32  ## not mapped to `NameExists` by errorcodes

  when not defined(nimNativeIo):
    proc c_open_osfhandle(osh: Handle; flags: cint): cint {.
      importc: "_open_osfhandle", header: "<io.h>".}
    proc c_fdopen(fd: cint; mode: cstring): File {.
      importc: "_fdopen", header: "<stdio.h>".}
    proc c_close(fd: cint): cint {.importc: "_close", header: "<io.h>".}
    const O_RDWR_WIN = cint(2)  ## `_O_RDWR`
elif not defined(nimNativeIo):
  # Declared to byte-match libc's `FILE *fdopen(int, const char *)`, see
  # `std/osproc`: under useLibcIo `<stdio.h>` is in scope in this TU.
  type ConstChar {.importc: "const char", nodecl.} = int8
  proc c_fdopen(fd: cint; mode: nil ptr ConstChar): File {.importc: "fdopen".}

var
  fallbackRand {.threadvar.}: Rand
  fallbackInit {.threadvar.}: bool

proc randomPathName(length: int): string =
  ## `length` characters from `letters`, uniformly. Bytes at or above 248 (the
  ## largest multiple of 62 that fits a byte) are rejected so that no letter
  ## is more likely than another.
  result = newString(length)
  var buf = default(array[32, byte])
  var used = buf.len
  var i = 0
  while i < length:
    if used == buf.len:
      if not urandom(buf):
        if not fallbackInit:
          fallbackInit = true
          var local = 0
          fallbackRand = initRand(getMonoTime().ticks xor
                                  cast[int64](cast[uint](addr local)))
        for j in 0 ..< buf.len:
          buf[j] = byte(fallbackRand.next() and 0xFF'u64)
      used = 0
    let b = int(buf[used])
    inc used
    if b < 248:
      result[i] = letters[b mod letters.len]
      inc i

proc getTempDirImpl(dir: Path): Path {.inline.} =
  result = if ($dir).len == 0: getTempDir() else: dir

proc genTempPath*(prefix, suffix: string; dir = path("")): Path =
  ## Generates a path name in `dir`.
  ##
  ## The path begins with `prefix` and ends with `suffix`; between them are
  ## `nimTempPathLength` (default 8) random letters and digits.
  ##
  ## .. note:: `dir` must exist (empty `dir` will resolve to `getTempDir <appdirs.html#getTempDir>`_).
  result = getTempDirImpl(dir) / path(prefix & randomPathName(nimTempPathLength) & suffix)

proc safeOpen(filename: Path; f: var File): ErrorCode =
  ## Creates and opens `filename` exclusively for reading and writing.
  ## `NameExists` if it already exists.
  var name = $filename
  when defined(windows):
    let h = createFileW(newWideCString(name).toWideCString,
                        GENERIC_READ or GENERIC_WRITE,
                        FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                        nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, Handle 0)
    if h == INVALID_HANDLE_VALUE:
      let err = getLastError()
      return if err == ERROR_FILE_EXISTS: NameExists else: windowsToErrorCode(err)
    when defined(nimNativeIo):
      discard open(f, h, fmReadWrite)
      result = Success
    else:
      let fd = c_open_osfhandle(h, O_RDWR_WIN)
      if fd == -1'i32:
        result = windowsToErrorCode(getLastError())
        discard closeHandle(h)
        return
      f = c_fdopen(fd, cstring"w+b")
      if f == nil:
        result = Failure
        discard c_close(fd)
      else:
        result = Success
  else:
    let r = pcall(createOpen(name.toCString, O_RDWR or O_CREAT or O_EXCL or O_CLOEXEC,
                             Mode(0o600)))
    if r < 0: return posixToErrorCode(cint(-r))
    let fd = cint(r)
    when defined(nimNativeIo):
      discard open(f, fd, fmReadWrite)
      result = Success
    else:
      var mode = "w+b"
      f = c_fdopen(fd, cast[ptr ConstChar](mode.toCString))
      if f == nil:
        result = Failure
        discard close(fd)
      else:
        result = Success

proc createTempFile*(prefix, suffix: string;
                     dir = path("")): tuple[cfile: File, path: Path] {.raises.} =
  ## Creates a new temporary file in the directory `dir`.
  ##
  ## This generates a path name using `genTempPath(prefix, suffix, dir)` and
  ## returns a file handle to an open file and the path of that file, possibly
  ## after retrying to ensure it doesn't already exist. On POSIX the file is
  ## created with mode `0o600`.
  ##
  ## Raises the `ErrorCode` of the failure, `NameExists` when every name
  ## tried was taken.
  ##
  ## .. note:: It is the caller's responsibility to close `result.cfile` and
  ##    remove `result.path` when no longer needed.
  ## .. note:: `dir` must exist (empty `dir` will resolve to `getTempDir <appdirs.html#getTempDir>`_).
  result = (cfile: default(File), path: path(""))
  let dir = getTempDirImpl(dir)
  for i in 0 ..< maxRetry:
    let p = genTempPath(prefix, suffix, dir)
    var f = default(File)
    let res = safeOpen(p, f)
    if res == Success: return (cfile: f, path: p)
    if res != NameExists: raise res
  raise NameExists

proc createTempDir*(prefix, suffix: string; dir = path("")): Path {.raises.} =
  ## Creates a new temporary directory in the directory `dir`.
  ##
  ## This generates a dir name using `genTempPath(prefix, suffix, dir)`,
  ## creates the directory and returns it, possibly after retrying to ensure
  ## it doesn't already exist. On POSIX the directory is created with mode
  ## `0o700`.
  ##
  ## Raises the `ErrorCode` of the failure, `NameExists` when every name
  ## tried was taken.
  ##
  ## .. note:: It is the caller's responsibility to remove the directory when no longer needed.
  ## .. note:: `dir` must exist (empty `dir` will resolve to `getTempDir <appdirs.html#getTempDir>`_).
  let dir = getTempDirImpl(dir)
  for i in 0 ..< maxRetry:
    result = genTempPath(prefix, suffix, dir)
    when defined(windows):
      let res = tryCreateFinalDir(result)
    else:
      var s = $result
      let r = pcall(mkdir(s.toCString, Mode(0o700)))
      let res = if r == 0: Success else: posixToErrorCode(cint(-r))
    if res == Success: return
    if res != NameExists: raise res
  raise NameExists
