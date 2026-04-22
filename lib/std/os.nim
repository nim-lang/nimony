import strutils
import posix/posix
import cmdline
import envvars
import oserrors
import private/[ospaths2, osappdirs, oscommons]

export cmdline
export ospaths2
export osappdirs
export oscommons

proc c_system(cmd: cstring): cint {.
    importc: "system", header: "<stdlib.h>".}

when defined(posix):
  proc expandFilename*(filename: string): string {.raises.} =
    ## Returns the full (`absolute`) path of an existing file `filename`,
    ## resolving symlinks. Raises `OSError` if `filename` does not exist.
    const PATH_MAX = 4096
    result = newString(PATH_MAX)
    var filename = filename
    let r = realpath(filename.toCString, result.toCString)
    if r.isNil:
      raiseOSError(osLastError(), filename)
    else:
      var L = 0
      while L < PATH_MAX and result[L] != '\0':
        inc L
      setLen(result, L)
else:
  proc expandFilename*(filename: string): string {.raises.} =
    result = absolutePath(filename)

when defined(linux) or defined(aix):
  const maxSymlinkLen = 1024

  proc getApplAux(procPath: string): string =
    result = newString(maxSymlinkLen)
    var procPath = procPath
    var len = readlink(procPath.toCString, result.toCString, maxSymlinkLen)
    if len > maxSymlinkLen:
      result = newString(len+1)
      len = readlink(procPath.toCString, result.toCString, len)
    if len < 0:
      len = 0
    setLen(result, len)

when supportedSystem:
  proc getAppFilename*(): string {.tags: [ReadIOEffect], raises: [].} =
    when defined(linux) or defined(aix):
      result = getApplAux("/proc/self/exe")
    else:
      result = paramStr(0)

  proc getAppDir*(): string {.tags: [ReadIOEffect].} =
    result = splitFile(getAppFilename()).dir

func quoteShellWindows*(s: string): string {.noSideEffect.} =
  ## Quote `s`, so it can be safely passed to Windows API.
  ##
  ## Based on Python's `subprocess.list2cmdline`.
  ## See `this link <https://msdn.microsoft.com/en-us/library/17w5ykft.aspx>`_
  ## for more details.
  let needQuote = s.contains({' ', '\t'}) or s.len == 0
  result = ""
  var backslashBuff = ""
  if needQuote:
    result.add("\"")

  for c in s:
    if c == '\\':
      backslashBuff.add(c)
    elif c == '\"':
      for i in 0..<backslashBuff.len*2:
        result.add('\\')
      backslashBuff.setLen(0)
      result.add("\\\"")
    else:
      if backslashBuff.len != 0:
        result.add(backslashBuff)
        backslashBuff.setLen(0)
      result.add(c)

  if backslashBuff.len > 0:
    result.add(backslashBuff)
  if needQuote:
    result.add(backslashBuff)
    result.add("\"")


func quoteShellPosix*(s: string): string {.noSideEffect.} =
  ## Quote ``s``, so it can be safely passed to POSIX shell.
  const safeUnixChars = {'%', '+', '-', '.', '/', '_', ':', '=', '@',
                         '0'..'9', 'A'..'Z', 'a'..'z'}
  if s.len == 0:
    result = "''"
  elif s.allCharsInSet(safeUnixChars):
    result = s
  else:
    result = "'" & s.replace("'", "'\"'\"'") & "'"

when defined(windows) or defined(posix) or defined(nintendoswitch):
  func quoteShell*(s: string): string {.noSideEffect.} =
    ## Quote ``s``, so it can be safely passed to shell.
    ##
    ## When on Windows, it calls `quoteShellWindows func`_.
    ## Otherwise, calls `quoteShellPosix func`_.
    when defined(windows):
      result = quoteShellWindows(s)
    else:
      result = quoteShellPosix(s)

  func quoteShellCommand*(args: openArray[string]): string =
    ## Concatenates and quotes shell arguments `args`.
    runnableExamples:
      when defined(posix):
        assert quoteShellCommand(["aaa", "", "c d"]) == "aaa '' 'c d'"
      when defined(windows):
        assert quoteShellCommand(["aaa", "", "c d"]) == "aaa \"\" \"c d\""

    # can't use `map` pending https://github.com/nim-lang/Nim/issues/8303
    result = ""
    for i in 0..<args.len:
      if i > 0: result.add " "
      result.add quoteShell(args[i])

when defined(posix):
  proc getLastModificationTime*(file: string): int64 {.raises.} =
    ## Returns the file's last modification time as seconds since the epoch.
    ## Raises `OSError` if the file does not exist or cannot be stat'ed.
    var s = default(Stat)
    var filename = file
    if stat(filename.toCString, s) < 0:
      raiseOSError(osLastError(), file)
    result = s.st_mtime
elif defined(windows):
  import windows/winlean

  const
    # Number of 100-nanosecond intervals between 1601-01-01 and 1970-01-01.
    winEpochDiff: int64 = 116444736000000000'i64
    hnsecsPerSec: int64 = 10000000'i64

  proc rdFileTime(f: FILETIME): int64 {.inline.} =
    result = int64(cast[uint32](f.dwLowDateTime)) or
             (int64(cast[uint32](f.dwHighDateTime)) shl 32)

  proc winFileTimeToUnix(t: int64): int64 {.inline.} =
    (t - winEpochDiff) div hnsecsPerSec

  proc getLastModificationTime*(file: string): int64 {.raises.} =
    ## Returns the file's last modification time as seconds since the Unix
    ## epoch (1970-01-01 UTC).
    ## Raises `OSError` if the file does not exist or cannot be queried.
    var f {.noinit.}: WIN32_FIND_DATA
    let h = findFirstFile(file, f)
    if h == INVALID_HANDLE_VALUE:
      raiseOSError(osLastError(), file)
    result = winFileTimeToUnix(rdFileTime(f.ftLastWriteTime))
    discard findClose(h)

proc exitStatusLikeShell*(status: cint): cint =
  ## Converts exit code from `c_system` into a shell exit code.
  when defined(posix):
    if WIFSIGNALED(status):
      # like the shell!
      128 + WTERMSIG(status)
    else:
      WEXITSTATUS(status)
  else:
    status

proc execShellCmd*(command: string): int {.tags: [ExecIOEffect].} =
  ## Executes a `shell command`:idx:.
  ##
  ## Command has the form 'program args' where args are the command
  ## line arguments given to program. The proc returns the error code
  ## of the shell when it has finished (zero if there is no error).
  ## The proc does not return until the process has finished.
  ##
  ## To execute a program without having a shell involved, use `osproc.execProcess proc
  ## <osproc.html#execProcess,string,string,openArray[string],StringTableRef,set[ProcessOption]>`_.
  ##
  ## **Examples:**
  ##   ```Nim
  ##   discard execShellCmd("ls -la")
  ##   ```
  var command = command
  let cc = command.toCString()
  if cc.isNil:
    result = -202 # OOM
  else:
    result = exitStatusLikeShell(c_system(cc))
