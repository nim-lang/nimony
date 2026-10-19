import strutils, syncio
import posix/posix
import cmdline
import envvars
import oserrors
import private/[ospaths2, osappdirs, oscommons, ossymlinks]

export cmdline
export ospaths2
export osappdirs
export oscommons
export ossymlinks

when not defined(posix):
  # POSIX replaces libc's `system()` with the native fork+exec path in
  # `execShellCmd`; only Windows still calls out to `system`.
  proc c_system(cmd: cstring): cint {.
      importc: "system", header: "<stdlib.h>", dynlib: "msvcrt".}

when defined(posix):
  when defined(nimNativeIo):
    const maxSymlinkFollows = 40
      ## Link expansions allowed per call before failing with `ELOOP`; the
      ## `MAXSYMLINKS` value of Linux and of musl's `realpath`.

    proc readLinkTarget(path: var string; info: string): string {.raises.} =
      ## The target of the symlink `path`. `pcall`, not `osLastError`: a
      ## freestanding build has no libc errno to read the failure from.
      var bufLen = 256
      while true:
        result = newString(bufLen)
        let r = pcall(readlink(path.toCString, result.toCString, bufLen))
        if r < 0:
          raiseOSError(OSErrorCode(-r), info)
        if r < bufLen:
          result.setLen(int(r))
          break
        bufLen = bufLen shl 1

    proc nativeRealpath(filename: string): string {.raises.} =
      ## Libc-free `realpath`: walks `filename` one component at a time from
      ## the root (absolute input) or the cwd, `lstat`ing each new component
      ## and splicing in the target of every symlink met, so the result names
      ## the file itself rather than a path to it. `..` pops the resolved
      ## prefix, which is right because that prefix holds no symlinks: as in
      ## `realpath`, `link/..` is the parent of the link's TARGET. Errors are
      ## `realpath`'s: ENOENT for a missing component, ENOTDIR for a
      ## non-directory followed by more path, ELOOP after too many links.
      if filename.len == 0:
        raiseOSError(OSErrorCode(ENOENT), filename)
      # `rest` is what is still to be walked; `pos` the walk's place in it.
      var rest = filename
      var pos = 0
      # getcwd yields the physical path: it needs no resolving of its own.
      result = if filename[0] == '/': "/" else: getCurrentDir()
      var follows = 0
      while pos < rest.len:
        if rest[pos] == '/':
          inc pos
          continue
        var e = pos
        while e < rest.len and rest[e] != '/': inc e
        let comp = substr(rest, pos, e - 1)
        pos = e
        if comp == ".":
          discard
        elif comp == "..":
          let slash = rfind(result, '/')
          result.setLen(max(slash, 1))
        else:
          var candidate = if result == "/": "/" & comp else: result & "/" & comp
          var s = default(Stat)
          let r = pcall(lstat(candidate.toCString, s))
          if r < 0:
            raiseOSError(OSErrorCode(-r), filename)
          if S_ISLNK(s.st_mode):
            inc follows
            if follows > maxSymlinkFollows:
              raiseOSError(OSErrorCode(ELOOP), filename)
            let target = readLinkTarget(candidate, filename)
            if target.len == 0:
              raiseOSError(OSErrorCode(ENOENT), filename)
            # The target replaces the link in the path still to walk; an
            # absolute one restarts the walk from the root.
            rest = target & substr(rest, pos)
            pos = 0
            if target[0] == '/':
              result = "/"
          elif not S_ISDIR(s.st_mode) and pos < rest.len:
            # Even a bare trailing `/` asks for a directory.
            raiseOSError(OSErrorCode(ENOTDIR), filename)
          else:
            result = candidate

  proc expandFilename*(filename: string): string {.raises.} =
    ## Returns the full (`absolute`) path of an existing file `filename`,
    ## resolving symlinks. Raises `OSError` if `filename` does not exist.
    when defined(nimNativeIo):
      # Libc-free: `realpath` is a libc routine, not a syscall.
      result = nativeRealpath(filename)
    else:
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
elif not defined(windows):
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
    ## Returns the file's last modification time as nanoseconds since the
    ## Unix epoch (1970-01-01 UTC). Raises `OSError` if the file does not
    ## exist or cannot be stat'ed.
    var s = default(Stat)
    var filename = file
    if stat(filename.toCString, s) < 0:
      raiseOSError(osLastError(), file)
    result = int64(s.st_mtim.tv_sec) * 1_000_000_000'i64 + int64(s.st_mtim.tv_nsec)
elif defined(windows):
  import windows/winlean
  import widestrs

  const
    # Number of 100-nanosecond intervals between 1601-01-01 and 1970-01-01.
    winEpochDiff: int64 = 116444736000000000'i64

  proc rdFileTime(f: FILETIME): int64 {.inline.} =
    result = int64(cast[uint32](f.dwLowDateTime)) or
             (int64(cast[uint32](f.dwHighDateTime)) shl 32)

  proc rdFileSize(f: WIN32_FIND_DATA): int64 {.inline.} =
    result = int64(cast[uint32](f.nFileSizeLow)) or
             (int64(cast[uint32](f.nFileSizeHigh)) shl 32)

  proc getLastModificationTime*(file: string): int64 {.raises.} =
    ## Returns the file's last modification time as nanoseconds since the
    ## Unix epoch (1970-01-01 UTC). Raises `OSError` if the file does not
    ## exist or cannot be queried.
    var f {.noinit.}: WIN32_FIND_DATA
    let h = findFirstFile(file, f)
    if h == INVALID_HANDLE_VALUE:
      raiseOSError(osLastError(), file)
    # FILETIME is in 100ns intervals since 1601-01-01; rebase to ns since epoch.
    result = (rdFileTime(f.ftLastWriteTime) - winEpochDiff) * 100'i64
    discard findClose(h)

  proc expandFilename*(filename: string): string {.raises.} =
    ## Returns the full (`absolute`) path of an existing file `filename`,
    ## with the case of its last component as stored on disk. Raises
    ## `OSError` if `filename` does not exist.
    ##
    ## Unlike the POSIX version this does not resolve symlinks or junctions
    ## (neither does Nim's): `GetFullPathNameW` is lexical.
    var name = filename
    var wName = newWideCString(name)
    var bufsize = MAX_PATH.int32
    var res = newWideCString(bufsize)
    var filePart: WideCString = nil
    while true:
      let L = getFullPathNameW(wName.toWideCString, bufsize, res.toWideCString, filePart)
      if L == 0'i32:
        raiseOSError(osLastError(), filename)
      elif L > bufsize:
        res = newWideCString(L)
        bufsize = L
      else:
        result = $res
        break
    # GetFullPathNameW does no case correction; FindFirstFileW reports the
    # last component as stored. A wildcard would make it a pattern instead.
    if not result.contains({'*', '?'}):
      var f {.noinit.}: WIN32_FIND_DATA
      let h = findFirstFile(result, f)
      if h != INVALID_HANDLE_VALUE:
        discard findClose(h)
        let stored = $cast[ptr UncheckedArray[WinChar]](addr f.cFileName)
        let (dir, _) = splitPath(result)
        result = dir / stored
    if not fileExists(result) and not dirExists(result):
      raiseOSError(OSErrorCode(2'i32), filename) # ERROR_FILE_NOT_FOUND

proc sleep*(milsecs: int) {.tags: [TimeEffect], sideEffect.} =
  ## Sleeps `milsecs` milliseconds.
  ## A negative `milsecs` causes sleep to return immediately.
  when not supportedSystem:
    {.error: "sleep is not available on a freestanding target".}
  elif defined(windows):
    if milsecs > 0:
      winlean.sleep(DWORD(milsecs))
  else:
    if milsecs > 0:
      var req = default(Timespec)
      req.tv_sec = typeof(req.tv_sec)(milsecs div 1000)
      req.tv_nsec = typeof(req.tv_nsec)((milsecs mod 1000) * 1_000_000)
      var rem = default(Timespec)
      # A signal interrupts nanosleep with EINTR and leaves the time still
      # to sleep in `rem`; resume with it so the full interval elapses.
      while pcall(nanosleep(req, rem)) == clong(-EINTR):
        req = rem

proc setFilePermissions*(filename: string; permissions: set[FilePermission]) {.raises.} =
  ## Sets the file permissions for `filename`. Raises `OSError` on failure.
  ##
  ## On Windows only the ``fpUserWrite`` permission is honoured: without it the
  ## file becomes read-only (Windows has no execute or group/other bits).
  when defined(posix):
    var p = 0'u32
    if fpUserRead in permissions: p = p or 0o400'u32
    if fpUserWrite in permissions: p = p or 0o200'u32
    if fpUserExec in permissions: p = p or 0o100'u32
    if fpGroupRead in permissions: p = p or 0o040'u32
    if fpGroupWrite in permissions: p = p or 0o020'u32
    if fpGroupExec in permissions: p = p or 0o010'u32
    if fpOthersRead in permissions: p = p or 0o004'u32
    if fpOthersWrite in permissions: p = p or 0o002'u32
    if fpOthersExec in permissions: p = p or 0o001'u32
    var filename = filename
    if chmod(filename.toCString, Mode(p)) != 0'i32:
      raiseOSError(osLastError(), filename)
  elif defined(windows):
    var filename = filename
    let w = newWideCString(filename)
    var a = getFileAttributesW(w.toWideCString)
    if a == -1'i32: raiseOSError(osLastError(), filename)
    if fpUserWrite in permissions:
      a = a and not int32(FILE_ATTRIBUTE_READONLY)
    else:
      a = a or int32(FILE_ATTRIBUTE_READONLY)
    if isFail(setFileAttributesW(w.toWideCString, a)):
      raiseOSError(osLastError(), filename)
  else:
    {.error: "no filesystem on a freestanding target".}

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

proc findExe*(exe: string): string {.tags: [ReadDirEffect, ReadEnvEffect].} =
  ## Searches for `exe` in the directories listed in the PATH environment
  ## variable (and, on POSIX, in the current directory if `exe` contains a
  ## path separator). Returns `""` if the exe cannot be found.
  result = ""
  if exe.len == 0: return
  let withExt = addFileExt(exe, ExeExt)
  when defined(posix):
    if '/' in exe:
      if fileExists(withExt): return withExt
  else:
    if fileExists(withExt): return withExt
  let path = getEnv("PATH")
  for candidate in split(path, PathSep):
    if candidate.len == 0: continue
    when defined(windows):
      let dir =
        if candidate[0] == '"' and candidate[^1] == '"':
          substr(candidate, 1, candidate.len-2)
        else:
          candidate
    else:
      let dir = candidate
    let x = addFileExt(dir / exe, ExeExt)
    if fileExists(x): return x
  result = ""

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
  when defined(posix):
    # No libc `system()` binding: launch `/bin/sh -c command` and wait,
    # inheriting the parent's streams. illumos always uses posix_spawn;
    # other platforms use fork/exec. `os` can't import osproc (circular).
    var sh = "/bin/sh"
    var dashc = "-c"
    var cmd = command
    # `nil cstring` elements: slot 3 is the NULL terminator `execve` demands,
    # so the element type has to be one that admits nil.
    let argv = cast[ptr UncheckedArray[nil cstring]](alloc0(4 * sizeof(cstring)))
    argv[0] = sh.toCString
    argv[1] = dashc.toCString
    argv[2] = cmd.toCString
    argv[3] = nil.cstring
    var pid = Pid(-1)
    when defined(illumos):
      let rc = posix_spawn(pid, sh.toCString, nil, nil,
                           cast[CCharArray](argv), cast[CCharArray](posix_environ))
      let launchFailed = rc != 0
    else:
      pid = fork()
      if pid == Pid(0):
        # child: exec the shell; only reached past here if exec failed.
        discard execve(sh.toCString, cast[CCharArray](argv),
                       cast[CCharArray](posix_environ))
        exitnow(127'i32)
      let launchFailed = pid.int < 0
    dealloc(cast[pointer](argv))
    if launchFailed:
      result = -1   # launch failed, like `system()`
    else:
      var status: cint = 0'i32
      if waitpid(pid, status, 0'i32).int < 0:
        result = -1
      else:
        result = exitStatusLikeShell(status)
  else:
    var command = command
    let cc = command.toCString()
    if cc.isNil:
      result = -202 # OOM
    else:
      result = exitStatusLikeShell(c_system(cc))

proc getFileSize*(file: string): int64 {.tags: [ReadIOEffect], raises.} =
  ## Returns the file size of `file` (in bytes).
  when not supportedSystem:
    {.error: "no filesystem on a freestanding target".}
  elif defined(windows):
    var a {.noinit.}: WIN32_FIND_DATA
    let resA = findFirstFile(file, a)
    if resA == INVALID_HANDLE_VALUE: raiseOSError(osLastError(), file)
    try:
      result = rdFileSize(a)
    finally:
      discard findClose(resA)
  else:
    var rawInfo = default(Stat)
    var filename = file
    if stat(filename.toCString, rawInfo) < 0'i32:
      raiseOSError(osLastError(), file)
    result = int64(rawInfo.st_size)
