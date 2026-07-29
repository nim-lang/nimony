#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The `std/osproc` module implements an advanced facility for executing OS
## processes and process communication.
##
## This is the Nimony port. Compared to the upstream Nim module the POSIX
## path uses `fork` + `execve` exclusively (no posix_spawn — its attribute
## types are opaque libc-internal structs, which std/posix no longer binds)
## and the timeout path of `waitForExit` uses a busy-wait with `nanosleep`.
##
## **See also:**
## * `os module <os.html>`_
## * `streams module <streams.html>`_
## * `cpuinfo module <cpuinfo.html>`_

{.feature: "lenientnils".}

import std/[strutils, os, strtabs, streams, cpuinfo, syncio, oserrors,
            envvars, monotimes, times, assertions]

export quoteShell, quoteShellWindows, quoteShellPosix

when defined(windows):
  import std/windows/winlean
  import std/widestrs
  type FileHandle* = Handle
    ## OS-level handle. Win32 HANDLE on Windows (pointer-sized so the upper
    ## 32 bits of a HANDLE survive — storing it as `cint` truncated handles
    ## to garbage and broke subprocess I/O on x64). Pipe and process handles
    ## returned by `CreatePipe` / `CreateProcess` go straight into this field.
else:
  import std/posix/posix
  type FileHandle* = cint
    ## OS-level file descriptor.

type
  ProcessOption* = enum ## Options that can be passed to `startProcess`.
    poEchoCmd,          ## Echo the command before execution.
    poUsePath,          ## Asks system to search for executable using `PATH`.
                        ## On Windows, this is the default.
    poEvalCommand,      ## Pass `command` directly to the shell, without
                        ## quoting. Use it only if `command` comes from a
                        ## trusted source.
    poStdErrToStdOut,   ## Merge stdout and stderr to the stdout stream.
    poParentStreams,    ## Use the parent's streams.
    poInteractive,      ## Optimize the buffer handling for responsiveness
                        ## for UI applications. Currently this only affects
                        ## Windows: named pipes are used so that you can
                        ## peek at the process' output streams.
    poDaemon            ## Windows: the program creates no window.
                        ## Unix: start the program as a daemon. Still a
                        ## work in progress.

  ProcessObj = object of RootObj
    when defined(windows):
      fProcessHandle: Handle
      fThreadHandle: Handle
      inHandle, outHandle, errHandle: FileHandle
      id: Handle
    else:
      inHandle, outHandle, errHandle: FileHandle
      id: Pid
    inStream, outStream, errStream: Stream
    exitStatus: cint
    exitFlag: bool
    options: set[ProcessOption]

  Process* = ref ProcessObj
    ## Represents an operating system process.

# ---------------------------------------------------------------------------
# Public API — non-platform-specific accessors
# ---------------------------------------------------------------------------

proc processID*(p: Process): int {.inline.} =
  ## Returns `p`'s process ID.
  result = int(p.id)

proc inputHandle*(p: Process): FileHandle {.inline.} =
  ## Returns `p`'s input file handle for writing to.
  result = p.inHandle

proc outputHandle*(p: Process): FileHandle {.inline.} =
  ## Returns `p`'s output file handle for reading from.
  result = p.outHandle

proc errorHandle*(p: Process): FileHandle {.inline.} =
  ## Returns `p`'s error file handle for reading from.
  result = p.errHandle

proc countProcessorsAvailable(): int {.inline.} =
  ## Returns the number of processors/cores the machine has.
  result = cpuinfo.countProcessors()

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

template streamAccess(p: typed) {.untyped.} =
  assert poParentStreams notin p.options,
    "API usage error: stream access not allowed when you use poParentStreams"

proc joinArgs(args: openArray[string]; sep: string): string =
  result = ""
  for i in 0 ..< args.len:
    if i > 0: result.add sep
    result.add args[i]

# ---------------------------------------------------------------------------
# POSIX backend
# ---------------------------------------------------------------------------

when not defined(windows):
  type
    cstringArray = nil ptr UncheckedArray[cstring]

  const
    readIdx = 0
    writeIdx = 1

  proc isExitStatus(status: cint): bool =
    WIFEXITED(status) or WIFSIGNALED(status)

  proc allocCStringArray(s: openArray[string]): cstringArray =
    let n = s.len
    result = cast[cstringArray](alloc0((n + 1) * sizeof(cstring)))
    for i in 0 ..< n:
      var item = s[i]
      let l = item.len
      let buf = cast[cstring](alloc(l + 1))
      if l > 0:
        copyMem(buf, item.toCString, l)
      cast[ptr char](cast[uint](buf) + uint(l))[] = '\0'
      result[i] = buf
    result[n] = nil.cstring

  proc deallocCStringArray(a: cstringArray; n: int) =
    if a == nil: return
    for i in 0 ..< n:
      if a[i] != nil: dealloc(cast[pointer](a[i]))
    dealloc(cast[pointer](a))

  proc envToCStringArray(t: StringTableRef): cstringArray =
    var count = 0
    for key, val in pairs(t): inc count
    result = cast[cstringArray](alloc0((count + 1) * sizeof(cstring)))
    var i = 0
    for key, val in pairs(t):
      var kv = key & "=" & val
      let l = kv.len
      let buf = cast[cstring](alloc(l + 1))
      copyMem(buf, kv.toCString, l)
      cast[ptr char](cast[uint](buf) + uint(l))[] = '\0'
      result[i] = buf
      inc i
    result[count] = nil.cstring

  proc envToCStringArray(): cstringArray =
    var count = 0
    for key, val in envPairs(): inc count
    result = cast[cstringArray](alloc0((count + 1) * sizeof(cstring)))
    var i = 0
    for key, val in envPairs():
      var kv = key & "=" & val
      let l = kv.len
      let buf = cast[cstring](alloc(l + 1))
      copyMem(buf, kv.toCString, l)
      cast[ptr char](cast[uint](buf) + uint(l))[] = '\0'
      result[i] = buf
      inc i
    result[count] = nil.cstring

  proc countCStringArray(a: cstringArray): int =
    result = 0
    if a == nil: return
    while a[result] != nil: inc result

  proc resolveExecPaths(command: string; usePath: bool): seq[string] =
    ## Builds the candidate executable paths BEFORE forking. A forked child of
    ## a multithreaded parent may only do async-signal-safe work (another
    ## thread can hold the allocator lock at fork time), so all PATH searching
    ## and string building happens here, in the parent; the child just walks
    ## the prebuilt list calling bare `execve`.
    result = @[]
    var hasSlash = false
    for ch in command.items:
      if ch == '/':
        hasSlash = true
        break
    if hasSlash or not usePath:
      result.add command
      return
    var path = getEnv("PATH")
    if path.len == 0:
      path = "/bin:/usr/bin"
    var start = 0
    var i = 0
    while i <= path.len:
      if i == path.len or path[i] == ':':
        let dir = if i > start: path.substr(start, i-1) else: "."
        result.add dir & "/" & command
        start = i + 1
      inc i

  proc startProcess*(command: string; workingDir: string = "";
                     args: openArray[string] = [];
                     env: nil StringTableRef = nil;
                     options: set[ProcessOption] = {poStdErrToStdOut}):
      Process {.raises, tags: [ExecIOEffect, ReadEnvEffect, RootEffect].} =
    var pStdin {.noinit.}: array[0..1, cint]
    var pStdout {.noinit.}: array[0..1, cint]
    var pStderr {.noinit.}: array[0..1, cint]

    if poParentStreams notin options:
      if pipe(addr pStdin[0]) != 0'i32 or pipe(addr pStdout[0]) != 0'i32 or
         pipe(addr pStderr[0]) != 0'i32:
        raiseOSError(osLastError())

    # Build argv
    var argsRaw: seq[string]
    var syscmd: string
    if poEvalCommand in options:
      const useShPath = "/bin/sh"
      syscmd = useShPath
      argsRaw = @[useShPath, "-c", command]
      assert args.len == 0,
        "`args` has to be empty when using poEvalCommand."
    else:
      syscmd = command
      argsRaw = @[command]
      for a in args.items:
        argsRaw.add a

    let sysArgs = allocCStringArray(argsRaw)
    let sysEnv =
      if env == nil: envToCStringArray()
      else: envToCStringArray(env)
    # PATH resolution happens pre-fork (see resolveExecPaths); poEvalCommand
    # always execs /bin/sh directly.
    let execPaths = resolveExecPaths(syscmd,
        poUsePath in options and poEvalCommand notin options)
    let sysPaths = allocCStringArray(execPaths)

    if poEchoCmd in options:
      echo command, " ", joinArgs(args, " ")

    let argvC = cast[CCharArray](sysArgs)
    let envpC = cast[CCharArray](sysEnv)
    let nPaths = execPaths.len
    var wd = workingDir
    var pid: Pid = fork()
    if pid == Pid(0):
      # --- child: only async-signal-safe calls from here to execve/_exit
      # (`toCString` borrows the string's existing buffer; no allocation) ---
      if poParentStreams notin options:
        discard dup2(pStdin[readIdx], 0'i32)
        discard dup2(pStdout[writeIdx], 1'i32)
        if poStdErrToStdOut in options:
          discard dup2(pStdout[writeIdx], 2'i32)
        else:
          discard dup2(pStderr[writeIdx], 2'i32)
        discard close(pStdin[readIdx]); discard close(pStdin[writeIdx])
        discard close(pStdout[readIdx]); discard close(pStdout[writeIdx])
        discard close(pStderr[readIdx]); discard close(pStderr[writeIdx])
      if poDaemon in options:
        discard setpgid(Pid(0), Pid(0))
      if wd.len > 0 and chdir(wd.toCString) != 0'i32:
        exitnow(127'i32)
      for k in 0 ..< nPaths:
        let cand: cstring = sysPaths[k]
        if cand != nil:
          discard execve(cand, argvC, envpC)  # returns only on failure
      exitnow(127'i32)   # reached only if every exec failed
    # --- parent ---
    deallocCStringArray(sysPaths, execPaths.len)
    deallocCStringArray(sysArgs, argsRaw.len)
    deallocCStringArray(sysEnv, countCStringArray(sysEnv))
    if pid.int < 0:
      raiseOSError(osLastError())

    result = Process(options: options, exitFlag: false, id: pid,
                     inHandle: 0'i32, outHandle: 0'i32, errHandle: 0'i32,
                     inStream: nil, outStream: nil, errStream: nil,
                     exitStatus: 0'i32)

    if poParentStreams in options:
      result.inHandle = 0'i32
      result.outHandle = 1'i32
      if poStdErrToStdOut in options:
        result.errHandle = result.outHandle
      else:
        result.errHandle = 2'i32
    else:
      result.inHandle = pStdin[writeIdx]
      result.outHandle = pStdout[readIdx]
      if poStdErrToStdOut in options:
        result.errHandle = result.outHandle
        discard close(pStderr[readIdx])
      else:
        result.errHandle = pStderr[readIdx]
      discard close(pStderr[writeIdx])
      discard close(pStdin[readIdx])
      discard close(pStdout[writeIdx])

  proc close*(p: Process) {.raises, tags: [WriteIOEffect].} =
    if poParentStreams notin p.options:
      if p.inStream != nil:
        close(p.inStream)
      else:
        discard close(p.inHandle)
      if p.outStream != nil:
        close(p.outStream)
      else:
        discard close(p.outHandle)
      if p.errStream != nil:
        close(p.errStream)
      elif p.errHandle != p.outHandle:
        discard close(p.errHandle)

  proc suspend*(p: Process) {.raises, tags: [].} =
    if kill(p.id, SIGSTOP) != 0'i32:
      raiseOSError(osLastError())

  proc resume*(p: Process) {.raises, tags: [].} =
    if kill(p.id, SIGCONT) != 0'i32:
      raiseOSError(osLastError())

  proc terminate*(p: Process) {.raises, tags: [].} =
    if kill(p.id, SIGTERM) != 0'i32:
      raiseOSError(osLastError())

  proc kill*(p: Process) {.raises, tags: [].} =
    if kill(p.id, SIGKILL) != 0'i32:
      raiseOSError(osLastError())

  proc running*(p: Process): bool {.raises, tags: [].} =
    if p.exitFlag:
      return false
    var status: cint = 1'i32
    let ret = waitpid(p.id, status, WNOHANG)
    if ret == p.id:
      if isExitStatus(status):
        p.exitFlag = true
        p.exitStatus = status
        result = false
      else:
        result = true
    elif ret == 0.Pid:
      result = true
    else:
      result = false
      raiseOSError(osLastError())

  proc waitForExit*(p: Process; timeout: int = -1): int {.raises,
      tags: [TimeEffect].} =
    if p.exitFlag:
      return exitStatusLikeShell(p.exitStatus)

    if timeout < 0:
      var status: cint = 0'i32
      if waitpid(p.id, status, 0'i32).int < 0:
        raiseOSError(osLastError())
      p.exitFlag = true
      p.exitStatus = status
    else:
      let deadlineTicks =
        getMonoTime().ticks + int64(timeout) * 1_000_000'i64
      var delayNs: int64 = 50_000'i64   # 50 μs start
      const maxDelayNs: int64 = 50_000_000'i64  # 50 ms cap
      while true:
        var status: cint = 0'i32
        let pid = waitpid(p.id, status, WNOHANG)
        if pid == p.id:
          p.exitFlag = true
          p.exitStatus = status
          break
        elif pid.int == -1:
          raiseOSError(osLastError())
        else:
          if getMonoTime().ticks >= deadlineTicks:
            if kill(p.id, SIGKILL) < 0'i32:
              raiseOSError(osLastError())
            var waitStatus: cint = 0'i32
            if waitpid(p.id, waitStatus, 0'i32).int < 0:
              raiseOSError(osLastError())
            p.exitFlag = true
            p.exitStatus = waitStatus
            break
          var req: Timespec = default(Timespec)
          var rem: Timespec = default(Timespec)
          req.tv_sec = posix.Time(0)
          req.tv_nsec = clong(delayNs)
          discard nanosleep(req, rem)
          if delayNs < maxDelayNs:
            delayNs = delayNs * 2'i64

    result = exitStatusLikeShell(p.exitStatus)

  proc peekExitCode*(p: Process): int {.raises, tags: [].} =
    var status: cint = 0'i32
    result = -1
    if p.exitFlag:
      return exitStatusLikeShell(p.exitStatus)
    let ret = waitpid(p.id, status, WNOHANG)
    if ret.int > 0:
      if isExitStatus(status):
        p.exitFlag = true
        p.exitStatus = status
        result = exitStatusLikeShell(status)

  proc createStream(handle: var FileHandle; mode: FileMode): FileStream
      {.raises.} =
    # Turn a pipe fd into a buffered File for use with FileStream.
    var f: File = default(File)
    when defined(nimNativeIo):
      # Freestanding: build a native File around the raw fd (no libc fdopen).
      if not open(f, handle, mode):
        raiseOSError(osLastError())
    else:
      let modeC =
        case mode
        of fmRead: cstring"rb"
        of fmWrite: cstring"wb"
        else: cstring"rb"
      # Declared to byte-match libc's `FILE *fdopen(int, const char *)`:
      # under useLibc, syncio's `File` still carries <stdio.h>, so the real
      # prototype is in scope in this TU and any deviation (even Nimony's
      # unsigned-char `cstring`) is a conflicting redeclaration.
      type ConstChar {.importc: "const char", nodecl.} = int8
      proc c_fdopen(fd: cint; mode: nil ptr ConstChar): File {.importc: "fdopen".}
      f = c_fdopen(handle, cast[ptr ConstChar](modeC))
      if f == nil:
        raiseOSError(osLastError())
    result = newFileStream(f)

  proc inputStream*(p: Process): Stream {.raises, tags: [].} =
    streamAccess(p)
    if p.inStream == nil:
      p.inStream = createStream(p.inHandle, fmWrite)
    result = p.inStream

  proc outputStream*(p: Process): Stream {.raises, tags: [].} =
    streamAccess(p)
    if p.outStream == nil:
      p.outStream = createStream(p.outHandle, fmRead)
    result = p.outStream

  proc errorStream*(p: Process): Stream {.raises, tags: [].} =
    streamAccess(p)
    if p.errStream == nil:
      p.errStream = createStream(p.errHandle, fmRead)
    result = p.errStream

  proc execCmd*(command: string): int {.raises,
      tags: [ExecIOEffect, ReadIOEffect, RootEffect].} =
    ## Executes `command` and returns its error code.
    # Run via /bin/sh -c with inherited standard streams and wait — the
    # semantics of libc `system()`, over the same fork+exec path as
    # startProcess (no libc `system` binding needed).
    var p = startProcess(command,
      options = {poEvalCommand, poParentStreams, poUsePath})
    result = waitForExit(p)
    close(p)

# ---------------------------------------------------------------------------
# Windows backend
# ---------------------------------------------------------------------------

when defined(windows):
  type
    FileHandleStream = ref object of StreamObj
      handle: Handle
      atTheEnd: bool

  proc closeHandleCheck(handle: Handle) {.raises.} =
    if closeHandle(handle).isFail:
      raiseOSError(osLastError())

  proc fileClose(h: var Handle) {.raises.} =
    if h.int > 4:
      closeHandleCheck(h)
      h = INVALID_HANDLE_VALUE

  proc fhFileClose(h: var FileHandle) {.raises.} =
    if h.int > 4:
      closeHandleCheck(Handle h)
      h = FileHandle(INVALID_HANDLE_VALUE)

  proc hsClose(s: Stream) {.nimcall, raises, tags: [WriteIOEffect].} =
    var fs = FileHandleStream(s)
    fileClose(fs.handle)

  proc hsAtEnd(s: Stream): bool {.nimcall, raises, tags: [].} =
    result = FileHandleStream(s).atTheEnd

  proc hsReadData(s: Stream; buffer: pointer; bufLen: int): int
      {.nimcall, raises, tags: [ReadIOEffect].} =
    var fs = FileHandleStream(s)
    if fs.atTheEnd:
      return 0
    var br: int32 = 0'i32
    let ok = readFile(fs.handle, buffer, bufLen.int32, addr br, nil)
    if ok.isFail and br != 0'i32:
      raiseOSError(osLastError())
    fs.atTheEnd = br == 0'i32
    result = br.int

  proc hsWriteData(s: Stream; buffer: pointer; bufLen: int)
      {.nimcall, raises, tags: [WriteIOEffect].} =
    var fs = FileHandleStream(s)
    var bw: int32 = 0'i32
    let ok = writeFile(fs.handle, buffer, bufLen.int32, addr bw, nil)
    if ok.isFail:
      raiseOSError(osLastError())

  proc newFileHandleStream(h: FileHandle): FileHandleStream =
    result = FileHandleStream(
      handle: Handle(h),
      atTheEnd: false,
      closeImpl: hsClose,
      atEndImpl: hsAtEnd,
      readDataImpl: hsReadData,
      writeDataImpl: hsWriteData)

  proc buildCommandLine(a: string; args: openArray[string]): string =
    result = quoteShell(a)
    for i in 0 ..< args.len:
      result.add ' '
      result.add quoteShell(args[i])

  proc buildEnv(env: StringTableRef): (cstring, int) =
    var L = 0
    for key, val in pairs(env): inc L, key.len + val.len + 2
    var str = cast[cstring](alloc0(L + 2))
    var pos = 0
    for key, val in pairs(env):
      var kv = key & "=" & val
      copyMem(cast[pointer](cast[uint](str) + uint(pos)),
              kv.toCString, kv.len + 1)
      inc pos, kv.len + 1
    result = (str, L)

  proc myDup(h: Handle; inherit: WINBOOL = WINBOOL 1): Handle {.raises.} =
    result = INVALID_HANDLE_VALUE
    let thisProc = getCurrentProcess()
    if duplicateHandle(thisProc, h, thisProc, result, 0'u32, inherit,
                       DUPLICATE_SAME_ACCESS).isFail:
      raiseOSError(osLastError())

  proc createPipeHandles(rdHandle, wrHandle: var Handle) {.raises.} =
    var sa: SECURITY_ATTRIBUTES = default(SECURITY_ATTRIBUTES)
    sa.nLength = sizeof(SECURITY_ATTRIBUTES).int32
    sa.lpSecurityDescriptor = nil
    sa.bInheritHandle = WINBOOL 1
    if createPipe(rdHandle, wrHandle, sa, 0'u32).isFail:
      raiseOSError(osLastError())

  proc startProcess*(command: string; workingDir: string = "";
                    args: openArray[string] = [];
                    env: nil StringTableRef = nil;
                    options: set[ProcessOption] = {poStdErrToStdOut}):
      Process {.raises, tags: [ExecIOEffect, ReadEnvEffect, RootEffect].} =
    var si: STARTUPINFO = default(STARTUPINFO)
    var procInfo: PROCESS_INFORMATION = default(PROCESS_INFORMATION)
    var hi, ho, he: Handle = INVALID_HANDLE_VALUE

    result = Process(options: options, exitFlag: true,
                     fProcessHandle: INVALID_HANDLE_VALUE,
                     fThreadHandle: INVALID_HANDLE_VALUE,
                     inHandle: FileHandle(INVALID_HANDLE_VALUE),
                     outHandle: FileHandle(INVALID_HANDLE_VALUE),
                     errHandle: FileHandle(INVALID_HANDLE_VALUE),
                     id: INVALID_HANDLE_VALUE,
                     inStream: nil, outStream: nil, errStream: nil,
                     exitStatus: 0'i32)

    si.cb = sizeof(STARTUPINFO).int32
    if poParentStreams notin options:
      si.dwFlags = STARTF_USESTDHANDLES.int32
      createPipeHandles(si.hStdInput, hi)
      createPipeHandles(ho, si.hStdOutput)
      if poStdErrToStdOut in options:
        si.hStdError = si.hStdOutput
        he = ho
      else:
        createPipeHandles(he, si.hStdError)
        if setHandleInformation(he, HANDLE_FLAG_INHERIT, 0'u32).isFail:
          raiseOSError(osLastError())
      if setHandleInformation(hi, HANDLE_FLAG_INHERIT, 0'u32).isFail:
        raiseOSError(osLastError())
      if setHandleInformation(ho, HANDLE_FLAG_INHERIT, 0'u32).isFail:
        raiseOSError(osLastError())
      result.inHandle = FileHandle(hi)
      result.outHandle = FileHandle(ho)
      result.errHandle = FileHandle(he)
    else:
      si.hStdError = getStdHandle(STD_ERROR_HANDLE)
      si.hStdInput = getStdHandle(STD_INPUT_HANDLE)
      si.hStdOutput = getStdHandle(STD_OUTPUT_HANDLE)
      result.inHandle = FileHandle(si.hStdInput)
      result.outHandle = FileHandle(si.hStdOutput)
      result.errHandle = FileHandle(si.hStdError)

    var cmdRoot = ""
    if poEvalCommand in options:
      cmdRoot = command
      assert args.len == 0
    else:
      cmdRoot = buildCommandLine(command, args)

    var ee: (cstring, int) = (nil.cstring, 0)
    if env != nil:
      ee = buildEnv(env)

    if poEchoCmd in options: echo cmdRoot

    var cmdWide = newWideCString(cmdRoot)
    var envWide =
      if ee[0].isNil: newWideCString(cstring(nil))
      else: newWideCString(ee[0], ee[1])
    var cwdWide =
      if workingDir.len == 0: newWideCString(cstring(nil))
      else:
        var tmp = workingDir
        newWideCString(tmp.toCString)

    var flags: DWORD = NORMAL_PRIORITY_CLASS or CREATE_UNICODE_ENVIRONMENT
    if poDaemon in options: flags = flags or CREATE_NO_WINDOW

    # `envWide` is `nil` (inherit the parent environment) unless a custom `env`
    # was supplied; either way it goes through as `lpEnvironment` so that a
    # custom environment actually reaches the child (CREATE_UNICODE_ENVIRONMENT
    # is set above, and the block was built wide accordingly).
    let success = createProcessW(nil, cmdWide.toWideCString, nil, nil,
                                 WINBOOL 1, flags,
                                 cast[pointer](envWide.toWideCString),
                                 cwdWide.toWideCString, si, procInfo)
    let lastError = osLastError()

    if poParentStreams notin options:
      fileClose(si.hStdInput)
      fileClose(si.hStdOutput)
      if poStdErrToStdOut notin options:
        fileClose(si.hStdError)

    if ee[0] != nil: dealloc(cast[pointer](ee[0]))

    if success.isFail:
      raiseOSError(lastError, command)

    result.fProcessHandle = procInfo.hProcess
    result.fThreadHandle = procInfo.hThread
    result.id = cast[Handle](uint64(procInfo.dwProcessId))
    result.exitFlag = false

  proc closeThreadAndProcessHandle(p: Process) {.raises.} =
    if p.fThreadHandle != Handle 0:
      closeHandleCheck(p.fThreadHandle)
      p.fThreadHandle = Handle 0
    if p.fProcessHandle != Handle 0:
      closeHandleCheck(p.fProcessHandle)
      p.fProcessHandle = Handle 0

  proc close*(p: Process) {.raises, tags: [WriteIOEffect].} =
    if poParentStreams notin p.options:
      if p.inStream == nil:
        fhFileClose(p.inHandle)
      else:
        close(p.inStream)
      if p.outHandle != p.errHandle:
        fhFileClose(p.errHandle)
      fhFileClose(p.outHandle)
    closeThreadAndProcessHandle(p)

  proc suspend*(p: Process) {.raises, tags: [].} =
    discard suspendThread(p.fThreadHandle)

  proc resume*(p: Process) {.raises, tags: [].} =
    discard resumeThread(p.fThreadHandle)

  proc running*(p: Process): bool {.raises, tags: [].} =
    if p.exitFlag:
      result = false
    else:
      let x = waitForSingleObject(p.fProcessHandle, 0'u32)
      result = x == WAIT_TIMEOUT

  proc terminate*(p: Process) {.raises, tags: [].} =
    if running(p):
      discard terminateProcess(p.fProcessHandle, 0'u32)

  proc kill*(p: Process) {.raises, tags: [].} =
    terminate(p)

  proc waitForExit*(p: Process; timeout: int = -1): int {.raises,
      tags: [TimeEffect].} =
    if p.exitFlag:
      return p.exitStatus.int
    let tout =
      if timeout < 0: INFINITE
      else: DWORD timeout.int32
    let res = waitForSingleObject(p.fProcessHandle, tout)
    if res == WAIT_TIMEOUT:
      terminate(p)
    var status: int32 = 0'i32
    discard getExitCodeProcess(p.fProcessHandle, status)
    if status != STILL_ACTIVE:
      p.exitFlag = true
      p.exitStatus = status
      closeThreadAndProcessHandle(p)
      result = status.int
    else:
      result = -1

  proc peekExitCode*(p: Process): int {.raises, tags: [].} =
    if p.exitFlag:
      return p.exitStatus.int
    result = -1
    let timedOut = waitForSingleObject(p.fProcessHandle, 0'u32) == WAIT_TIMEOUT
    if not timedOut:
      var status: int32 = 0'i32
      discard getExitCodeProcess(p.fProcessHandle, status)
      p.exitFlag = true
      p.exitStatus = status
      closeThreadAndProcessHandle(p)
      result = status.int

  proc inputStream*(p: Process): Stream {.raises, tags: [].} =
    streamAccess(p)
    if p.inStream == nil:
      p.inStream = newFileHandleStream(p.inHandle)
    result = p.inStream

  proc outputStream*(p: Process): Stream {.raises, tags: [].} =
    streamAccess(p)
    if p.outStream == nil:
      p.outStream = newFileHandleStream(p.outHandle)
    result = p.outStream

  proc errorStream*(p: Process): Stream {.raises, tags: [].} =
    streamAccess(p)
    if p.errStream == nil:
      p.errStream = newFileHandleStream(p.errHandle)
    result = p.errStream

  proc execCmd*(command: string): int {.raises,
      tags: [ExecIOEffect, ReadIOEffect, RootEffect].} =
    var si: STARTUPINFO = default(STARTUPINFO)
    var procInfo: PROCESS_INFORMATION = default(PROCESS_INFORMATION)
    si.cb = sizeof(STARTUPINFO).int32
    si.hStdError = getStdHandle(STD_ERROR_HANDLE)
    si.hStdInput = getStdHandle(STD_INPUT_HANDLE)
    si.hStdOutput = getStdHandle(STD_OUTPUT_HANDLE)
    var cmd = command
    var cmdWide = newWideCString(cmd.toCString)
    let ok = createProcessW(nil, cmdWide.toWideCString, nil, nil,
                            WINBOOL 0, NORMAL_PRIORITY_CLASS, nil, nil,
                            si, procInfo)
    if ok.isFail:
      raiseOSError(osLastError())
    let process = procInfo.hProcess
    discard closeHandle(procInfo.hThread)
    var L: int32 = 0'i32
    if waitForSingleObject(process, INFINITE) != -1'i32:
      discard getExitCodeProcess(process, L)
      result = L.int
    else:
      result = -1
    discard closeHandle(process)

# ---------------------------------------------------------------------------
# High-level procs common to both backends
# ---------------------------------------------------------------------------

iterator lines*(p: Process; keepNewLines = false): string
    {.sideEffect, raises, tags: [ReadIOEffect, TimeEffect].} =
  ## Iterates line-by-line over the process's stdout until EOF, then waits
  ## for the process to exit.
  var outp = p.outputStream
  var line = newStringOfCap(120)
  while outp.readLine(line):
    if keepNewLines:
      line.add '\n'
    yield line
  discard waitForExit(p)

proc readLines*(p: Process): (seq[string], int) {.raises,
    tags: [ReadIOEffect, TimeEffect].} =
  ## Reads all stdout lines and returns them together with the exit code.
  var acc: seq[string] = @[]
  for line in p.lines: acc.add line
  result = (acc, p.peekExitCode)

proc execProcess*(command: string; workingDir: string = "";
                  args: openArray[string] = [];
                  env: nil StringTableRef = nil;
                  options: set[ProcessOption] = {poStdErrToStdOut,
                      poUsePath, poEvalCommand}): string {.raises,
    tags: [ExecIOEffect, ReadIOEffect, RootEffect].} =
  ## Executes `command` and returns its combined stdout as a string.
  var p = startProcess(command, workingDir = workingDir, args = args,
                       env = env, options = options)
  var outp = outputStream(p)
  result = ""
  var line = newStringOfCap(120)
  while true:
    if outp.readLine(line):
      result.add line
      result.add '\n'
    elif not running(p):
      break
  close(p)

proc execCmdEx*(command: string;
                options: set[ProcessOption] = {poStdErrToStdOut, poUsePath};
                env: nil StringTableRef = nil;
                workingDir = ""; input = ""):
    (string, int) {.raises, tags: [ExecIOEffect, ReadIOEffect, RootEffect].} =
  ## Runs `command`, returning its output and exit code.
  var p = startProcess(command,
                       options = options + {poEvalCommand},
                       workingDir = workingDir, env = env)
  var outp = outputStream(p)
  if input.len > 0:
    inputStream(p).write(input)
  close(inputStream(p))
  var output = ""
  var exitCode = -1
  var line = newStringOfCap(120)
  while true:
    if outp.readLine(line):
      output.add line
      output.add '\n'
    else:
      exitCode = peekExitCode(p)
      if exitCode != -1: break
  close(p)
  result = (output, exitCode)

proc execProcesses*(cmds: openArray[string];
                    options: set[ProcessOption] =
                      {poStdErrToStdOut, poParentStreams};
                    n: int = countProcessorsAvailable()): int {.raises,
    tags: [ExecIOEffect, TimeEffect, ReadEnvEffect, RootEffect].} =
  ## Executes `cmds` with at most `n` in flight concurrently. Returns the
  ## maximum absolute exit code. `beforeRunEvent` / `afterRunEvent` hooks are
  ## omitted here; subscribe to them upstream if you need the callbacks.
  result = 0
  assert n > 0
  if cmds.len == 0: return 0
  var i = 0
  var alive = 0
  var q: seq[Process] = @[]
  let parallel = if n < cmds.len: n else: cmds.len
  while q.len < parallel:
    q.add startProcess(cmds[i], options = options + {poEvalCommand})
    inc i
    inc alive
  while alive > 0:
    # Spin through the queue: any process that has exited advances
    # the worklist, others are re-polled.
    var j = 0
    while j < q.len:
      if q[j] != nil and not running(q[j]):
        let exit = peekExitCode(q[j])
        let v = if exit < 0: -exit else: exit
        if v > result: result = v
        close(q[j])
        dec alive
        if i < cmds.len:
          q[j] = startProcess(cmds[i], options = options + {poEvalCommand})
          inc i
          inc alive
        else:
          q[j] = nil
      inc j
