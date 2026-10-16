import std/[assertions, syncio]

when defined(posix):
  import std/[os, osproc, streams, strtabs, envvars, dirs]
  import std/posix/posix

  proc getpid(): Pid {.importc: "getpid".}
  proc getpgid(pid: Pid): Pid {.importc: "getpgid".}

  proc collect(command: string; args: openArray[string] = [];
               workingDir: string = ""; env: nil StringTableRef = nil;
               options: set[ProcessOption] = {}): string {.raises.} =
    let p = startProcess(command, workingDir, args, env, options)
    try:
      result = readAll(outputStream(p))
      assert waitForExit(p) == 0
    finally:
      close(p)

  proc main() {.raises.} =
    assert collect("/bin/sh", ["-c", "printf '%s|%s' \"$1\" \"$2\"", "sh",
                                "a b", "'quoted'"]) == "a b|'quoted'"
    assert collect("printf shell", options = {poEvalCommand}) == "shell"
    let env = newStringTable(modeCaseSensitive)
    env["SPAWN_TEST_VALUE"] = "custom value"
    env["PATH"] = "/nonexistent"
    assert collect("sh", ["-c", "printf '%s' \"$SPAWN_TEST_VALUE\""],
                   env = env, options = {poUsePath}) == "custom value"

    let separate = startProcess("/bin/sh", args = ["-c", "printf out; printf err >&2"],
                                options = {})
    assert readAll(outputStream(separate)) == "out"
    assert readAll(errorStream(separate)) == "err"
    assert waitForExit(separate) == 0
    close(separate)
    assert collect("/bin/sh", ["-c", "printf out; printf err >&2"],
                   options = {poStdErrToStdOut}) == "outerr"

    let input = startProcess("/bin/sh", args = ["-c", "cat"], options = {})
    write(inputStream(input), "stdin data")
    close(inputStream(input))
    assert readAll(outputStream(input)) == "stdin data"
    assert waitForExit(input, 5000) == 0
    close(input)

    let parent = startProcess("/bin/sh", args = ["-c", "exit 23"],
                              options = {poParentStreams})
    assert waitForExit(parent) == 23
    close(parent)
    assert execShellCmd("exit 17") == 17
    assert execCmd("exit 19") == 19

    let daemon = startProcess("/bin/sh", args = ["-c", "printf 'ready\\n'; read line"],
                              options = {poDaemon})
    assert readLine(outputStream(daemon)) == "ready"
    assert getpgid(Pid(processID(daemon))) == Pid(processID(daemon))
    close(inputStream(daemon))
    discard waitForExit(daemon)
    close(daemon)

    let sleeper = startProcess("/bin/sh", args = ["-c", "exec sleep 30"], options = {})
    assert waitForExit(sleeper, 1) == 128 + int(SIGKILL)
    assert not running(sleeper)
    close(sleeper)
    let stopped = startProcess("/bin/sh", args = ["-c", "exec sleep 30"], options = {})
    terminate(stopped)
    assert waitForExit(stopped, 5000) == 128 + int(SIGTERM)
    close(stopped)

    let tmp = getTempDir() & "nimony_spawn_" & $getpid()
    createDir(path(tmp))
    var script = tmp & "/child"
    try:
      writeFile(script, "#!/bin/sh\nprintf relative")
      assert chmod(script.toCString, Mode(0o755)) == 0
      assert collect("./child", workingDir = tmp) == "relative"
      assert collect("test -f child && printf cwd", workingDir = tmp,
                     options = {poEvalCommand}) == "cwd"

      when defined(illumos):
        from std/errorcodes/errorcodes_posix import posixToErrorCode
        proc errnoSlot(): ptr cint {.importc: "___errno".}

        proc fdCount(): int {.raises.} =
          result = 0
          for kind, file in walkDir(path("/proc/self/fd")):
            inc result

        proc launchFails(command: string; wd: string = "";
                         options: set[ProcessOption] = {};
                         expected: cint = ENOENT) {.raises.} =
          var failed = false
          errnoSlot()[] = 0
          try:
            let p = startProcess(command, workingDir = wd, options = options)
            discard waitForExit(p)
            close(p)
          except ErrorCode as e:
            assert e == posixToErrorCode(expected)
            failed = true
          assert failed

        # Returned spawn errors must be used directly, regardless of errno.
        let before = fdCount()
        for i in 0 ..< 40:
          launchFails(tmp & "/missing")
          launchFails("/bin/sh", tmp & "/missing-dir")
        assert fdCount() == before
        assert chmod(script.toCString, Mode(0o644)) == 0
        launchFails(script, expected = EACCES)
        assert chmod(script.toCString, Mode(0o755)) == 0
        writeFile(script, "not an executable image\n")
        launchFails(script, expected = ENOEXEC)

        # PATH candidates remain relative to the child's requested cwd.
        let oldPath = getEnv("PATH")
        try:
          putEnv("PATH", "missing:.")
          writeFile(script, "#!/bin/sh\nprintf path")
          assert collect("child", workingDir = tmp, options = {poUsePath}) == "path"
          launchFails("missing-child", tmp, {poUsePath})
          assert chmod(script.toCString, Mode(0o644)) == 0
          launchFails("child", tmp, {poUsePath}, EACCES)
        finally:
          putEnv("PATH", oldPath)

        # Every internal pipe end must retain CLOEXEC in the parent.
        let pipes = startProcess("/bin/sh", args = ["-c", "exit 0"], options = {})
        for fd in [inputHandle(pipes), outputHandle(pipes), errorHandle(pipes)]:
          assert (fcntl(fd, 1'i32) and FD_CLOEXEC) != 0
        assert waitForExit(pipes) == 0
        close(pipes)

        # Closing standard descriptors must not cause dup2 sources to be
        # overwritten, or the later close actions to close their targets.
        var saved = [-1'i32, -1'i32, -1'i32]
        for i in 0 ..< 3:
          saved[i] = fcntl(cint(i), F_DUPFD_CLOEXEC, 3'i32)
          assert saved[i] >= 0
        var output = ""
        try:
          for i in 0 ..< 3: discard posix.close(cint(i))
          output = collect("/bin/sh", ["-c", "printf closed; printf err >&2"],
                           options = {poStdErrToStdOut})
        finally:
          for i in 0 ..< 3:
            discard dup2(saved[i], cint(i))
            discard posix.close(saved[i])
        assert output == "closederr"
        assert fdCount() == before
    finally:
      discard tryRemoveFile(path(script))
      removeDir(path(tmp))

  try:
    main()
  except:
    quit "unexpected process test error"

echo "ok"
{.feature: "assumeSync".}
