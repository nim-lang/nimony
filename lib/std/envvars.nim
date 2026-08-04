when defined(nodejs):
  proc getEnv*(key: string, default = ""): string {.tags: [ReadEnvEffect].} =
    var ret = default.cstring
    let key2 = key.cstring
    {.emit: "const value = process.env[`key2`];".}
    {.emit: "if (value !== undefined) { `ret` = value };".}
    result = $ret

  proc existsEnv*(key: string): bool {.tags: [ReadEnvEffect].} =
    var key2 = key.cstring
    var ret: bool
    {.emit: "`ret` = `key2` in process.env;".}
    result = ret

  proc putEnv*(key, val: string) {.tags: [WriteEnvEffect].} =
    var key2 = key.cstring
    var val2 = val.cstring
    {.emit: "process.env[`key2`] = `val2`;".}

  proc delEnv*(key: string) {.tags: [WriteEnvEffect].} =
    var key2 = key.cstring
    {.emit: "delete process.env[`key2`];".}

  iterator envPairs*(): tuple[key, value: string] {.tags: [ReadEnvEffect], sideEffect.} =
    var num: int
    var keys: RootObj
    {.emit: "`keys` = Object.keys(process.env); `num` = `keys`.length;".}
    for i in 0..<num:
      var key, value: cstring
      {.emit: "`key` = `keys`[`i`]; `value` = process.env[`key`];".}
      yield ($key, $value)

# commented because it must keep working with js+VM
# elif defined(js):
#   {.error: "requires -d:nodejs".}

else:
  when defined(windows):
    from parseutils import skipIgnoreCase

  import strutils, oserrors


  type
    # TODO: {.importc: "char**", nodecl.}
    cstringArray = ptr UncheckedArray[cstring]

  proc delete[T](x: var seq[T], i: Natural) {.noSideEffect.} =
    let xl = x.len
    for j in i.int..xl-2:
      x[j] = move x[j+1]
    shrink(x, xl-1)

  const useWinEnv = defined(windows) and not defined(nimscript)
    ## Read and write the environment through kernel32 alone. No libc is
    ## involved on that path, so it works unchanged on the freestanding
    ## (`-d:nimNativeIo`) target, whose process has no C runtime at all.

  when useWinEnv:
    import widestrs
    import windows/winlean

  when not useWinEnv:
    # Real libc exports; bare importc, no <stdlib.h>. On the truly
    # freestanding (arkham) target these have no implementation, but that
    # target never mutates the process environment either.
    proc c_getenv(env: cstring): cstring {.importc: "getenv".}
    proc c_setenv(envname: cstring, envval: cstring, overwrite: cint): cint {.importc: "setenv".}
    proc c_unsetenv(env: cstring): cint {.importc: "unsetenv".}

  # Environment handling cannot be put into RTL, because the `envPairs`
  # iterator depends on `environment`.

  var
    envComputed {.threadvar.}: bool
    environment {.threadvar.}: seq[string]

  when defined(nimV2):
    proc unpairedEnvAllocs*(): int =
      result = environment.len
      if result > 0: inc result

  when useWinEnv:
    # Windows hands a process entry point no `envp` — GUI entry points never
    # did, and a PE entry point never does — so the block is fetched from the
    # OS instead. It arrives as UTF-16 `KEY=VALUE\0KEY=VALUE\0…\0\0`.
    func strEnd(cstr: WideCString): WideCString =
      ## The address of `cstr`'s NUL terminator — `wcschr(cstr, 0)`, spelled out
      ## rather than imported so the walk needs no libc `<string.h>`.
      var i = 0
      while int16(cstr[i]) != 0'i16: inc i
      result = cast[WideCString](cast[uint](cstr) + uint(i * 2))

    proc getEnvVarsC() =
      if not envComputed:
        environment = @[]
        var
          env = getEnvironmentStringsW()
          e = env
        if e == nil: return # an error occurred
        while true:
          var eend = strEnd(e)
          add(environment, $e)
          e = cast[WideCString](cast[uint](eend)+2)
          if int16(eend[1]) == 0'i16: break
        discard freeEnvironmentStringsW(env)

        envComputed = true

  else:
    # The generated `main` captures the env block it receives (`char** envp`)
    # into the `nimEnviron` global (hexer genMainProc), so neither libc's
    # `environ` nor Darwin's `_NSGetEnviron` is needed. Windows never comes
    # here: its entry point receives no `envp` at all, so the block is read
    # from `GetEnvironmentStringsW` above instead.
    var gEnv {.importc: "nimEnviron".}: cstringArray

    proc getEnvVarsC() =
      # retrieves the variables of char** env of C's main proc
      if not envComputed:
        environment = @[]
        var i = 0
        while gEnv[i] != nil:
          add environment, $gEnv[i]
          inc(i)
        envComputed = true

  proc findEnvVar(key: string): int =
    getEnvVarsC()
    var temp = key & "="
    for i in 0..high(environment):
      when defined(windows):
        if skipIgnoreCase(environment[i], temp) == len(temp): return i
      else:
        if startsWith(environment[i], temp): return i
    return -1

  proc getEnv*(key: string, default = ""): string {.tags: [ReadEnvEffect].} =
    ## Returns the value of the `environment variable`:idx: named `key`.
    ##
    ## If the variable does not exist, `""` is returned. To distinguish
    ## whether a variable exists or it's value is just `""`, call
    ## `existsEnv(key) proc <#existsEnv,string>`_.
    ##
    ## See also:
    ## * `existsEnv proc <#existsEnv,string>`_
    ## * `putEnv proc <#putEnv,string,string>`_
    ## * `delEnv proc <#delEnv,string>`_
    ## * `envPairs iterator <#envPairs.i>`_
    runnableExamples:
      assert getEnv("unknownEnv") == ""
      assert getEnv("unknownEnv", "doesn't exist") == "doesn't exist"


    var i = findEnvVar(key)
    if i >= 0:
      result = substr(environment[i], find(environment[i], '=')+1)
    else:
      when useWinEnv or defined(nimNativeIo):
        # No libc `getenv`: on Windows because the environment is kernel32's
        # alone, on the freestanding target because there is no libc at all.
        # The `environment` scan above is the complete view either way, so a
        # miss means "not set".
        result = default
      else:
        var key = key
        let kc = key.toCString()
        if kc.isNil:
          result = default
        else:
          var env = c_getenv(kc)
          if env == nil: result = default
          else: result = $env

  proc existsEnv*(key: string): bool {.tags: [ReadEnvEffect].} =
    ## Checks whether the environment variable named `key` exists.
    ## Returns true if it exists, false otherwise.
    ##
    ## See also:
    ## * `getEnv proc <#getEnv,string,string>`_
    ## * `putEnv proc <#putEnv,string,string>`_
    ## * `delEnv proc <#delEnv,string>`_
    ## * `envPairs iterator <#envPairs.i>`_
    runnableExamples:
      assert not existsEnv("unknownEnv")

    when useWinEnv or defined(nimNativeIo):
      # No libc `getenv` here (see `getEnv`) — and this has to be an `else`, not
      # an early return, or the reference below still reaches the linker. The
      # `environment` scan is the complete view, so it answers alone.
      result = findEnvVar(key) >= 0
    else:
      var key = key
      let kc = key.toCString()
      if kc.isNil:
        result = false
      elif c_getenv(kc) != nil:
        result = true
      else:
        result = findEnvVar(key) >= 0

  proc putEnv*(key, val: string) {.tags: [WriteEnvEffect], raises.} =
    ## Sets the value of the `environment variable`:idx: named `key` to `val`.
    ## If an error occurs, `OSError` is raised.
    ##
    ## See also:
    ## * `getEnv proc <#getEnv,string,string>`_
    ## * `existsEnv proc <#existsEnv,string>`_
    ## * `delEnv proc <#delEnv,string>`_
    ## * `envPairs iterator <#envPairs.i>`_

    # Note: by storing the string in the environment sequence,
    # we guarantee that we don't free the memory before the program
    # ends (this is needed for POSIX compliance). It is also needed so that
    # the process itself may access its modified environment variables!

    var indx = findEnvVar(key)
    if indx >= 0:
      environment[indx] = key & '=' & val
    else:
      add environment, (key & '=' & val)
      indx = high(environment)

    var key = key
    var val = val
    when useWinEnv:
      var k = newWideCString(key)
      var v = newWideCString(val)
      if isFail setEnvironmentVariableW(k.toWideCString(), v.toWideCString()):
        raiseOSError(osLastError())

    else:
      let kc = key.toCString()
      let vc = val.toCString()
      if kc.isNil:
        raise OutOfMemError
      if vc.isNil:
        raise OutOfMemError
      if c_setenv(kc, vc, 1'i32) != 0'i32:
        raiseOSError(osLastError())

  proc delEnv*(key: string) {.tags: [WriteEnvEffect], raises.} =
    ## Deletes the `environment variable`:idx: named `key`.
    ## If an error occurs, `OSError` is raised.
    ##
    ## See also:ven
    ## * `getEnv proc <#getEnv,string,string>`_
    ## * `existsEnv proc <#existsEnv,string>`_
    ## * `putEnv proc <#putEnv,string,string>`_
    ## * `envPairs iterator <#envPairs.i>`_
    var indx = findEnvVar(key)
    if indx >= 0:
      when useWinEnv:
        var key = key
        var k = newWideCString(key)
        if isFail setEnvironmentVariableW(k.toWideCString(), nil):
          raiseOSError(osLastError())
      else:
        var key = key
        let kc = key.toCString()
        if kc.isNil:
          raise OutOfMemError
        if c_unsetenv(kc) != 0'i32:
          raiseOSError(osLastError())
      environment.delete(indx)
    else:
      discard # Do nothing if the env var is not already set

  iterator envPairs*(): tuple[key, value: string] {.tags: [ReadEnvEffect], sideEffect.} =
    ## Iterate over all `environments variables`:idx:.
    ##
    ## In the first component of the tuple is the name of the current variable stored,
    ## in the second its value.
    ##
    ## See also:
    ## * `getEnv proc <#getEnv,string,string>`_
    ## * `existsEnv proc <#existsEnv,string>`_
    ## * `putEnv proc <#putEnv,string,string>`_
    ## * `delEnv proc <#delEnv,string>`_
    getEnvVarsC()
    for i in 0..high(environment):
      var p = find(environment[i], '=')
      yield (substr(environment[i], 0, p-1),
             substr(environment[i], p+1))
