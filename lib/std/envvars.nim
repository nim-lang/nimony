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

  iterator envPairs*(): tuple[key, value: string] {.tags: [ReadEnvEffect].} =
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

  when defined(windows) and not defined(nimscript):
    import widestrs


    type
      WINBOOL = int32
      ## `WINBOOL` uses opposite convention as posix, !=0 meaning success.
      # xxx this should be distinct int32, distinct would make code less error prone
    proc getEnvironmentStringsW(): WideCString {.
      importc: "GetEnvironmentStringsW", header: "<windows.h>".}
    proc freeEnvironmentStringsW(env: WideCString): WINBOOL {.
      importc: "FreeEnvironmentStringsW", header: "<windows.h>".}
    proc setEnvironmentVariableW(name, value: WideCString): WINBOOL {.
      importc: "SetEnvironmentVariableW", header: "<windows.h>".}

  proc c_getenv(env: cstring): cstring {.
    importc: "getenv", header: "<stdlib.h>".}
  when defined(vcc):
    proc c_putenv_s(envname: cstring, envval: cstring): cint {.importc: "_putenv_s", header: "<stdlib.h>".}
  else:
    proc c_setenv(envname: cstring, envval: cstring, overwrite: cint): cint {.importc: "setenv", header: "<stdlib.h>".}
  proc c_unsetenv(env: cstring): cint {.
    importc: "unsetenv", header: "<stdlib.h>".}

  # Environment handling cannot be put into RTL, because the `envPairs`
  # iterator depends on `environment`.

  var
    envComputed {.threadvar.}: bool
    environment {.threadvar.}: seq[string]

  when defined(nimV2):
    proc unpairedEnvAllocs*(): int =
      result = environment.len
      if result > 0: inc result

  when defined(windows) and not defined(nimscript):
    # because we support Windows GUI applications, things get really
    # messy here...
    when defined(cpp):
      proc strEnd(cstr: WideCString, c = 0'i32): WideCString {.
        importcpp: "(NI16*)wcschr((const wchar_t *)#, #)", header: "<string.h>".}
    else:
      proc strEnd(cstr: WideCString, c = 0'i32): WideCString {.
        importc: "wcschr", header: "<string.h>".}

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
          if eend[1].int == 0: break
        discard freeEnvironmentStringsW(env)

        envComputed = true

  else:
    const
      useNSGetEnviron = (defined(macosx) and not defined(ios) and not defined(emscripten)) or defined(nimscript)

    when useNSGetEnviron:
      # From the manual:
      # Shared libraries and bundles don't have direct access to environ,
      # which is only available to the loader ld(1) when a complete program
      # is being linked.
      # The environment routines can still be used, but if direct access to
      # environ is needed, the _NSGetEnviron() routine, defined in
      # <crt_externs.h>, can be used to retrieve the address of environ
      # at runtime.
      proc NSGetEnviron(): ptr cstringArray {.
        importc: "_NSGetEnviron", header: "<crt_externs.h>".}
    elif defined(haiku):
      var gEnv {.importc: "environ", header: "<stdlib.h>".}: cstringArray
    else:
      var gEnv {.importc: "environ".}: cstringArray

    proc getEnvVarsC() =
      # retrieves the variables of char** env of C's main proc
      if not envComputed:
        environment = @[]
        when useNSGetEnviron:
          var gEnv = NSGetEnviron()[]
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
      return substr(environment[i], find(environment[i], '=')+1)
    else:
      var key = key
      var env = c_getenv(key.toCString())
      if env == nil: return default
      result = $env

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

    var key = key
    if c_getenv(key.toCString()) != nil: return true
    else: return findEnvVar(key) >= 0

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
    when defined(windows) and not defined(nimscript):
      var k = newWideCString(key)
      var v = newWideCString(val)
      if setEnvironmentVariableW(k.toWideCString(), v.toWideCString()) == 0'i32:
        raiseOSError(osLastError())

    elif defined(vcc):
      if c_putenv_s(key, val) != 0'i32:
        raiseOSError(osLastError())
    else:
      if c_setenv(key.toCString(), val.toCString(), 1'i32) != 0'i32:
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
      when defined(windows) and not defined(nimscript):
        var key = key
        var k = newWideCString(key.toCString(), key.len).toWideCString()
        if setEnvironmentVariableW(k, nil) == 0'i32:
          raiseOSError(osLastError())
      else:
        var key = key
        if c_unsetenv(key.toCString()) != 0'i32:
          raiseOSError(osLastError())
      environment.delete(indx)
    else:
      discard # Do nothing if the env var is not already set

  iterator envPairs*(): tuple[key, value: string] {.tags: [ReadEnvEffect].} =
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