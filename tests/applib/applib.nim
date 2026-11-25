import std/[syncio, envvars]

proc f(): cint {.nimcall.} = 1
proc g(): cint {.nimcall.} = 2

var cb: proc (): cint {.nimcall.} = f
cb = g # top-level code, run by `NimMain`
echo "module init called"

proc lib_call(): cint {.exportc, cdecl.} =
  echo "hi from lib"
  {.cast(assumeSync).}:
    result = cb()

proc lib_env(): cint {.exportc, cdecl.} =
  # std/envvars reads `nimEnviron`, which `NimMain` sets from libc's `environ`
  result = cint(getEnv("APPLIB_ENV").len)
