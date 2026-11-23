import std/syncio

proc f(): cint {.nimcall.} = 1
proc g(): cint {.nimcall.} = 2

var cb: proc (): cint {.nimcall.} = f
cb = g # top-level code, run by `NimMain`
echo "module init called"

proc lib_call(): cint {.exportc, cdecl.} =
  echo "hi from lib"
  {.cast(assumeSync).}:
    result = cb()
