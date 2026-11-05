## Fixture for `nimony w --hostImports`: bodyless `importc` procs become `env`
## imports the host page provides. `setup.nim` builds this and runs it under
## `host.js`.
import std/syncio

proc hostAdd(a, b: cint): cint {.importc: "host_add".}
proc hostNote(x: cint) {.importc: "host_note".}

hostNote 7
echo "sum: ", hostAdd(40, 2)
