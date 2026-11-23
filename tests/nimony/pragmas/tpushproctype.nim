import std/syncio

# planetis-m/nimony-bugs-llms#329: a pushed `importc` is for the procs of the
# section, not for the proc types of their parameters.
{.push importc, cdecl.}
proc atexit(fn: proc () {.cdecl.}): cint
{.pop.}

type Callback = proc (x: cint): cint {.cdecl.}
{.push importc.}
proc abs(x: cint): cint {.cdecl.}
proc unusedTakesCallback(cb: Callback): cint {.cdecl.}
{.pop.}

proc onExit() {.cdecl.} = discard
echo atexit(onExit)
echo abs(-3)

# A pushed calling convention still reaches a proc type that spells none.
{.push cdecl.}
type CdeclCb = proc (x: int32): int32
{.pop.}
proc twice(x: int32): int32 {.cdecl.} = x * 2'i32
let cb: CdeclCb = twice
echo cb(21'i32)

# A pushed `exportc` (it names the declared symbol) does not.
{.push exportc.}
type Plain = proc (x: int32): int32
{.pop.}
proc thrice(x: int32): int32 = x * 3'i32
let p: Plain = thrice
echo p(3'i32)
