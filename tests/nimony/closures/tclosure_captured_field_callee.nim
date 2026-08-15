import std/syncio

# Regression: a closure-typed OBJECT FIELD reached THROUGH a captured env and
# CALLED directly. lambdalifting pass 1 (`tr`) rewrites the captured object
# base of `state.onEvent()` into `(envp EnvType state)`, which typenav cannot
# type; pass 2's `genCall` then called `getType` on the whole `(dot (envp …)
# onEvent)` callee, got `auto`, decided `wantsEnv = false`, and emitted the
# closure call as a DIRECT call of the {fn, env} tuple value. clang rejected
# it: "called object type '…tuple…' is not a function or function pointer".
#
# Fixed by resolving the closure field's type through an env-aware
# `capturedBaseType` walk (envp leaf via `envFieldType`, intermediate fields
# via `lookupField`) when `getType` on a Dot/Ddot callee fails to yield a
# closure — the call-position sibling of the direct-envp gap-fill already in
# `genCall` and of the coro_transform nil-compare shape generalization
# (7bbe47b0). Only overrides when the field itself is a closure, so the
# non-captured control below (a plain-proc caller, whose `getType` already
# succeeds) stays a normal tuple-projected call.

type
  State = ref object of RootObj
    onZero: proc () {.closure.}
    onArg: proc (level: int) {.closure.}
    onArgs: proc (study: string; series: int) {.closure.}

# --- control: field called from a PLAIN proc (state is a real param, not
# captured — getType already types the ddot). Guards against an over-fix.
proc fireDirect(state: State) =
  state.onZero()

proc buildZero(): proc () {.closure.} =
  let state = State()
  state.onZero = proc () {.closure.} = echo "zero"
  result = proc () {.closure.} =
    state.onZero()               # captured object, zero-arg closure field

proc buildArg(): proc () {.closure.} =
  let state = State()
  state.onArg = proc (level: int) {.closure.} = echo "arg ", level
  result = proc () {.closure.} =
    state.onArg(7)               # captured object, closure field with an arg

proc buildArgsCaptured(): proc () {.closure.} =
  let state = State()
  state.onArgs = proc (study: string; series: int) {.closure.} =
    echo "args ", study, " ", series
  let study = "CT"
  let series = 3
  result = proc () {.closure.} =
    state.onArgs(study, series)  # args are themselves captured into the env

let s = State()
s.onZero = proc () {.closure.} = echo "direct"
fireDirect(s)

let f0 = buildZero()
f0()
let f1 = buildArg()
f1()
let f2 = buildArgsCaptured()
f2()
