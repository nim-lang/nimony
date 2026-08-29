# The input-dispatcher shape (canvas-browser sprint, 2026-07-24): capture-free
# {.closure.} handlers stored in a sorted seq and invoked with an aggregate
# parameter, plus a ref global assigned via the dup path. Caught, in one
# night: algorithm.sort's len<2 bogus dealloc (stdlib), the closure-cast
# env-arity trap (ithaqua thunks now bridge it), and the flexarray-const
# undersizing that silently overwrote neighbouring globals (ithaqua's
# static-layout invariant now rejects it at emit time). The dispatcher's
# real parameter is a C union — arkham (the native oracle) has no union
# support, so the fixture uses a plain object; union field access is
# exercised by the sumi browser gates.
{.feature: "lenientnils".}
import std/[syncio, algorithm]

type
  Ev {.bycopy.} = object
    kind: uint32
    payload: array[4, uint32]

  Handler = object
    pri: int
    fn: proc(e: Ev): bool {.closure.}

  Disp = ref object
    handlers: seq[Handler]

  Thing = ref object
    x: int

var gThing: Thing = nil
var hits = 0

proc onA(e: Ev): bool {.closure.} =
  if e.kind == 7: inc hits
  true
proc onB(e: Ev): bool {.closure.} =
  if e.kind == 7: hits += 100
  true

proc addH(d: Disp, pri: int, fn: proc(e: Ev): bool {.closure.}) =
  d.handlers.add(Handler(pri: pri, fn: fn))
  d.handlers.sort(proc(a, b: Handler): int = a.pri - b.pri)

proc fire(d: Disp, e: Ev): int =
  result = 0
  for h in d.handlers:
    if h.fn(e): inc result

proc run() =
  let t = Thing(x: 42)
  gThing = t                     # dup-path store (t used below)
  let d = Disp(handlers: @[])
  d.addH(2, onA)                 # sort at len 1 (the old bogus-dealloc crash)
  d.addH(1, onB)                 # sort at len 2 (real merge over closures)
  var e = Ev(kind: 7)
  echo "fired: ", d.fire(e), " hits: ", hits
  echo "kept: ", t.x, " global: ", (if gThing == nil: -1 else: gThing.x)

run()
