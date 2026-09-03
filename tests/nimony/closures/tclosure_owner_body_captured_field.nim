import std/syncio

# Regression: a closure OWNER whose nested non-escaping closure captures a `ref`
# param AND whose own body also reads a field of that param. Upstream #2074's
# env==nil dispatch fired for the local closure VAR call (`cl()` — its type
# resolves to the concrete lifted-lambda `ProcT`, not a proctype); the
# else-branch then fed the lambda's whole decl — body and all — to
# `toNonClosureProcType`, which copied the still-`ddot` field accesses in that
# body verbatim into the cast, and those reached the duplifier as
# "nodekind should have been eliminated in desugar.nim". Gating the nil-check to
# a raw `ProctypeT` callee fixes it. The owner reads the LIVE ref (shared into
# the env), not a moved-from husk: `d.val` mutated to 99 after the capture is
# seen by both the owner body and the closure.

type Data = ref object
  val: int

proc run() =
  let d = Data(val: 1)
  var seen = 0
  let cl = proc () {.closure.} =
    seen = d.val
  d.val = 99
  if d.val != 0:
    echo "owner ", d.val
  cl()
  echo "closure ", seen

run()
