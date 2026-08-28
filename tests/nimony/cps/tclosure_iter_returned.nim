## A proc returning a `.closure` iterator (issue #2205). Two bugs blocked
## this shape:
##   1. `toNonClosureProcType` asserted on `ItertypeT` — genCall runs
##      before cps rewrites itertypes to wrapper proctypes, so a
##      first-class closure-iter value legitimately reaches it.
##   2. For a NESTED closure iter, elimLambdas' end-of-pass signature
##      flush replayed `shouldPublish` offsets against stmtsBuf, but the
##      iter was written into treProcLift's local lift buffer — so a
##      random subtree got published as the iter's decl and duplifier
##      died on `fnType.substructureKind == ParamsU`.
##
## Also exercises Nim's shared-state iter-value semantics: `break` then
## resuming the same value continues where it left off.
##
## This one deliberately does NOT capture, so it stays a test of the plain
## returned-iter shape. Captures from the enclosing proc inside the iter
## body live in `tclosure_iter_capture.nim` (issue #2340).

import std / syncio

proc makeIter(): iterator(): int {.closure.} =
    return iterator(): int {.closure.} =
        var i = 0
        while i <= 4:
            yield i
            inc i

proc main() =
  let it = makeIter()
  for v in it():
    echo v
    if v == 2: break
  for v in it():
    echo v

main()
