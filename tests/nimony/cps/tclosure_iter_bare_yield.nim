## A valueless `yield` in a `.closure` iterator must survive nifler
## (issue #2204): `nkYieldStmt` was missing from bridge.nim's allowEmpty
## statement list, so `yield` with no value tripped an AssertionDefect
## at parse time while `return` with no value worked.
##
## Driving such an iterator through a for loop is still blocked further
## down the pipeline (void for-var reaches C codegen), so this test only
## guards the parse+build path.

import std / syncio

iterator tick() {.closure.} =
  yield
  yield

echo "parsed"
