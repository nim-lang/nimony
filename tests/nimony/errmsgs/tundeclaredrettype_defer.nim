# An undeclared return type, a `defer`, and a `return` whose type has to be
# INFERRED used to crash the compiler instead of diagnosing the source:
#
#   nifcore.nim(895) `c.rem == 0` into: body did not consume all N children
#     deferstmts.nim(97) trReturn
#
# `semcall` matched the callee and `leaveCall` closed the call tree, and the
# "could not infer type" diagnostic was then APPENDED beside it — leaving a
# `(ret X (err …))` with two children where the grammar allows one. `defer`
# lowering was simply the first pass to walk that tree and notice
# (nim-lang/nimony#2400).
#
# All three properties have to meet: without the `defer` nothing walks the
# tree, without the `return` there is no fixed-arity node to overfill, and a
# concretely typed `return 1` reports a plain type mismatch instead of going
# through the failed inference. Those neighbours are in
# `tundeclaredrettype_variants.nim`, and each of them already behaved.
#
# Reported as `return @[]`, which fails inference the same way through
# `system.@`; spelled with a local generic here so the golden does not name a
# typevar of `system`, whose numbering shifts whenever `system.nim` does.

proc emptySeq[T](): seq[T] = @[]

proc brokenProc(): NotAType =
  defer: discard
  return emptySeq()
