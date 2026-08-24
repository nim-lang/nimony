#
#
#           Lengc — what an `(instr …)` node may be moved through
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The optimizer's purity predicates ask about `(call …)`. They have to ask about
## `(instr …)` too, and until this module they did not: `cse.isPureExpr` answered
## TRUE for every intrinsic, so a `volatileLoad` was a pure value and everything
## keyed off one could be proved equal to everything keyed off the next. That is
## not a slow program — it is a status register read once and believed forever.
##
## The answer is per-opcode and it already existed: `IntrinsicRow.effects`, which
## every row has carried since the table was written and nothing had ever read.
## This is the reader. `Ctz` and friends say `efPure` and stay optimisable; a
## volatile access and an atomic do not.
##
## Resolution is `typenav.intrinsicOfCallee`, the same walk both back ends use to
## pick their builtin — not a second one. A private copy here got the `into`
## drain wrong on its early exits, which surfaced as `into: body did not consume
## all children` from the middle of an unrelated pass.

import ".." / ".." / "lib" / nifcoreparse
import ".." / ".." / "lib" / nifcdecl
import ".." / ".." / "lib" / intrinsics
import ".." / nifmodules
import ".." / typenav

proc intrinsicOpOf*(m: var MainModule; instrNode: Cursor): IntrinsicOp =
  ## The row an `(instr <sym> …)` applies, or `NoIntrinsicOp` when the callee
  ## cannot be resolved. Callers must read that as OPAQUE and never as pure: an
  ## unresolvable callee is precisely the case where nothing is known.
  result = NoIntrinsicOp
  if instrNode.kind != TagLit: return
  let callee = sub(instrNode)
  var bits = 0
  result = intrinsicOfCallee(m, callee, bits)

proc intrinsicEffects*(m: var MainModule; instrNode: Cursor): set[IntrinsicEffect] =
  ## `{}` for an unresolvable callee, which reads as "claims nothing" and is
  ## therefore the conservative answer to every question below.
  let op = intrinsicOpOf(m, instrNode)
  if op == NoIntrinsicOp: {} else: IntrinsicRows[op].effects

proc instrIsPure*(m: var MainModule; instrNode: Cursor): bool =
  ## Is this `(instr …)` a value that depends on nothing but its operands? Only a
  ## row that SAYS SO qualifies; silence is not a claim.
  efPure in intrinsicEffects(m, instrNode)

proc instrIsVolatile*(m: var MainModule; instrNode: Cursor): bool =
  ## A volatile access, which additionally may not be reordered against ANOTHER
  ## volatile access. That is a property of the pair rather than of either row,
  ## so it is asked here and not read out of `effects`.
  intrinsicOpOf(m, instrNode).isVolatile
