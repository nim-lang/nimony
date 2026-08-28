## Regression: an object constructor inside a `.closure` ITERATOR whose field
## NAME is spelled the same as a frame-lifted local crashed lengc with
##   [Error] expected field name but got: (dot … `this` … val)
##
## `coroTr` sent the `(kv FIELD value)` pair through its generic walk, which
## recursed into the field-identity key and rewrote it into a coroutine-frame
## access when it shared a SymId with a lifted local of the same spelling —
## sem's `name.N` numbering makes such collisions ordinary. Fixed by guarding
## the KvU field-identity position, the mirror of `coroTr`'s own DotX/DdotX
## selector guard.
##
## This is `closures/tclosure_ctor_field_local_collision` for the coroutine
## lowering: lambdalifting had the guard, cps did not.
##
## `other` is not lifted into the frame — it stays correct either way and
## guards against a blanket "never rewrite kv" over-fix, which would break the
## captured VALUE.

import std / syncio

type
  Rec = object
    val: int
    other: int

iterator it(): int {.closure.} =
  var val = 5              # a LOCAL spelled like Rec's field
  let other = 3            # used in one state only: never lifted
  yield val                # `val` crosses a yield, so it IS lifted
  var r = Rec(val: 9, other: other)
  yield r.val              # the field, not the local
  yield r.other            # the captured VALUE must still be rewritten
  yield val                # the local, still intact

proc main() =
  for v in it():
    echo v

main()
