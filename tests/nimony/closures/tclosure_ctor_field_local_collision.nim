import std/syncio

# Regression: an object constructor inside a `.closure` proc whose field NAME
# is spelled the same as a captured local crashed lengc with
#   [Error] expected field name but got: (envp … env … x)
# lambdalifting pass 1 (`tr`) sent `OconstrX` to `trSons`, which recursed into
# the `(kv FIELD value)` pairs and rewrote the FIELD-identity symbol into an
# `(envp …)` env access when it shared a SymId with a captured local of the
# same spelling. sem's `name.N` numbering makes such collisions ordinary.
# Fixed by guarding the KvU field-identity position (mirror of pass 2's treKv,
# the DotX/DdotX selector guard, and iterinliner's copyWithMapping guard); the
# captured VALUE `x` is still rewritten, only the field name is preserved.
#
# `y` is not captured — it stays correct either way and guards against a
# blanket "never rewrite kv" over-fix.

type
  Ctx = object
    x: int
    y: int
  Holder = ref object
    cb: proc(): int {.closure.}

proc make(x: int): Holder =
  result = Holder(cb: proc(): int {.closure.} =
    let c = Ctx(x: x, y: x + 1)   # field name `x` collides with captured local
    c.x + c.y)

let h = make(10)
echo h.cb()   # 10 + 11
