{.feature: "lenientnils".}
import std/[syncio, assertions]

# Regression: a runtime-checked downcast `T(expr)` where `expr` is a COMPOUND
# expression (not a plain symbol) crashed hexer with
#   [Bug] could not find symbol: `vtableTemp.N
# trBaseobj mints a temp for the compound operand and emits its VarS, but never
# registered the temp's type in the type cache; trInstanceofImpl's first act is
# getType on that temp, which then fell through to the "could not find symbol"
# ICE. Plain-symbol operands escaped because they reuse the real symbol.

type
  Base = ref object of RootObj
    tag: int
  Derived = ref object of Base
    text: string
  Holder = ref object of RootObj
    item: Base

method draw(n: Base) {.base.} = discard
method draw(n: Derived) = discard

proc viaCompound(h: Holder): string =
  # `h.item` is a compound operand -> trBaseobj mints a temp (the fixed path);
  # the downcast's own runtime check runs against that (registered) temp.
  Derived(h.item).text

proc viaSymbol(b: Base): string =
  # plain symbol operand -> UsesSelf path (must still work)
  Derived(b).text

proc main() =
  let d = Derived(text: "a")
  let h = Holder(item: d)
  assert viaCompound(h) == "a"
  assert viaSymbol(d) == "a"
  echo "ok"

main()
