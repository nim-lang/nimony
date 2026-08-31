# Storing a *variable* of nimcall proc type into a `.closure.` location hit
#
#   typenav.nim(613) `srcProc.kind != NoSym` [AssertionDefect]
#
# `sigmatch.procTypeMatch` wraps every nimcall -> closure conversion in a
# `(toClosure X)` node, no matter whether the converted source is a routine
# symbol or a variable/field/parameter of proc type. `typenav.getTypeImpl`'s
# `ToClosureX` branch only knew the routine-symbol shape and asserted on the
# other one. The bug was latent until borrow checking started asking for the
# types of more nodes (nim-lang/nimony#2404).
#
# Both reported trigger paths are covered: storing into a local declaration,
# and wiring through a `var` parameter into a field.

import std / [syncio, assertions]

type
  Cb = object
    accept*: proc(): bool {.closure.}
  P = proc(): bool              # alias of the *nimcall* proc type
  PC = proc(): bool {.closure.} # alias of the closure proc type

proc alwaysTrue(): bool = true
proc wire(c: var Cb, accept: proc(): bool) = c.accept = accept
proc takeIt(x: PC): bool = x()

proc main =
  # path A: a local of nimcall proc type is the source of the conversion
  var p: proc(): bool = alwaysTrue
  var cb: proc(): bool {.closure.} = p
  assert cb()

  # ... and the same through named type aliases, so the navigator has to look
  # through the alias to find the proctype it must closure-ify
  var q: P = alwaysTrue
  var cb2: PC = q
  assert cb2()
  assert takeIt(q)

  # path B: the conversion happens on the argument of a `var` parameter and
  # the result is stored into a field
  var o = default(Cb)
  wire(o, p)
  assert o.accept()

  # an array element, i.e. a source that is neither a symbol nor a field
  var arr: array[2, P] = [alwaysTrue, alwaysTrue]
  var cb3: PC = arr[0]
  assert cb3()

  echo "ok"

main()
