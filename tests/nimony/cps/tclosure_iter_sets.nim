## Sets inside a closure iterator — every construct `desugar` owns.
##
## `desugar` used to copy an `(iterator …)` decl verbatim, alongside the macros
## and templates. That is right for an INLINE iterator: `elimForLoops` splices
## its body into each caller before desugar runs, so the decl left behind is
## dead. A `.closure` iterator is not that — it survives as a real routine, and
## lambdalifting and cps turn its body into a state machine that goes all the
## way to `lengcgen`. So nothing ever lowered the forms desugar is responsible
## for, and a set literal reached the back end alive:
##   [Error] BUG: not eliminated: (setconstr …)
##
## Sets are what made it visible, being the biggest thing desugar lowers, but
## the gap was the whole pass: set operations, `card`, `incl`/`excl` were all
## unreachable in a closure iterator too. Each gets a case here.
##
## Both set representations are covered: `set[Color]` fits in a machine word,
## while `set[char]` is 32 bytes and so takes the array-of-bytes path with its
## separate `zeroMem`-based runtime construction.
##
## `Flags.cols` is deliberately spelled like the local `cols`: sem's `name.N`
## numbering lets a field share a SymId with a lifted local, and `coroTr` used
## to rewrite the constructor's field-identity key into a frame access
## ("expected field name but got: (dot …)"). See
## `closures/tclosure_ctor_field_local_collision` for the same bug in
## lambdalifting, which had been fixed there but not here.

import std / syncio
type
  Color = enum
    red, green, blue
  Flags = object
    cols: set[Color]
    n: int

iterator it(): int {.closure.} =
  # empty-set initializer, the original repro
  var cols: set[Color] = {}
  yield card(cols)
  # non-empty constructor, live across a yield (a frame field)
  cols = {red, blue}
  yield card(cols)
  yield (if red in cols: 1 else: 0)
  yield (if green in cols: 1 else: 0)
  # set operations
  var other = {green}
  yield card(cols + other)
  yield card(cols * {red})
  yield card(cols - {red})
  yield (if {red} <= cols: 1 else: 0)
  yield (if cols == {red, blue}: 1 else: 0)
  # incl / excl
  incl(cols, green)
  yield card(cols)
  excl(cols, red)
  yield card(cols)
  # a set inside an object local (nimsem's own default(set[T]) fill)
  var fl = Flags(n: 7)
  yield card(fl.cols)
  fl.cols = {blue}
  yield card(fl.cols)
  # a BIG set: > 8 bytes, so the array-of-bytes representation
  var big: set[char] = {}
  yield card(big)
  big = {'a', 'z'}
  yield card(big)
  yield (if 'a' in big: 1 else: 0)
  yield (if 'b' in big: 1 else: 0)
  incl(big, 'b')
  yield card(big)

proc main() =
  for v in it():
    echo v
main()
