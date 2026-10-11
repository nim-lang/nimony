import std/syncio
import deps/mcrosspassive

# Regression: a `.passive` proc defined in one module, driven from a
# `.passive` proc in another. Used to fail hexer with
# `could not find symbol: pingpong`init.0.<callerModule>` — the helper was
# named for the transforming module rather than the defining one, and even
# once named right, the caller's hexer run has no way to load a signature
# that only ever existed in the defining module's run.

proc driver() {.passive.} =
  echo "driver start"
  pingpong()
  let s = addUp(2, 3)
  echo "sum: ", s
  echo "driver end"

driver()

# Regression: a `.passive` method overridden in a module other than the one
# declaring it was bound statically to the base implementation when called
# through the base type, and the override landed in the wrong vtable slot, so
# a dispatching call from the base's module jumped into the frame-taking
# state proc and crashed.

type
  Square = ref object of Shape
    side: int

method name(s: Square): string = "square"

method area(s: Square): int {.passive.} =
  return s.side * s.side

method describe(s: Square) {.passive.} =
  echo "a square"
  suspend()
  echo "with four sides"

proc viaBase(s: Shape) {.passive.} =
  s.describe()
  echo "area: ", s.area()

let shapes: seq[Shape] = @[Shape(), Square(side: 3)]
for s in shapes:
  s.describe()            # dispatch from a normal proc
  viaBase(s)              # dispatch from a passive proc in this module
  report(s)               # dispatch from the declaring module
  complete(delay s.describe())  # `delay` dispatches too

# Regression: a `for` loop over a `.passive` iterator from another module
# failed hexer with `could not find symbol: countTo`init.0.<definingModule>`.

proc consume(g: NumIter) =
  for v in g(3, 5): echo "value: ", v

proc iterFromPassive() {.passive.} =
  for x in countTo(2): echo "passive countTo: ", x
  for w in words("hello passive world"): echo "word: ", w

proc pairsFromPassive() {.passive.} =
  for (i, s) in pairsOf(@["a", "b"]): echo "pair: ", i, " ", s

for x in countTo(2): echo "regular countTo: ", x
for (i, v) in pairsOf(@[10, 20]): echo "regular pair: ", i, " ", v
consume(countup)
iterFromPassive()
pairsFromPassive()
