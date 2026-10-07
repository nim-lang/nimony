import std/syncio

# `.passive` procs defined here, called from another module. Their coro
# helpers (`coro` frame, `init` wrapper, `s<state>` procs) are generated
# once, by THIS module's hexer run, so the caller has to mangle them with
# this module's suffix to reach them.

proc innerStep*() {.passive.} =
  echo "inner a"
  suspend()
  echo "inner b"

proc pingpong*() {.passive.} =
  echo "ping"
  innerStep()
  echo "pong"

proc addUp*(a, b: int): int {.passive.} =
  return a + b

# `.passive` methods whose overrides live in another module. A call site
# here never sees those overrides; it has to reach them through the vtable.
type
  Shape* = ref object of RootObj

method name*(s: Shape): string {.base.} = "shape"

method area*(s: Shape): int {.passive.} =
  return 0

method describe*(s: Shape) {.passive.} =
  echo "a shape"

method greet*(s: Shape) {.passive.} =
  echo "hello from ", s.name

proc report*(s: Shape) {.passive.} =
  s.describe()
  let a = s.area()
  echo s.name, " area: ", a
  s.greet()

# `.passive` iterators consumed from another module: the `for` loop calls the
# iterator's `init` wrapper, which only this module's hexer run emits.
type
  NumIter* = iterator(a, b: int): int {.passive.}

iterator countTo*(n: int): int {.passive.} =
  var i = 1
  while i <= n:
    {.cast(noSideEffect).}:
      innerStep()            # a passive call between two yields
    yield i
    inc i

iterator pairsOf*[T](xs: seq[T]): (int, T) {.passive.} =
  var i = 0
  while i < xs.len:
    yield (i, xs[i])
    inc i

iterator words*(s: string): string {.passive.} =
  var w = ""
  for ch in s:
    if ch == ' ':
      if w.len > 0: yield w
      w = ""
    else:
      w.add ch
  if w.len > 0: yield w

iterator countup*(a, b: int): int {.passive.} =
  var i = a
  while i <= b:
    yield i
    inc i
