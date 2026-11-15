import std/syncio
import deps / mdiscardablestmt

# An `if`/`case`/`try`/`block`/`when` in statement position is a statement:
# every branch is a statement, so a call to a `{.discardable.}` routine there
# is discarded whatever the order of the branches. The construct used to be
# typed by its first value branch, so a later statement branch was rejected
# ("type mismatch: got: void") and the construct reported as undiscarded.
# The same holds for the tail of a routine or template that returns nothing.

{.feature: "assumeSync".}  # test program: globals shared freely

var x = 0

proc f(tag: string): int {.discardable.} =
  echo "f ", tag
  result = 1

proc s(tag: string): string {.discardable.} =
  echo "s ", tag
  result = tag

proc fs(tag: string): seq[int] {.discardable.} =
  echo "fs ", tag
  result = @[1, 2]

proc mk[T](): seq[T] = @[]

proc mayFail(fail: bool): int {.discardable, raises.} =
  echo "mayFail ", fail
  if fail: raise IOError
  result = 2

proc tryBody(fail: bool) =
  try: mayFail(fail)
  except: x = 1
  echo x

proc exceptValue(fail: bool) =
  try: x = mayFail(fail)
  except: f("except")
  echo x

proc ifElse(c: bool) =
  if c: f("if") else: x = 3
  echo x

proc ifMixedTypes(c: bool) =
  if c: f("if") else: s("else")
  echo x

proc caseMixed(mode: int) =
  # a `string`, then a `seq` call, then a statement; and the reverse order
  case mode
  of 0: s("of 0")
  of 1: fs("of 1")
  else: x = 4
  case mode
  of 2: x = 5
  of 0: fs("of 0")
  else: s("case else")
  echo x

proc blockAndWhen() =
  block:
    f("block")
  when defined(nimonyNoSuchDefine):
    x = 6
  else:
    s("when")

proc nested(c: bool) =
  if c:
    try: f("nested try")
    except: x = 7
  else:
    case x
    of 0: x = 8
    else: s("nested case")
  echo x

proc generic[T](a: T; c: bool) =
  if c: f("generic") else: x = 9
  echo x

template tmpl(c: bool) =
  if c: s("template") else: x = 10

proc importedRoutine(c: bool) =
  if c: imported("if") else: x = 11
  echo x

proc noreturnBranch(c: bool): int =
  if c: f("before return") else: return 12
  result = 13

proc voidTail(c: bool) =
  # the last statement of a routine that returns nothing
  if c: s("tail") else: x = 14

# An iterator produces its values with `yield`: its last statement is a
# statement, in an inline and in a closure iterator.
func pf(): int {.discardable.} = 1
func ps(): string {.discardable.} = "ps"

iterator inlineTail(): int =
  var y = 0
  yield 16
  try: pf()
  except: y = 1

iterator stringTail(): int =
  yield 17
  ps()

iterator closureTail(): int {.closure.} =
  var y = 0
  yield 18
  if y == 0: ps() else: y = 2

proc loop() =
  for i in 0..2:
    if i == 1: x = 15 else: s("loop")

tryBody false
tryBody true
exceptValue false
exceptValue true
ifElse true
ifElse false
ifMixedTypes true
ifMixedTypes false
for mode in 0..2: caseMixed mode
blockAndWhen()
nested true
nested false
generic 0, true
generic 0, false
tmpl true
tmpl false
echo x
importedRoutine true
importedRoutine false
echo noreturnBranch(true)
echo noreturnBranch(false)
voidTail true
voidTail false
echo x
loop()
echo x
for v in inlineTail(): echo v
for v in stringTail(): echo v
for v in closureTail(): echo v

# In an expression the branches still share one type, inferred across
# branches whose own type is not known yet.
let c = x == 15
let d = if c: fs("let") else: @[]
let e = if not c: fs("let") else: mk()
let g = if c: f("let") else: 16
echo d.len, " ", e.len, " ", g
