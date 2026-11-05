import std/syncio

# A `{.discardable.}` call as the LAST statement of a branch, with an argument
# that is itself a call (written, or supplied by a default). The branch is an
# expression typed by the call; lowering it binds the argument to a temp before
# the call, and the branch came out with two statements where it holds one, so
# the call itself was dropped: only the argument ran.

{.feature: "assumeSync".}  # test program: globals shared freely

var counter = 0

proc g(): int =
  inc counter
  result = counter

proc f(x: int): int {.discardable.} =
  echo "f ", x
  result = x

proc h(tag: string; d = g()): int {.discardable.} =
  echo "h ", tag, " ", d
  result = d

proc s(tag: string; d = g()): string {.discardable.} =
  echo "s ", tag, " ", d
  result = tag

proc ifElifElse(mode: int) =
  if mode == 0: f(g())
  elif mode == 1: f(g() + 100)
  else: f(g() + 200)

proc ifNoElse(mode: int) =
  if mode == 0: h("if")
  elif mode == 1: h("elif")

proc elseAfterVoid(mode: int) =
  if mode == 0: echo "void branch"
  else: h("else")

proc caseBranches(mode: int) =
  case mode
  of 0: h("of 0")
  of 1: h("of 1")
  else: h("case else")

proc whenBlock() =
  when true:
    f(g())
  block:
    h("block")

proc multiStmtBranch(mode: int) =
  if mode == 0:
    echo "first"
    f(g())

proc withDestructor(mode: int) =
  if mode == 0: s("if")
  elif mode == 1: s("elif")
  echo "after"

proc fail(code: int; d = g()): int {.discardable, raises.} =
  echo "fail ", code
  if code != 0: raise IOError
  result = code

proc raisingDefault(mode: int) {.raises.} =
  if mode == 0: discard
  elif mode == 1: fail(5)

for mode in 0..2:
  ifElifElse mode
  ifNoElse mode
  elseAfterVoid mode
  caseBranches mode
  multiStmtBranch mode
  withDestructor mode
whenBlock()
try:
  raisingDefault 1
  echo "not raised"
except:
  echo "raised"
if true: f(g())
echo "counter ", counter
