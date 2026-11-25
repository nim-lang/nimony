import std/syncio

type Parent = ref object of RootObj
type Child = ref object of Parent

method m(x: Parent) =
  echo "parent"

method m(x: Child) =
  echo "child"

proc main =
  var c = Child()
  if c != nil:
    procCall Parent(c).m()

main()

# A dispatched call casts the vtable slot to the method's type; a `.closure`
# parameter or result in it is the (fn, env) pair (#333).
type
  Fn = proc (x: int) {.closure.}
  Getter = proc (): int {.closure.}

method run(x: Parent; f: Fn) {.base.} = f(1)
method run(x: Child; f: Fn) = f(2)

method mk(x: Parent; k: int): Getter {.base.} =
  result = proc (): int {.closure.} = k
method mk(x: Child; k: int): Getter =
  result = proc (): int {.closure.} = k + 1

proc dispatchClosures =
  var total = 0
  let f: Fn = proc (x: int) {.closure.} = total += x
  let p: Parent = Child()
  p.run(f)
  echo total
  echo p.mk(4)()

dispatchClosures()

# An inline proc type is the same parameter type in every override, whatever
# its parameters are named (#366).
type Other = ref object of Parent

method runInline(x: Parent; f: proc (x: int) {.closure.}) {.base.} = f(1)
method runInline(x: Child; f: proc (x: int) {.closure.}) = f(20)
method runInline(x: Other; f: proc (y: int) {.closure.}) = f(300)

proc dispatchInline =
  var total = 0
  let f = proc (x: int) {.closure.} = total += x
  for p in [Parent(), Parent(Child()), Parent(Other())]: p.runInline(f)
  echo total

dispatchInline()
