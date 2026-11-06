# `==` on seq and openArray compares elements with the element type's `==`;
# objects and tuples still compare field by field.
import std/syncio

type
  D = distinct string
  Obj = object
    a: int
    s: string
  Holder = object
    items: seq[int]

func `==`(a, b: D): bool {.borrow.}

proc eqOpen(a, b: openArray[int]): bool = a == b
proc eqGeneric[T: Equatable](a, b: seq[T]): bool = a == b

let xs = @[1, 2, 3]
var ys = @[1, 2]
ys.add 3
echo xs == ys          # equal contents in separate buffers
echo xs == @[1, 2, 4]
echo xs == @[1, 2]
echo @[D("x")] == @[D("x")]
echo @[D("x")] == @[D("y")]
echo @[Obj(a: 1, s: "p")] == @[Obj(a: 1, s: "p")]
echo @[Obj(a: 1, s: "p")] == @[Obj(a: 1, s: "q")]
echo @[(1, "a")] == @[(1, "a")]
echo @[@[1], @[2, 3]] == @[@[1], @[2, 3]]
echo eqOpen(xs, ys)
echo eqOpen(xs, [1, 2, 4])
echo Holder(items: @[1, 2]) == Holder(items: @[1, 2])
echo Holder(items: @[1, 2]) == Holder(items: @[2, 1])
echo Obj(a: 1, s: "p") == Obj(a: 1, s: "p")
echo Obj(a: 1, s: "p") == Obj(a: 2, s: "p")
echo (1, "a") == (1, "a")
echo (1, "a") == (1, "b")
echo eqGeneric(xs, ys)
echo eqGeneric(@[D("x")], @[D("y")])
