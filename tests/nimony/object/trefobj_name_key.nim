# `Foo = ref object` names its object `Foo.Obj`; the unrelated `FooObj`
# must still get its own ref type (nimony-bugs-llms#347).
import std/syncio

type
  Foo = ref object
    x: int8
  FooObj = object
    a: int8
    y: int
  Bar = ref FooObj

let a = Foo(x: 1)
let b = Bar(a: 3, y: 1234567)
echo a.x, " ", b.a, " ", b.y
