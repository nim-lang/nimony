import std / syncio

template inspect(body: typed): untyped {.plugin: "deps/mexpressionviews".}

type
  Base = object of RootObj
    value: int
  Child = object of Base
    extra: int
  Callable = object
    fn: proc (x: int): int {.nimcall.}

proc zero(): int = 0
proc one(x: int): int = x + 1
proc two(x, y: int): int = x + y

let child = Child(value: 10, extra: 20)
let callable = Callable(fn: one)
echo inspect(zero())
echo inspect(one(child.value))
echo inspect(two(child.value, one(child.extra)))
echo inspect(callable.fn(4))
