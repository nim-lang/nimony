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

inspect:
  proc exercise(start: int): float =
    var x = start
    let y = float(x) + 1.5
    while x < 5:
      x = x + 1
    if x == 5 and y != 0.0:
      if y < 5.0:
        result = y * 2.0
      else:
        result = -y
    elif x <= 0:
      result = 0.0
    else:
      result = float(x) / 2.0
    if result < 0.0:
      result = -result

  echo exercise(3)
  echo exercise(0)
  echo exercise(6)
