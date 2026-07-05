import std/syncio
import std/options

proc firstEven(xs: seq[int]): Option[int] =
  for x in xs:
    if x mod 2 == 0:
      return some(x)
  none[int]()

let a = firstEven(@[1, 3, 5, 8, 9])
echo a.isSome
echo a.get()
echo a.get(-1)

let b = firstEven(@[1, 3, 5])
echo b.isSome
echo b.isNone
echo b.get(-1)

let c = some("hello")
echo c.isSome
echo c.get()

var d = none[int]()
echo d.isNone
