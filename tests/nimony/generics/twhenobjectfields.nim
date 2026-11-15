import std/[assertions, syncio]

type
  Holder[T] = object
    count: int
    when T isnot void:
      values: seq[T]
      firstValue: T

proc addValue[T: not void](h: var Holder[T], value: T) =
  h.values.add value
  if h.values.len == 1:
    h.firstValue = value
  inc h.count

proc count[T](h: Holder[T]): int =
  h.count

proc first[T: not void](h: Holder[T]): T =
  h.firstValue

var ints = default(Holder[int])
ints.addValue 42
ints.addValue 7
assert ints.count == 2
assert ints.first == 42
assert ints.values == @[42, 7]

var empty = default(Holder[void])
empty.count = 3
assert empty.count == 3

type
  Choice[T] = object
    count: int
    when T is int:
      intPart: int
      textPart: string
    elif T is float:
      floatPart: float
      extraPart: int
    else:
      flag: bool

proc describe[T](c: Choice[T]): string =
  when T is int:
    $c.intPart & c.textPart
  elif T is float:
    $c.floatPart
  else:
    $c.flag

var intChoice = default(Choice[int])
intChoice.intPart = 3
intChoice.textPart = "x"
assert describe(intChoice) == "3x"

var floatChoice = default(Choice[float])
floatChoice.floatPart = 2.5
assert describe(floatChoice) == "2.5"

var boolChoice = default(Choice[bool])
boolChoice.flag = true
assert describe(boolChoice) == "true"

echo "generic when object fields: OK"
