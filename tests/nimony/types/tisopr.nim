import std/[assertions]

proc isInt[T](x: T): string =
  when T is int:
    result = "yes"
  else:
    result = "no"
  when x is int:
    result.add("yes")
  else:
    result.add("no")

proc isIntUntyped[T](x: T): string {.untyped.} =
  when T is int:
    result = "yes"
  else:
    result = "no"
  when x is int:
    result.add("yes")
  else:
    result.add("no")

const x = int is int
assert x
assert float is float
assert not (float is string)
assert isInt(1) == "yesyes"
assert isInt(1.0) == "nono"
assert isInt(1'i8) == "nono"
assert isIntUntyped(1) == "yesyes"
assert isIntUntyped(1.0) == "nono"
assert isIntUntyped(1'i8) == "nono"

assert not (8'i8 is int32)
assert 8'i8 is int8

type Foo[T1,T2] = object
type Bar[T1,T2] = object

assert Foo[int, int] is Foo[int, int]
assert Foo[int, float] is Foo[int, float]
assert Foo[float, float] is Foo[float, float]
assert Foo[int, float] isnot Foo[float, float]
assert Foo[int, int] isnot Bar[int, int]


type
  Person = tuple[name: string, age: int] # type representing a person:
                                         # it consists of a name and an age.
var person: Person
person = (name: "Peter", age: 30)
assert person.name == "Peter"
# the same, but less readable:
person = ("Peter", 30)
assert person[0] == "Peter"
assert Person is (string, int)
assert (string, int) is Person
