import std/[assertions, syncio]

type
  Color = enum
    red, green

  Custom = object
    value: int

func `$`(x: Custom): string = "custom" & $x.value

let x = 42
let s = "abc"
echo &("abc ", x, "\n")
assert &("x is ", x, '!') == "x is 42!"
assert &(s, " ", 1.5, " ", true, " ", green) == "abc 1.5 true green"
assert &(Custom(value: 7), "/", @[1, 2]) == "custom7/@[1, 2]"
assert &("only",) == "only"
assert &(name: "n", value: 3) == "n3"
echo &("done: ", s, ' ', x)

# `addFormatted` overrides `$` inside `&`.
type Person = object
  name: string

func `$`(p: Person): string = "Person(name: \"" & p.name & "\")"
func addFormatted(s: var string; p: Person) = s.add p.name

let p = Person(name: "Ann")
assert $p == "Person(name: \"Ann\")"
assert &("hello ", p, "!") == "hello Ann!"
assert &(-5'i8, " ", 200'u8, " ", high(int64), " ", 2.5'f32) == "-5 200 9223372036854775807 2.5"

proc greet[T: tuple](t: T): string = &t
assert greet(("hi ", p)) == "hi Ann"

# `&` is a template: the constructor's elements are evaluated once each, in
# order, without materializing the tuple.
proc next(c: var int): int =
  inc c
  c
var c = 0
assert &("A ", next(c), " B ", next(c)) == "A 1 B 2"
assert c == 2
