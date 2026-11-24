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
