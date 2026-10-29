import std/[assertions, strutils, syncio]

type
  Color = enum
    red, green, blue

  Custom = object
    value: int

func `$`(x: Custom): string =
  "custom"

let nums = @[1, 2, 3]
let emptyNums: seq[int] = @[]
let names = @["a", "b"]
let charSeq = @['a', 'b']
let customSeq = @[Custom(value: 1)]
let arr = [4, 5]
var emptyArr: array[0, int]
let chars = {'a', 'b'}
var emptyChars: set[char]
let colors = {red, green, blue}
let sl = 1 .. 3
let strSl = "a" .. "c"
let mixedSl = 1 .. "c"
let named = (x: 1, y: "two")
let unnamed = (1, "two")
let single = (42,)

proc showOpen(x: openArray[int]): string =
  $x

assert $nums == "@[1, 2, 3]"
assert $emptyNums == "@[]"
assert $names == "@[\"a\", \"b\"]"
assert $charSeq == "@['a', 'b']"
assert $customSeq == "@[custom]"
assert $arr == "[4, 5]"
assert $emptyArr == "[]"
assert $chars == "{'a', 'b'}"
assert $emptyChars == "{}"
assert $colors == "{red, green, blue}"
assert $sl == "1 .. 3"
assert $strSl == "a .. c"
assert $mixedSl == "1 .. c"
assert $named == "(x: 1, y: \"two\")"
assert $unnamed == "(1, \"two\")"
assert $single == "(42,)"
assert $Custom(value: 1) == "custom"
assert showOpen([6, 7]) == "[6, 7]"
assert $(@[@[1], @[2]]) == "@[@[1], @[2]]"

echo $nums
echo $emptyNums
echo $names
echo $charSeq
echo $customSeq
echo $arr
echo $emptyArr
echo $chars
echo $emptyChars
echo $colors
echo $sl
echo $strSl
echo $mixedSl
echo $named
echo $unnamed
echo $single
echo $Custom(value: 1)
echo showOpen([6, 7])
echo $(@[@[1], @[2]])
