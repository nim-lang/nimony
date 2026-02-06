import std/assertions

proc emptyArray(arr: array[0, int]) = discard
proc tableIntInt(arr: array[1, (int, int)]) = discard
proc tableStrInt(arr: array[2, (string, int)]) = discard
proc tableIntTup(arr: array[3, (int, (int, int))]) = discard

emptyArray {:}
tableIntInt {1: 2}
tableStrInt {"a": 1, "b": 2}
tableIntTup {1: (2, 3), 2: (4, 5), 3: (6, 7)}

block:
  let m = {"key1": "value1", "key2", "key3": "value2"}
  assert m[0][0] == "key1"
  assert m[0][1] == "value1"
  assert m[1][0] == "key2"
  assert m[1][1] == "value2"
  assert m[2][0] == "key3"
  assert m[2][1] == "value2"
