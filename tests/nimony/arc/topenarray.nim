import std/[syncio, assertions]

block:
  proc setPosition(params: openArray[string]) =
    for i in params.toOpenArray(0, params.len - 1):
      echo i

  let params = @["hi"]
  setPosition(params)

  let params2 = @["Nim"]
  for i in params2.toOpenArray(0, params2.len - 1):
    echo i

proc mutate(a: var string) =
  var v = a.toOpenArray(1, 3)
  v[0] = 'a'

var a = "Hello"
mutate(a)
assert a == "Hallo"

block:
  proc foo[T](arr: openArray[T], idx: int = arr.low): string =
    assert idx == 0
    result = $arr

  let bug = ["0", "c", "a"]
  let expected = """["0", "c", "a"]"""
  assert foo(bug) == expected

  let noBugConst = ["0", "c", "a"]
  assert foo(noBugConst) == expected

  let noBugSeq = @["0", "c", "a"]
  assert foo(noBugSeq) == expected

block:
  var v: array[1, byte]
  assert toOpenArray(v, 0, 0).len == 1
