import std/assertions

type
  Outer = object
    case a: bool
    of true:
      case b: bool
      of true:
        inner: int
      of false:
        nil
    of false:
      other: string

proc checkTrue() =
  var o = Outer(a: true, b: true, inner: 5)
  var fields: seq[string] = @[]
  for name, value in fieldPairs(o):
    fields.add name & "=" & $value
  assert fields == @["a=true", "b=true", "inner=5"]

proc checkFalse() =
  var o = Outer(a: false, other: "x")
  var fields: seq[string] = @[]
  for name, value in fieldPairs(o):
    fields.add name & "=" & $value
  assert fields == @["a=false", "other=x"]

checkTrue()
checkFalse()
