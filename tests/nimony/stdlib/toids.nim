import std/[assertions, oids, syncio, times]

block:
  let x = genOid()
  assert ($x).len == 24

  var text = $x
  let y = parseOid(text.toCString)
  assert x == y
  assert $y == text
  assert hash(x) == hash(y)

block:
  let before = getTime().toUnix
  let x = genOid()
  let after = getTime().toUnix
  let created = generatedTime(x).toUnix
  assert created >= before and created <= after

block:
  let x = parseOid(cstring("5fc7f546ddbbc84800006aaf"))
  assert $x == "5fc7f546ddbbc84800006aaf"
  assert generatedTime(x) == fromUnix(0x5fc7f546'i64)
  assert parseOid(cstring("5FC7F546DDBBC84800006AAF")) == x

block:
  assert hexbyte('0') == 0
  assert hexbyte('9') == 9
  assert hexbyte('a') == 10
  assert hexbyte('F') == 15
  assert hexbyte('x') == 0
  assert hexbyte('5') != hexbyte('6')

block:
  let x = genOid()
  let y = genOid()
  assert x != y

echo "oids: ok"
