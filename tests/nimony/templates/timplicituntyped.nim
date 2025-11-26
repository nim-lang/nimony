import std/syncio

template withCopy(i: var int, body) =
  var i2 = i
  body
  i = i2

var n = 1

withCopy n:
  inc n
  echo n

echo n
