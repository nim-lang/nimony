import std/syncio

const
  x = "abc"

const s: cstring = "135"

proc foo =
  var m = "12432"
  var v: string = x

  var n = "123" & x & m
  echo v
  echo n

  var m1 = s

foo()