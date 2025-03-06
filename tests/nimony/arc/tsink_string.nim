
import std / syncio

proc x(s: sink string) =
  let foo = ensureMove(s)
  echo foo

x("hi")
