# The untyped-generic control for twhenbranchscope: here `when` keeps the
# rules of the When statement, so a branch's declarations are visible after it.
import std/syncio

proc f[T](x: T) {.untyped.} =
  when T is int:
    var y = 1
  else:
    var y = "a"
  echo y

f(1)
f("s")
