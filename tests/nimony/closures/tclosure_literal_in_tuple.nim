# A tuple holding a proc literal types the field with the literal's proc
# type, as a tuple type's field types are checked as types.
import std/syncio

proc main() =
  var pre = 3
  let cl = proc (x: int): int {.closure.} = x + pre
  let t = (cl, 1)
  echo t[1], " ", t[0](4)
  let n = (f: cl, k: 2)
  echo n.k, " ", n.f(5)
  let a = [(cl, 3)]
  echo a[0][1], " ", a[0][0](6)
  # `addr` builds `ptr` of the literal's proc type
  var cl2 = cl
  let p = addr cl2
  var g = proc (n: int): int =
    var s = 0
    for i in 0 ..< n: s += i
    s
  let q = addr g
  echo p[](1), " ", q[](4)

main()
