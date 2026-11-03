# Aliases that reach themselves only through a nominal object are not cycles.
import std/syncio
type
  X = A
  A = ref Obj[int]
  Obj[T] = object
    v: T
    next: A
  Forward = object
    x: FN[int]
  FN[T] = ref FNObj[T]
  FNObj[T] = object
    val: T
    left: FN[T]
  L = ref LObj
  LObj = object
    v: int
    next: L

var x = X(v: 1, next: A(v: 2))
var f = Forward(x: FN[int](val: 3, left: FN[int](val: 4)))
var l = L(v: 5, next: L(v: 6))
echo x.next.v, " ", f.x.left.val, " ", l.next.v
