# issue #2944: the prover substitutes a routine's parameter by its argument; a
# field that shares the parameter's name must keep its own selector.

import std/syncio
import deps / mparamfield

type
  Big = object
    a, b: int
  Nest = object
    b: Big
  Deep = object
    b: Nest
  RBig = ref object
    a, b: int
  PBig = ptr Big
  Outer = object
    b: Big
  G[T] = object
    a, b: int
    c: T

proc sum(b: Big): int = b.a + b.b
proc plusOne(b: Big): int = b.b + 1
proc onePlus(b: Big): int = 1 + b.b
proc sumA(a: Big): int = a.a + a.b
proc twiceA(b: Big): int = b.a + b.a
proc cmp(b: Big): bool = b.b > b.a
proc plain(b: Big): int = b.b
proc refSum(b: RBig): int = b.a + b.b
proc ptrSum(b: PBig): int = b.a + b.b
proc nested(b: Deep): int = b.b.b.b + 1
proc tup(b: tuple[a, b: int]): int = b.a + b.b
proc ctor(b: int): int = Big(a: 2, b: b).b + 1
proc req(b: Big; i: int): int {.requires: i < b.b.} = b.a + i
proc ens(b: Big): int {.ensures: result == b.b.} = b.b
proc gen[T](b: G[T]): int = b.a + b.b
proc caller(b: Outer): int = sum(b.b) + 1
template tsum(x: Big): int = x.a + x.b

var x = Big(a: 1, b: 2)
let r = RBig(a: 3, b: 4)
echo sum(Big(a: 1, b: 2)), " ", sum(x)
echo plusOne(Big(a: 1, b: 2)), " ", onePlus(x)
echo sumA(Big(a: 1, b: 2)), " ", twiceA(Big(a: 1, b: 2))
echo cmp(Big(a: 1, b: 2)), " ", plain(Big(a: 1, b: 2))
echo refSum(r), " ", refSum(RBig(a: 5, b: 6)), " ", ptrSum(addr x)
echo nested(Deep(b: Nest(b: Big(a: 1, b: 7))))
echo tup((a: 1, b: 2)), " ", ctor(5), " ", ctor(x.b)
echo req(Big(a: 1, b: 9), 3), " ", req(x, 1), " ", ens(Big(a: 1, b: 8))
echo gen(G[float](a: 1, b: 2, c: 0.5)), " ", caller(Outer(b: Big(a: 4, b: 5)))
echo tsum(Big(a: 1, b: 2)), " ", total(Pair(a: 10, b: 20))

# issue #2994: two calls of an imported accessor with a `.requires`, one
# applied to a field named like its parameter
type Selection = object
  cursor: Cur
func reselect(before: Selection; cursor: Cur): bool =
  if not before.cursor.initialized or not cursor.initialized:
    return false
  before.cursor.lastStamp != cursor.lastStamp
echo reselect(Selection(cursor: initCur(1)), initCur(2)), " ",
  reselect(Selection(cursor: initCur(3)), initCur(3)), " ",
  reselect(Selection(), initCur(3))
