# A `var T` or `lent T` result makes the first parameter pass-by-reference
# (#3187), so the location the routine returns is the caller's storage: a
# write through it reaches the caller and a read sees the caller's value,
# whatever the parameter's size.
import std/syncio

type
  O = object
    a: array[1, int]
  D = distinct array[1, int]
  Big = object
    a: array[8, int]

proc first(x: openArray[int]): var int = x[0]
proc firstC(x: openArray[char]): var char = x[0]
proc firstL(x: openArray[int]): lent int = x[0]

proc atSmall(a: array[1, int]): var int = first(a)
proc atField(o: O): var int = first(o.a)
proc atTuple(t: (array[1, int], int)): var int = first(t[0])
proc atDistinct(d: D): var int = first(array[1, int](d))
proc atString(s: string): var char = firstC(s)
proc direct(a: array[1, int]): var int = a[0]
proc directBig(b: Big): var int = b.a[7]
proc directInt(i: int): var int = (addr i)[]
proc atL(a: array[1, int]): lent int = firstL(a)
proc directL(a: array[1, int]): lent int = a[0]
proc atLS(s: seq[int]): lent int = firstL(s)
proc twice(a: array[1, int]): var int = direct(a)
proc generic[T](x: T): var T = x

var va = [0]
atSmall(va) = 5
echo va[0]
var vo = O()
atField(vo) = 5
echo vo.a[0]
var vt = ([0], 1)
atTuple(vt) = 5
echo vt[0][0]
var vd = D([0])
atDistinct(vd) = 5
echo array[1, int](vd)[0]
var s = "ab"
atString(s) = 'x'
echo s
direct(va) = 6
echo va[0]
var vb = Big()
directBig(vb) = 7
echo vb.a[7]
var vi = 1
directInt(vi) = 8
echo vi
twice(va) = 9
echo va[0]
generic(vi) = 10
echo vi

let a = [7]
echo atL(a)
echo directL(a)
echo atLS(@[11, 12])

# through a proc-typed value: the call through the pointer agrees
let pd: proc (a: array[1, int]): var int {.nimcall.} = direct
pd(va) = 12
echo va[0]
