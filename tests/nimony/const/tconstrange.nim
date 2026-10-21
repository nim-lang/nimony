# issue #2727: range-typed consts
import std/syncio
type
  D = distinct int
  R = range[0..10]
proc g(): range[0..10] = 4
proc g2(): R = 7
proc g3(): range['a'..'z'] = 'q'
const c = g()
const c2 = g2()
const c3 = g3()
const c4: range[0..10] = 4
const c5: range['a'..'z'] = 'c'
const c6: R = c2 + 1
var x: R = c
case x
of c4: echo "four"
else: echo "other"
let arr = [1, 2, 3, 4, 5]
echo c, " ", c2, " ", c3, " ", c4, " ", c5, " ", c6, " ", x, " ", arr[c]
