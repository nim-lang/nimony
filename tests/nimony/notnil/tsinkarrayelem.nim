# the element of a `sink array` parameter keeps its element type
import std/syncio
type
  O = object
    x: int
  R = ref O
proc first(a: sink array[2, R]): R = a[0]
proc at(a: sink array[2, R]; i: int): R = a[i]
echo first([R(x: 1), R(x: 2)]).x, " ", at([R(x: 3), R(x: 4)], 1).x
