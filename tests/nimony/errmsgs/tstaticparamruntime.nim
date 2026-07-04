# A value ("static") generic parameter binds only compile-time-known values;
# a runtime variable is not a constant expression.
type
  Vec[N: static[int]] = object
    data: array[N, int]

var n = 3
var v: Vec[n]
