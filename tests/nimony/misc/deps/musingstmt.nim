type
  MUsing* = object
    discard

using
  c: MUsing
  y: array[3, float]

proc mtest1*(c; y) =
  discard

proc mtest2*(c; y: array[3, float]) =
  discard

proc mtest3*(c = MUsing(); y; z: int) =
  discard
