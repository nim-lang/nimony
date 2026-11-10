# Two `var` arguments that index the same seq or openArray overlap unless both
# indexes are constants that differ, just as for an array (doc/language.md,
# "Call-site aliasing checks"). `f(s[i], s[j])` with `i == j` hands one
# location to both parameters.

proc fi(a, b: var int) =
  a = 1
  b = 2

type
  Big = object
    x: array[20, int]
  A = object
    s: seq[int]

proc gb(a: var Big; b: Big) = a = b
proc fg[T](a, b: var T) = a = b
template tfi(s, i, j: untyped) = fi(s[i], s[j])

proc g(a: var int; b: var int; arr: array[10, int]): int =
  result = 0
  if a >= 0 and a < 10:
    b = 1000000000
    result = arr[a]

var arr: array[10, int]
var gs = @[3, 4]
var gi = 0
var gj = 0
discard g(gs[gi], gs[gj], arr)   # global seq

proc main(i, j: int; oa: var openArray[int]) =
  var s = @[1, 2]
  fi(s[i], s[j])              # local seq
  fi(oa[i], oa[j])            # openArray
  var a = A(s: @[1, 2])
  fi(a.s[i], a.s[j])          # field of an object
  var m = @[@[1, 2], @[3, 4]]
  fi(m[i][0], m[j][0])        # two-dimensional
  fi(s[0], s[0])              # equal constants
  var bq = @[Big(), Big()]
  gb(bq[i], bq[j])            # `var` and by-pointer immutable, same seq
  gb(bq[0], bq[0])
  fg(s[i], s[j])              # generic routine
  tfi(s, i, j)                # template
  var ar = [3, 4, 5]
  fi(toOpenArray(ar, 1, 2)[0], ar[1])  # a view's element: the same location
  fi(toOpenArray(s, 1, 2)[0], s[1])

main(0, 0, gs)
