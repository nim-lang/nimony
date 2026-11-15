# An enum whose first value is not 0 has no holes: it is ordinal, so it is an
# array index type, and `succ`, `inc` and `for ... in` apply to it.
import std/syncio

type
  Month = enum
    mJan = 1, mFeb, mMar
  Big = enum
    b200 = 200, b201, b202
  Neg = enum
    nm1 = -1, n0, n1
  Named = enum
    first = (5, "First"), second, third

proc lastOf[I, T](a: array[I, T]): T = a[high(a)]

proc main =
  const Names: array[Month, string] = ["Jan", "Feb", "Mar"]
  echo Names[mJan], " ", Names[mFeb], " ", Names[mMar], " ", Names.len
  var names = default(array[Month, string])
  names[mFeb] = "Feb"
  echo names[mFeb], " ", names.len, " ", names[mJan].len

  var big = default(array[Big, int])
  big[b200] = 1
  big[b202] = 3
  echo big[b200], " ", big[b201], " ", big[b202], " ", big.len

  var neg = default(array[Neg, int])
  neg[nm1] = -1
  neg[n1] = 1
  echo neg[nm1], " ", neg[n0], " ", neg[n1]

  let named: array[Named, int] = [10, 20, 30]
  echo named[third], " ", $second

  echo lastOf(Names)
  for m in Month: echo ord(m), " ", Names[m]
  var m = mJan
  inc m
  echo m, " ", succ(m), " ", pred(m)

main()
