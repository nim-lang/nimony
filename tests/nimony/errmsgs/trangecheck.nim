# Range checking is delegated to the contracts+inferle engine: a value bound
# to a `range[lo..hi]` slot carries the obligation `lo <= value <= hi`, which is
# discharged statically. Whatever cannot be proven in range is rejected here —
# no runtime check is ever emitted.

block:
  var a: range[0..10] = 20

block:
  let b: range[0..10] = 99

block:
  discard (range[0..10])(50)

block:
  # A wider range is not a subtype of a narrower one, and the engine cannot
  # prove the value fits, so this is rejected (rather than checked at runtime).
  proc widen(e: range[0..20]) =
    var f: range[0..10] = e
    discard f
  widen(5)
