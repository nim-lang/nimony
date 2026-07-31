# Test: a view must not outlive what it borrows from. See issue #1858.
#
# `topenarray_borrow_errors.nim` covers mutation of a still-borrowed source.
# This is the other half: the source dies while the view is still reachable,
# which no later mutation check can catch because by then the owner is gone.

var g: openArray[int]

# 1. Escape into a global.
proc escapeToGlobal =
  var a = [1, 2, 3]
  g = toOpenArray(a)

escapeToGlobal()

# 2. Escape into a global through a named local view.
proc escapeViaLocalView =
  var a = [1, 2, 3]
  let v = toOpenArray(a)
  g = v

escapeViaLocalView()

# 3. Escape through `result`.
proc escapeViaResult: openArray[int] =
  var a = [1, 2, 3]
  result = toOpenArray(a)

discard escapeViaResult()

# 4. Escape through an explicit `return`, which hands the value back directly
#    instead of storing it into `result` first.
proc escapeViaReturn: openArray[int] =
  var a = [1, 2, 3]
  return toOpenArray(a)

discard escapeViaReturn()

# 5. Escape through a `var` parameter, which writes to a caller-owned location.
proc escapeViaVarParam(o: var openArray[int]) =
  var a = [1, 2, 3]
  o = toOpenArray(a)

var sink: openArray[int]
escapeViaVarParam(sink)

# 6. Escape of a reborrow: the slice is no longer-lived than the view it slices.
proc escapeViaSlice: openArray[int] =
  var a = [1, 2, 3, 4]
  let whole = toOpenArray(a)
  result = toOpenArray(whole, 1, 2)

discard escapeViaSlice()

# 7. A local string's payload dies with the proc just like an array's.
proc escapeViaString: openArray[char] =
  var s = "hello"
  result = toOpenArray(s)

discard escapeViaString()

# 8. Escape of an inner scope into an enclosing one. NJ emits a `kill` per
#    scope, so the source is seen dying while the view still holds it.
proc escapeViaBlock =
  var outer: openArray[int]
  block:
    var a = [1, 2, 3]
    outer = toOpenArray(a)
  discard outer.len

escapeViaBlock()

# 9. Same, out of a loop body.
proc escapeViaLoop =
  var outer: openArray[int]
  for i in 0 ..< 2:
    var a = [1, 2, 3]
    outer = toOpenArray(a)
  discard outer.len

escapeViaLoop()
