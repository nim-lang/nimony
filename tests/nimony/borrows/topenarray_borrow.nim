# Test: `establishesBorrow` must not reject legitimate openArray usage.
# The companion error cases live in `topenarray_borrow_errors.nim`.

import std/assertions

proc sum(a: openArray[int]): int =
  result = 0
  for x in a: result += x

proc grow(s: var seq[int]; val: int) =
  s.add val

# A view that has died releases its borrow, so the source is mutable again:
proc testBorrowEndsWithView =
  var s = @[1, 2, 3]
  block:
    let view = toOpenArray(s)
    assert sum(view) == 6
  grow(s, 4)
  assert s.len == 4

testBorrowEndsWithView()

# A view built for the duration of one call borrows only for that call:
proc testCallScopedView =
  var s = @[1, 2, 3]
  assert sum(s) == 6
  grow(s, 4)
  assert sum(s) == 10

testCallScopedView()

# `x notin s` builds a temporary view for `contains`; it must not keep `s`
# borrowed for the rest of the enclosing proc.
proc testContainsThenMutate =
  var seen: seq[int] = @[]
  for candidate in [1, 2, 2, 3]:
    if candidate notin seen:
      seen.add candidate
  assert seen.len == 3

testContainsThenMutate()

# Slicing a view reborrows through it; the source stays readable.
proc testReborrow =
  var s = @[1, 2, 3, 4]
  let whole = toOpenArray(s)
  let part = toOpenArray(whole, 1, 2)
  assert sum(part) == 5
  assert s.len == 4

testReborrow()

# Disjoint containers are unaffected by each other's borrows:
proc testDisjoint =
  var a = @[1, 2]
  var b = @[3, 4]
  let view = toOpenArray(a)
  grow(b, 5)
  assert sum(view) == 3
  assert b.len == 3

testDisjoint()

# Strings and arrays go through their own converters:
proc countChars(s: openArray[char]): int = s.len

proc testStringAndArray =
  var str = "hello"
  assert countChars(str) == 5
  str.add '!'
  assert countChars(str) == 6
  var arr = [1, 2, 3]
  assert sum(arr) == 6
  arr[0] = 10
  assert sum(arr) == 15

testStringAndArray()

# --- lifetimes: a borrow may leave the proc when its source does too ---

var globalSeq = @[7, 8, 9]
var globalView: openArray[int]

# Borrowing from a parameter and handing the view back is fine: the caller owns
# the source and outlives the call.
proc fromParam(s: seq[int]): openArray[int] =
  result = toOpenArray(s)

proc testFromParam =
  var s = @[1, 2, 3]
  assert sum(fromParam(s)) == 6

testFromParam()

# A global source outlives everything, so the view may be stored in a global.
proc fromGlobal =
  globalView = toOpenArray(globalSeq)

proc testFromGlobal =
  fromGlobal()
  assert sum(globalView) == 24

testFromGlobal()

# Returning a plain value read through an iteration borrow copies it; only
# reference-like results can carry a borrow out of the proc.
proc firstOver(s: seq[int]): int =
  for x in s: return x
  return -1

proc testValueEscapesFreely =
  var s = @[5, 6]
  assert firstOver(s) == 5

testValueEscapesFreely()

# A view assigned in an inner scope is fine as long as its source lives in an
# enclosing one: the source is not among the symbols that scope kills.
proc testInnerScopeOuterSource =
  var s = @[1, 2, 3]
  var view: openArray[int]
  block:
    view = toOpenArray(s)
  assert sum(view) == 6

testInnerScopeOuterSource()

# Reassigning a view rebinds its borrow, so the previous source is free again.
proc testRebind =
  var a = @[1, 2]
  var b = @[3, 4]
  var view = toOpenArray(a)
  assert sum(view) == 3
  view = toOpenArray(b)
  grow(a, 9)            # 'a' is no longer borrowed
  assert sum(view) == 7
  assert a.len == 3

testRebind()
