# Test: borrow checking must work for openArray. See issue #1858.
#
# An `openArray` is a `.view` type: `toOpenArray` stores a raw pointer into the
# source container. While such a view is alive the source must not be mutated,
# and it must not be passed as a `var` argument next to the view.
#
# The converters in `lib/std/system/openarrays.nim` build the view out of a raw
# pointer, which is where any path the borrow checker could follow ends, so the
# `establishesBorrow` pragma on the converter is what makes these errors fire.
# See issue #1858 and `topenarray_borrow.nim` for the cases that must still pass.

proc grow(s: var seq[int]) =
  s.add 99

proc mutate(a: var array[3, int]) =
  a[0] = 42

# 1. `add` can reallocate the seq's payload; `view` dangles afterwards.
proc testSeqRealloc =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  grow(s)
  let x = view[0]
  discard x

testSeqRealloc()

# 2. Mutating the array through a `var` parameter while a view is alive.
proc testArrayMutate =
  var a = [1, 2, 3]
  let view = toOpenArray(a)
  mutate(a)
  let x = view[0]
  discard x

testArrayMutate()

# 3. The same container passed as a view and as a `var` argument in one call.
proc appendFirst(v: openArray[int]; s: var seq[int]) =
  s.add v[0]

proc testAliasInCall =
  var s = @[1, 2, 3]
  appendFirst(s, s)

testAliasInCall()

# 4. A `var` parameter forwarded to another `var` parameter is a location
# already, so it takes no `(haddr …)`; the mutation must be caught all the same.
proc testForwardedVar(s: var string) =
  let p = toOpenArray(s)
  s.setLen(1)
  if p.len > 0: discard p[0]

var fwd = "abc"
testForwardedVar(fwd)

# 5. Iterating a field of a `var` parameter while passing the parameter on as
# `var`: the callee may change the container being iterated.
type
  Holder = object
    items: seq[int]

proc touch(h: var Holder; x: int) =
  h.items.add x

proc testLoopOverForwarded(h: var Holder) =
  for x in h.items:
    touch(h, x)

var holder = Holder(items: @[1, 2])
testLoopOverForwarded(holder)

# 6. A `var T` result is derived from the call's first argument, so iterating
# it borrows that argument.
proc mitemsOf(h: var Holder): var seq[int] = h.items

proc testVarResult(h: var Holder) =
  for x in mitems(mitemsOf(h)):
    touch(h, x)

testVarResult(holder)

# 7. A fresh call result lives in a temporary that dies with the scope that
# evaluated it: a view of it must not be returned ...
proc freshSeq(n: int): seq[int] =
  result = @[]
  for i in 0 ..< n: result.add i

proc viewOfTemp(): openArray[int] =
  result = toOpenArray(freshSeq(3))

discard viewOfTemp()

# 8. ... nor outlive the scope it was created in.
proc tempEscapesScope(b: bool) =
  var g = toOpenArray(freshSeq(1))
  if b:
    g = toOpenArray(freshSeq(3))
  if g.len > 0: discard g[0]

tempEscapesScope(true)

# 9. `result` is never killed, but the borrow it holds lasts until the proc
# returns: mutating the source afterwards hands the caller a dangling view.
proc resultThenMutate(s: var seq[int]): openArray[int] =
  result = toOpenArray(s)
  grow(s)

var src = @[1, 2, 3]
discard resultThenMutate(src)

# 10. `let a = b` for a view `b` takes over `b`'s borrow; rebinding `b` must not
# release the only borrow on `s` while `a` still points into it.
proc copiedViewRebind =
  var s = @[1, 2, 3]
  var r = @[4, 5]
  var b = toOpenArray(s)
  let a = b
  b = toOpenArray(r)
  grow(s)
  discard a[0]

copiedViewRebind()

# 11. A reslice of a view assigned back to itself keeps the original borrow:
# `a` must not end up borrowing from `a`.
proc selfReslice =
  var s = @[1, 2, 3]
  var a = toOpenArray(s)
  a = toOpenArray(a, 1, 2)
  grow(s)
  discard a[0]

selfReslice()

# 12. A location passed to a `sink` parameter is not to be used afterwards, and
# a live view of it is such a use: the callee may free the payload.
proc consume(s: sink seq[int]): int = s.len

proc viewReadAfterSink =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  discard consume(s)
  discard view[0]

viewReadAfterSink()

# 13. Only a use of a view after the mutation makes the mutation an error. A
# use on the next iteration is after it ...
proc readOnNextIteration =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  for i in 0 ..< 2:
    discard view[0]
    grow(s)

readOnNextIteration()

# 14. ... and so is the next step of an iterator over the view ...
iterator values(v: openArray[int]): int =
  for i in 0 ..< v.len: yield v[i]

proc readByIteratorStep =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  for x in values(view):
    grow(s)

readByIteratorStep()

# 15. ... a use after an `if` that mutates in one arm ...
proc readAfterBranch(c: bool) =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  if c:
    grow(s)
  discard view[0]

readAfterBranch(true)

# 16. ... a use in a handler that a raise before the rebinding reaches ...
proc mayFail(c: bool) {.raises.} =
  if c: raise SyntaxError

proc readInHandler(c: bool) {.raises.} =
  var s = @[1, 2, 3]
  var r = @[4]
  var view = toOpenArray(s)
  try:
    grow(s)
    mayFail(c)
    view = toOpenArray(r)
  except:
    discard view[0]

readInHandler(true)

# 17. ... a temp holding an element, read by the call after the mutation ...
proc add2(a, b: int): int = a + b

proc readThroughElementTemp =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  discard add2(view[0], (grow(s); 1))

readThroughElementTemp()

# 18. ... and a view derived from the view.
proc readDerivedView =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  let w = toOpenArray(view, 0, 1)
  grow(s)
  discard w[0]

readDerivedView()

# 19. A value that carries the view holds what the view borrows. A `for`
# binder holds what the iterator is handed, which steps on with the
# container ...
proc mutateUnderBinder =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  for x in view:
    grow(s)
    discard x
    break

mutateUnderBinder()

# 20. ... a view stored in an object ...
type
  Holds {.view.} = object
    v: openArray[int]

proc storedInObject =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  let o = Holds(v: view)
  grow(s)
  discard o.v[0]

storedInObject()

# 21. ... or in a field ...
proc storedInField =
  var s = @[1, 2, 3]
  var t = @[4]
  var o = Holds(v: toOpenArray(t))
  let view = toOpenArray(s)
  o.v = view
  grow(s)
  discard o.v[0]

storedInField()

# 22. ... a view a `func` returns ...
func mk(v: openArray[int]): openArray[int] = toOpenArray(v, 0, 1)

proc heldByUntrackedLocal =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  let w = mk(view)
  grow(s)
  discard w[0]

heldByUntrackedLocal()

# 23. ... yielded by an iterator ...
iterator whole(v: openArray[int]): openArray[int] =
  yield v

proc yieldedView =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  for w in whole(view):
    grow(s)
    discard w[0]
    break

yieldedView()

# 24. ... returned inside a value that is no view ...
type
  Plain = object
    v: openArray[int]

func wrap(v: openArray[int]): Plain = Plain(v: v)

proc wrappedInResult =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  let p = wrap(view)
  grow(s)
  discard p.v[0]

wrappedInResult()

# 25. ... or passed next to the `var` argument that is mutated: the callee
# may read it after the mutation.
type
  Big = object
    a, b, c, d: int

func firstThenGrow(v: openArray[int]; s: var seq[int]): int =
  s.add 1
  result = v[0]

func bigThenGrow(b: Big; s: var seq[Big]): int =
  s.add Big()
  result = b.a

proc passedNextToMutation =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  discard firstThenGrow(view, s)
  var t = @[Big(a: 1)]
  let tv = toOpenArray(t)
  discard bigThenGrow(tv[0], t)

passedNextToMutation()

# 26. A view stored into an existing local through a call is held by it.
func wrapV(v: openArray[int]): Holds = Holds(v: v)

proc storedThroughCall =
  var s = @[1, 2, 3]
  var t = @[4]
  var o = wrapV(toOpenArray(t))
  let view = toOpenArray(s)
  o = wrapV(view)
  grow(s)
  discard o.v[0]

storedThroughCall()

# 27. A `defer` or `fin` runs on every way out, an early `return` too, ...
proc readInDeferAfterReturn(c: bool) =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  defer: discard view[0]
  if c:
    grow(s)
    return
  discard s.len

readInDeferAfterReturn(true)

# 28. ... and after a handler that mutated and rebound the view.
proc readInFinAfterHandler(c: bool) {.raises.} =
  var s = @[1, 2, 3]
  var r = @[4]
  var view = toOpenArray(s)
  try:
    mayFail(true)
  except:
    grow(s)
    mayFail(c)
    view = toOpenArray(r)
  finally:
    discard view[0]

readInFinAfterHandler(true)

# 29. A value holding the view that a hoisted temp passes on holds it as well.
type
  Plain2 = object
    v: openArray[int]

func wrap2(v: openArray[int]): Plain2 = Plain2(v: v)
func id2(p: Plain2): Plain2 = p

proc heldThroughTemp =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  let q = id2(wrap2(view))
  grow(s)
  discard q.v[0]

heldThroughTemp()

# 30. A view stored by a `func` through a `var` parameter, which is passed
# without an address when its type is a view, is held by that argument.
func setView(h: var Holds; v: openArray[int]) = h.v = v

proc storedThroughVarParam =
  var s = @[1, 2, 3]
  var t = @[4]
  var h = Holds(v: toOpenArray(t))
  let view = toOpenArray(s)
  setView(h, view)
  grow(s)
  discard h.v[0]

storedThroughVarParam()

# 31. Binding a view anew while a view derived from it is used afterwards
# mutates what that view borrows.
proc derivedOutlivesRebinding =
  var s = @[1, 2, 3]
  var t = @[4]
  var view = toOpenArray(s)
  let w = toOpenArray(view, 0, 1)
  view = toOpenArray(t)
  grow(s)
  discard w[0]

derivedOutlivesRebinding()

# 32. A view in an object in a `seq` is held by the `seq`: its elements lie
# behind a pointer.
proc heldInSeq =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  let ps = @[Plain(v: view)]
  grow(s)
  discard ps[0].v[0]

heldInSeq()

# 33. A view stored through a `var` loop variable lands in the container the
# loop runs over ...
proc storedThroughMitems =
  var s = @[1, 2, 3]
  var t = @[4]
  var hs = [Plain(v: toOpenArray(t))]
  let view = toOpenArray(s)
  for h in mitems(hs):
    h.v = view
  grow(s)
  discard hs[0].v[0]

storedThroughMitems()

# 34. ... with `mpairs` as well.
proc storedThroughMpairs =
  var s = @[1, 2, 3]
  var t = @[4]
  var hs = [Plain(v: toOpenArray(t))]
  let view = toOpenArray(s)
  for i, h in mpairs(hs):
    h.v = view
  grow(s)
  discard hs[0].v[0]

storedThroughMpairs()

# 35. A store through a `var` view lands in what the view was taken from.
func putFirst(a: var openArray[(Holds, int)]; v: openArray[int]) =
  a[0][0].v = v

proc storedThroughVarView =
  var s = @[1, 2, 3]
  var t = @[4]
  var arr = [(Holds(v: toOpenArray(t)), 1)]
  let view = toOpenArray(s)
  var w = toOpenArray(arr)
  putFirst(w, view)
  grow(s)
  discard arr[0][0].v[0]

storedThroughVarView()

# 36. A borrow a holder takes inside a loop is held where the loop stops ...
proc heldAfterLoop =
  var s = @[1, 2, 3]
  var t = @[4]
  let view = toOpenArray(s)
  var o = Plain(v: toOpenArray(t))
  var i = 0
  while i < 1:
    o.v = view
    inc i
  s = @[5]
  discard o.v[0]

heldAfterLoop()

# 37. ... and in the iterations after the one that took it.
proc heldInNextIteration =
  var s = @[1, 2, 3]
  var t = @[4]
  let view = toOpenArray(s)
  var o = Plain(v: toOpenArray(t))
  var i = 0
  while true:
    if i == 1:
      s = @[5]
      discard o.v[0]
      break
    o.v = view
    inc i

heldInNextIteration()

# 38. A view of an element of a view points where that view does.
proc viewOfElement =
  var s = @[@[1, 2, 3]]
  let v = toOpenArray(s)
  let w = toOpenArray(v[0], 0, 1)
  s = @[@[5]]
  discard w[0]

viewOfElement()

# 39. `swap` stores each `var` argument's value in the other.
proc swappedHolders =
  var s = @[1, 2, 3]
  var t = @[4]
  let v = toOpenArray(s)
  var a = (Holds(v: v), 1)
  var b = (Holds(v: toOpenArray(t)), 2)
  swap(a, b)
  s = @[5]
  discard b[0].v[0]

swappedHolders()

# 40. A `var` loop variable of a `.view` type is written without a
# dereference; the store lands in the container.
proc storedThroughViewBinder =
  var s = @[1, 2, 3]
  var t = @[4]
  var hs = [Holds(v: toOpenArray(t))]
  let view = toOpenArray(s)
  for h in mitems(hs):
    h.v = view
  s = @[5]
  discard hs[0].v[0]

storedThroughViewBinder()

# 41. A `var` or `out` parameter holds what is stored in it to the end of the
# routine: the caller reads it afterwards.
proc fillField(dst: var Plain; s: var seq[int]) =
  let v = toOpenArray(s)
  dst.v = v
  s = @[5]

proc fillSeq(dst: var seq[Plain]; s: var seq[int]) =
  let v = toOpenArray(s)
  dst.add Plain(v: v)
  s = @[5]

proc fillOut(dst: out Plain; s: var seq[int]) =
  let v = toOpenArray(s)
  dst = Plain(v: v)
  s = @[5]

proc fillTuple(o: var (openArray[int], int); s: var seq[int]) =
  let v = toOpenArray(s)
  o[0] = v
  s = @[5]

proc fillThroughMitems(dst: var seq[Plain]; s: var seq[int]) =
  let v = toOpenArray(s)
  for h in mitems(dst):
    h.v = v
  s = @[5]

# 42. A store behind a `ref` is held by the routine to its end: an alias of
# the `ref` reads it.
type
  RefHolds = ref Holds

proc storedBehindRef =
  var s = @[1, 2, 3]
  var t = @[9]
  let r = RefHolds(v: toOpenArray(t))
  let r2 = r
  let v = toOpenArray(s)
  r.v = v
  s = @[5]
  discard r2.v[0]

storedBehindRef()

proc storedBehindRefParam(r: RefHolds; s: var seq[int]) =
  let v = toOpenArray(s)
  r.v = v
  s = @[5]

# 43. A view nested nine constructors deep is held as well.
type
  N0 {.view.} = object
    a: openArray[int]
  N1 {.view.} = object
    a: N0
  N2 {.view.} = object
    a: N1
  N3 {.view.} = object
    a: N2
  N4 {.view.} = object
    a: N3
  N5 {.view.} = object
    a: N4
  N6 {.view.} = object
    a: N5
  N7 {.view.} = object
    a: N6
  N8 {.view.} = object
    a: N7

proc nestedDeep =
  var s = @[1, 2, 3]
  let v = toOpenArray(s)
  let w = N8(a: N7(a: N6(a: N5(a: N4(a: N3(a: N2(a: N1(a: N0(a: v)))))))))
  s = @[5]
  discard w.a.a.a.a.a.a.a.a.a[0]

nestedDeep()

# 44. A holder that borrows a part of its own storage holds that borrow:
# the part must not change while the view in it is used.
type
  OwnsAndViews {.view.} = object
    f: openArray[int]
    own: seq[int]

proc selfBorrow =
  var t = @[9]
  var m = OwnsAndViews(f: toOpenArray(t), own: @[1, 2, 3])
  let v = toOpenArray(m.own)
  m.f = v
  m.own = @[4]
  discard m.f[0]

selfBorrow()

# 45. Only a view result borrows from the first argument alone: any other
# value may hold a copy of any argument.
proc heldByCopyOfLaterArgument =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  let ps = newSeqOf(1, Plain(v: view))
  s = @[5]
  discard ps[0].v[0]

heldByCopyOfLaterArgument()
