{.feature: "staticContracts".}

import std/assertions

type
  SortOrder* = enum
    Descending, Ascending

func `*`*(x: int; order: SortOrder): int {.inline.} =
  ## Flips the sign of `x` if `order == Descending`.
  ## If `order == Ascending` then `x` is returned.
  ##
  ## `x` is supposed to be the result of a comparator, i.e.
  ## | `< 0` for *less than*,
  ## | `== 0` for *equal*,
  ## | `> 0` for *greater than*.
  runnableExamples:
    assert -123 * Descending == 123
    assert 123 * Descending == -123
    assert -123 * Ascending == -123
    assert 123 * Ascending == 123
  var y = order.ord - 1
  result = (x xor y) - y

proc mergeAlt[T](a, b: var openArray[T];
                 lo, m, hi: int;
                 cmp: proc (x, y: T): int;
                 order: SortOrder) {.requires: 0 <= lo and lo <= m and m < hi and hi < a.len.} =
  # Optimization: If max(left) <= min(right) there is nothing to do!
  # 1 2 3 4 ## 5 6 7 8
  # -> O(n) for sorted arrays.
  # On random data this saves up to 40% of mergeAlt calls.

  # all values in `b` is uninitialized
  # don't call destructor to `b[i]`
  if cmp(a[m], a[m+1]) * order <= 0: return
  var j = lo
  # move a[j..m] into b:
  assert j <= m
  #when onlySafeCode:
  when true:
    var bb = 0
    while j <= m:
      # `b` holds the left run `a[lo..m]`, and `bb` counts it from 0 as `j` runs
      # from `lo` (`sort` sizes `b` for the longest left run).
      {.assume: bb < b.len.}
      swap b[bb], a[j]
      inc(bb)
      inc(j)
  else:
    copyMem(addr(b[0]), addr(a[j]), sizeof(T)*(m-j+1))
    j = m+1
  var i = 0
  var k = lo
  # copy proper element back:
  while k < j and j <= hi:
    # `i` indexes the part of the left run not yet copied back: `i < j - k`.
    {.assume: 0 <= i and i < b.len.}
    if cmp(b[i], a[j]) * order <= 0:
      swap a[k], b[i]
      inc(i)
    else:
      swap a[k], a[j]
      inc(j)
    inc(k)
  # copy rest of b:
  # The loop above advances `j` only while `j <= hi`.
  {.assume: j <= hi + 1.}
  #when onlySafeCode:
  when true:
    while k < j:
      {.assume: 0 <= i and i < b.len.}
      swap a[k], b[i]
      inc(k)
      inc(i)
  else:
    if k < j: copyMem(addr(a[k]), addr(b[i]), sizeof(T)*(j-k))

  # all uninitialized values moved from `b` to `a` returned to `b`

proc sort*[T](a: var openArray[T];
              cmp: proc (x, y: T): int;
              order = SortOrder.Ascending) {.nodestroy.} =
  ## Default Nim sort (an implementation of merge sort). The sorting
  ## is guaranteed to be stable (that is, equal elements stay in the same order)
  ## and the worst case is guaranteed to be O(n log n).
  ## Sorts by `cmp` in the specified `order`.
  ##
  ## The current implementation uses an iterative
  ## mergesort to achieve this. It uses a temporary sequence of
  ## length `a.len div 2`. If you do not wish to provide your own
  ## `cmp`, you may use `system.cmp` or instead call the overloaded
  ## version of `sort`, which uses `system.cmp`.
  ##
  ##   ```nim
  ##   sort(myIntArray, system.cmp[int])
  ##   # do not use cmp[string] here as we want to use the specialized
  ##   # overload:
  ##   sort(myStrArray, system.cmp)
  ##   ```
  ##
  ## You can inline adhoc comparison procs with the `do notation
  ## <manual.html#procedures-do-notation>`_. Example:
  ##
  ##   ```nim
  ##   people.sort do (x, y: Person) -> int:
  ##     result = cmp(x.surname, y.surname)
  ##     if result == 0:
  ##       result = cmp(x.name, y.name)
  ##   ```
  ##
  ## **See also:**
  ## * `sort proc<#sort,openArray[T]>`_
  ## * `sorted proc<#sorted,openArray[T],proc(T,T)>`_ sorted by `cmp` in the specified order
  ## * `sorted proc<#sorted,openArray[T]>`_
  ## * `sortedByIt template<#sortedByIt.t,untyped,untyped>`_
  runnableExamples:
    var d = ["boo", "fo", "barr", "qux"]
    proc myCmp(x, y: string): int =
      if x.len() > y.len() or x.len() == y.len(): 1
      else: -1
    sort(d, myCmp)
    assert d == ["fo", "qux", "boo", "barr"]

  var n = a.len
  # len < 2 needs no work — and the temp buffer would be empty, making the
  # manual `dealloc b.rawData` below free a bogus pointer (a SIGSEGV on some
  # backends, a silent heap-header scribble on others).
  if n < 2: return
  var b = newSeqUninit[T](n div 2)
  var s = 1
  while s < n:
    var m = n-1-s
    while m >= 0:
      let lo = max(m-s+1, 0)
      let hi = m+s
      # The strides of the iterative merge: `m` starts at `n-1-s` and falls by
      # `2*s`, so the right run `m+1..m+s` never passes the end.
      {.assume: 0 <= lo and lo <= m and m < hi and hi < a.len.}
      mergeAlt(a, b, lo, m, hi, cmp, order)
      dec(m, s*2)
    s = s*2

  # dealloc `b` without calling `=destroy` as all elements of `b` is uninitialized
  dealloc b.rawData

proc sorted*[T](a: openArray[T]; cmp: proc(x, y: T): int;
                order = SortOrder.Ascending): seq[T] =
  ## Returns `a` sorted by `cmp` in the specified `order`.
  ##
  ## **See also:**
  ## * `sort func<#sort,openArray[T],proc(T,T)>`_
  ## * `sort proc<#sort,openArray[T]>`_
  ## * `sortedByIt template<#sortedByIt.t,untyped,untyped>`_
  runnableExamples:
    let
      a = [2, 3, 1, 5, 4]
      b = sorted(a, system.cmp[int])
      c = sorted(a, system.cmp[int], Descending)
      d = sorted(["adam", "dande", "brian", "cat"], system.cmp[string])
    assert b == @[1, 2, 3, 4, 5]
    assert c == @[5, 4, 3, 2, 1]
    assert d == @["adam", "brian", "cat", "dande"]
  result = @a
  sort(result, cmp, order)

proc isSorted*[T](a: openArray[T];
                  cmp: proc(x, y: T): int;
                  order = SortOrder.Ascending): bool =
  ## Checks to see whether `a` is already sorted in `order`
  ## using `cmp` for the comparison. The parameters are identical
  ## to `sort`. Requires O(n) time.
  ##
  ## **See also:**
  ## * `isSorted proc<#isSorted,openArray[T]>`_
  runnableExamples:
    let
      a = [2, 3, 1, 5, 4]
      b = [1, 2, 3, 4, 5]
      c = [5, 4, 3, 2, 1]
      d = ["adam", "brian", "cat", "dande"]
      e = ["adam", "dande", "brian", "cat"]
    assert isSorted(a) == false
    assert isSorted(b) == true
    assert isSorted(c) == false
    assert isSorted(c, Descending) == true
    assert isSorted(d) == true
    assert isSorted(e) == false
  result = true
  for i in 0..<len(a)-1:
    if cmp(a[i], a[i+1]) * order > 0:
      return false

proc sort*[T: Comparable](a: var openArray[T]; order = SortOrder.Ascending) =
  ## Shortcut version of `sort` that uses `system.cmp[T]` as the comparison
  ## function.
  sort(a, proc (x, y: T): int = cmp(x, y), order)

proc lowerBound*[T: Comparable](a: openArray[T]; key: T): int =
  ## Returns the index of the first element of the sorted `a` that is not
  ## less than `key` (`a.len` if there is none): where `key` would be inserted
  ## to keep `a` sorted, before any elements equal to it. O(log n).
  runnableExamples:
    assert lowerBound([1, 2, 2, 4], 2) == 1
    assert lowerBound([1, 2, 2, 4], 5) == 4
  result = 0
  var count = a.len
  while count > 0:
    let step = count div 2
    let pos = result + step
    if a[pos] < key:
      result = pos + 1
      count -= step + 1
    else:
      count = step

proc upperBound*[T: Comparable](a: openArray[T]; key: T): int =
  ## Returns the index of the first element of the sorted `a` that is greater
  ## than `key` (`a.len` if there is none): where `key` would be inserted to
  ## keep `a` sorted, after any elements equal to it. O(log n).
  runnableExamples:
    assert upperBound([1, 2, 2, 4], 2) == 3
    assert upperBound([1, 2, 2, 4], 0) == 0
  result = 0
  var count = a.len
  while count > 0:
    let step = count div 2
    let pos = result + step
    if not (key < a[pos]):
      result = pos + 1
      count -= step + 1
    else:
      count = step
