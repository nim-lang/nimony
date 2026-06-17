# seq implementation.

{.feature: "lenientnils".}

type
  seq*[T] = object ## Built-in growable sequence type.
    len: int
    data: ptr UncheckedArray[T]

func capInBytes[T](s: seq[T]): int {.inline.} =
  result = if s.data != nil: allocatedSize(s.data) else: 0

func `=destroy`*[T](s: seq[T]) =
  if s.data != nil:
    var i = 0
    while i < s.len:
      `=destroy`(s.data[i])
      inc i
    dealloc s.data

func destroyUninit*[T](s: var seq[T]) {.inline.} =
  ## Call this only if all elements of `s` are uninitialized
  if s.data != nil:
    dealloc s.data
    s.data = nil
    s.len = 0

func `=wasMoved`*[T](s: var seq[T]) {.inline.} =
  s.len = 0
  s.data = nil

func memSizeInBytes[T](size: int): int {.inline.} =
  {.keepOverflowFlag.}:
    result = size * sizeof(T)
    if overflowFlag():
      # When required memory size is overflowed, cause out of memory.
      result = high(int)

func newSeq*[T: HasDefault](size: int = 0): seq[T] {.nodestroy.} =
  ## Creates a new sequence of length `size` with default-initialized elements.
  if size == 0:
    result = seq[T](len: size, data: nil)
  else:
    let memSize = memSizeInBytes[T](size)
    result = seq[T](len: size, data: cast[ptr UncheckedArray[T]](alloc(memSize)))
    if result.data != nil:
      var i = 0
      while i < size:
        (result.data[i]) = default(T)
        inc i
    else:
      result.len = 0
      {.cast(noSideEffect).}:
        oomHandler memSize

func newSeqOf*[T](size: int; initValue: T): seq[T] {.nodestroy.} =
  ## Creates a new sequence of length `size` where every slot is `initValue`.
  if size == 0:
    result = seq[T](len: size, data: nil)
  else:
    let memSize = memSizeInBytes[T](size)
    result = seq[T](len: size, data: cast[ptr UncheckedArray[T]](alloc(memSize)))
    if result.data != nil:
      var i = 0
      while i < size:
        (result.data[i]) = `=dup`(initValue)
        inc i
    else:
      result.len = 0
      {.cast(noSideEffect).}:
        oomHandler memSize

func newSeqUninit*[T](size: int): seq[T] {.nodestroy, inline.} =
  ## Creates a new sequence of length `size` without initializing elements (unsafe unless you fill them).
  if size == 0:
    result = seq[T](len: size, data: nil)
  else:
    let memSize = memSizeInBytes[T](size)
    result = seq[T](len: size, data: cast[ptr UncheckedArray[T]](alloc(memSize)))
    if result.data != nil:
      discard "leave uninitialized"
    else:
      result.len = 0
      {.cast(noSideEffect).}:
        oomHandler memSize

func newSeqOfCap*[T](cap: int): seq[T] {.nodestroy, inline.} =
  if cap <= 0:
    result = seq[T](len: 0, data: nil)
  else:
    let memSize = memSizeInBytes[T](cap)
    result = seq[T](len: 0, data: cast[ptr UncheckedArray[T]](alloc(memSize)))
    if result.data == nil:
      {.cast(noSideEffect).}:
        oomHandler memSize

template default*[T](x: typedesc[seq[T]]): seq[T] = newSeqUninit[T](0)

func `=dup`*[T](a: seq[T]): seq[T] {.nodestroy.} =
  result = newSeqUninit[T](a.len)
  var i = 0
  while i < a.len:
    (result.data[i]) = `=dup`(a.data[i])
    inc i

func recalcCap(oldCap, addedElements: int): int {.inline.} =
  {.keepOverflowFlag.}:
    let requiredLen = oldCap + addedElements
    if overflowFlag():
      result = high(int)
    else:
      result = oldCap + (oldCap shr 1)
      if overflowFlag():
        result = requiredLen
      else:
        result = max(result, requiredLen)

func resize[T](dest: var seq[T]; addedElements: int): bool {.nodestroy.} =
  let oldCap = dest.capInBytes div sizeof(T)
  let newCap = recalcCap(oldCap, addedElements)
  let memSize = memSizeInBytes[T](newCap)
  dest.data = cast[ptr UncheckedArray[T]](realloc(dest.data, memSize))
  if dest.data == nil:
    dest.len = 0
    {.cast(noSideEffect).}:
      oomHandler memSize
    result = false
  else:
    result = true

func `=copy`*[T](dest: var seq[T]; src: seq[T]) {.nodestroy.} =
  if dest.data == src.data: return
  if src.len < dest.len:
    var i = src.len
    while i < dest.len:
      `=destroy`(dest.data[i])
      inc i
  elif dest.capInBytes < src.len * sizeof(T):
    let oldCap = dest.capInBytes div sizeof(T)
    let newCap = recalcCap(oldCap, src.len - oldCap)
    let memSize = memSizeInBytes[T](newCap)
    dest.data = cast[ptr UncheckedArray[T]](realloc(dest.data, memSize))
    if dest.data == nil:
      dest.len = 0
      {.cast(noSideEffect).}:
        oomHandler memSize
      return
  dest.len = src.len
  var i = 0
  while i < dest.len:
    dest.data[i] = `=dup`(src.data[i])
    inc i

func add*[T](s: var seq[T]; elem: sink T) {.inline, nodestroy.} =
  ## Appends `elem` to the end of `s`, growing storage if necessary.
  let L = s.len
  if s.capInBytes < (L * sizeof(T)) + sizeof(T):
    if not resize(s, 1):
      # It is our responsibility to destroy the `sink` element if
      # it could not be added:
      `=destroy`(elem)
      return
  inc s.len
  (s.data[L]) = elem

func len*[T](s: seq[T]): int {.inline.} =
  ## Number of elements in `s`.
  s.len

func rawData*[T](s: seq[T]): ptr UncheckedArray[T] {.inline.} =
  ## Unchecked pointer to `s`'s element storage (valid for `0 ..< s.len`).
  result = s.data

func `[]`*[T](s: seq[T]; i: int): var T {.requires: (i < s.len and i >= 0), inline.} = s.data[i]

func `[]=`*[T](s: var seq[T]; i: int; elem: sink T) {.requires: (i < s.len and i >= 0), inline.} =
  (s.data[i]) = elem

func `[]`*[T](s: seq[T]; i: uint): var T {.requires: (i < s.len.uint), inline.} = s.data[int i]

func `[]=`*[T](s: var seq[T]; i: uint; elem: sink T) {.requires: (i < s.len.uint), inline.} =
  (s.data[int i]) = elem

func `@`*[I, T](a: array[I, T]): seq[T] {.nodestroy.} =
  ## Copies an array into a new sequence (`@[1,2]` builds `seq` literals via array constructors).
  result = newSeqUninit[T](a.len)
  if result.data != nil:
    var i = 0
    while i < result.len:
      (result.data[i]) = `=dup`(a[i])
      inc i

# special cased in compiler as "@.1.<system suffix>" for empty seq type inference:
template `@`*[T](a: array[0, T]): seq[T] =
  ## Empty-array overload used with typed empty literals such as `@[]`.
  newSeqUninit[T](0)

func del*[T](s: var seq[T]; idx: int) {.nodestroy.} =
  ## Deletes index `idx` in amortized O(1) by swapping in the last element (may reorder tail).
  let L = s.len
  `=destroy`(s.data[idx])
  if idx != L-1:
    (s.data[idx]) = s.data[L-1]
  dec s.len

func addUnique*[T: Equatable](s: var seq[T]; x: sink T) =
  ## Append `x` to `s` only if no existing element equals it.
  for i in 0 ..< s.len:
    if s[i] == x: return
  s.add x

func shrink*[T](s: var seq[T]; newLen: int) =
  ## Drops trailing elements so length becomes `newLen`, destroying removed slots.
  var i = s.len-1
  while i >= newLen:
    `=destroy`(s.data[i])
    dec i
  s.len = newLen

func growUnsafe*[T](s: var seq[T]; newLen: int) =
  ## Grows length to `newLen` without constructing new slots (unsafe unless initialized afterward).
  {.keepOverflowFlag.}:
    let newSize = newLen * sizeof(T)
    if overflowFlag():
      {.cast(noSideEffect).}:
        oomHandler high(int)
      return
  if s.capInBytes < newSize:
    if not resize(s, newLen - s.len): return
  s.len = newLen

func grow*[T](s: var seq[T]; newLen: int; val: T) {.nodestroy.} =
  ## Extends `s` to length `newLen`, filling new slots with copies of `val`.
  var i = s.len
  growUnsafe(s, newLen)
  if s.data == nil: return
  while i < newLen:
    (s.data[i]) = `=dup`(val)
    inc i

func setLen*[T: HasDefault](s: var seq[T]; newLen: int) {.nodestroy.} =
  ## Sets length to `newLen`, shrinking or default-growing as needed (`grow`/`shrink` are often clearer).
  if newLen < s.len:
    shrink(s, newLen)
  else:
    var i = s.len
    growUnsafe(s, newLen)
    if s.data == nil: return
    while i < newLen:
      (s.data[i]) = default(T)
      inc i

proc newSeq*[T: HasDefault](s: out seq[T]; newLen: int) {.nodestroy, inline.} =
  s = newSeq[T](newLen)

func high*[T](s: seq[T]): int {.inline.} =
  ## Highest valid index (`len-1`), or `-1` when empty.
  s.len - 1
func low*[T](s: seq[T]): int {.inline.} =
  ## Lowest valid index (`0` for sequences).
  0

func pop*[T](s: var seq[T]): T {.requires: (s.len > 0), inline, nodestroy.} =
  ## Removes and returns the last element.
  let L = s.len-1
  result = s[L]
  s.len = L
