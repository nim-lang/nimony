# seq implementation.

type
  seq*[T] = object
    len: int
    data: ptr UncheckedArray[T]

proc capInBytes[T](s: seq[T]): int {.inline.} =
  result = if s.data != nil: allocatedSize(s.data) else: 0

proc `=destroy`*[T](s: seq[T]) =
  var i = 0
  while i < s.len:
    `=destroy`(s.data[i])
    inc i
  if s.data != nil:
    dealloc s.data

proc `=wasMoved`*[T](s: var seq[T]) {.inline.} =
  s.len = 0
  s.data = nil

proc memSizeInBytes[T](size: int): int {.inline.} =
  {.keepOverflowFlag.}:
    result = size * sizeof(T)
    if overflowFlag():
      # When required memory size is overflowed, cause out of memory.
      result = high(int)

proc newSeq*[T: HasDefault](size: int): seq[T] {.nodestroy.} =
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
      oomHandler memSize

proc newSeqOf*[T](size: int; initValue: T): seq[T] {.nodestroy.} =
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
      oomHandler memSize

proc newSeqUninit*[T](size: int): seq[T] {.nodestroy, inline.} =
  if size == 0:
    result = seq[T](len: size, data: nil)
  else:
    let memSize = memSizeInBytes[T](size)
    result = seq[T](len: size, data: cast[ptr UncheckedArray[T]](alloc(memSize)))
    if result.data != nil:
      discard "leave uninitialized"
    else:
      result.len = 0
      oomHandler memSize

template default*[T](x: typedesc[seq[T]]): seq[T] = newSeqUninit[T](0)

proc `=dup`*[T](a: seq[T]): seq[T] {.nodestroy.} =
  result = newSeqUninit[T](a.len)
  var i = 0
  while i < a.len:
    (result.data[i]) = `=dup`(a.data[i])
    inc i

proc recalcCap(oldCap, addedElements: int): int {.inline.} =
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

proc resize[T](dest: var seq[T]; addedElements: int): bool {.nodestroy.} =
  let oldCap = dest.capInBytes div sizeof(T)
  let newCap = recalcCap(oldCap, addedElements)
  let memSize = memSizeInBytes[T](newCap)
  dest.data = cast[ptr UncheckedArray[T]](realloc(dest.data, memSize))
  if dest.data == nil:
    dest.len = 0
    oomHandler memSize
    result = false
  else:
    result = true

proc `=copy`*[T](dest: var seq[T]; src: seq[T]) {.nodestroy.} =
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
      oomHandler memSize
      return
  dest.len = src.len
  var i = 0
  while i < dest.len:
    dest.data[i] = `=dup`(src.data[i])
    inc i

proc add*[T](s: var seq[T]; elem: sink T) {.inline, nodestroy.} =
  let L = s.len
  if s.capInBytes <= L * sizeof(T):
    if not resize(s, 1):
      # It is our responsibility to destroy the `sink` element if
      # it could not be added:
      `=destroy`(elem)
      return
  inc s.len
  (s.data[L]) = elem

proc len*[T](s: seq[T]): int {.inline.} = s.len

proc rawData*[T](s: seq[T]): ptr UncheckedArray[T] {.inline.} =
  result = s.data

proc `[]`*[T](s: seq[T]; i: int): var T {.requires: (i < s.len and i >= 0), inline.} = s.data[i]

proc `[]=`*[T](s: var seq[T]; i: int; elem: sink T) {.requires: (i < s.len and i >= 0), inline.} =
  (s.data[i]) = elem

proc `[]`*[T](s: seq[T]; i: uint): var T {.requires: (i < s.len.uint), inline.} = s.data[int i]

proc `[]=`*[T](s: var seq[T]; i: uint; elem: sink T) {.requires: (i < s.len.uint), inline.} =
  (s.data[int i]) = elem

proc `@`*[I, T](a: array[I, T]): seq[T] {.nodestroy.} =
  result = newSeqUninit[T](a.len)
  if result.data != nil:
    var i = 0
    while i < result.len:
      (result.data[i]) = `=dup`(a[i])
      inc i

# special cased in compiler as "@.1.<system suffix>" for empty seq type inference:
template `@`*[T](a: array[0, T]): seq[T] = newSeqUninit[T](0)

proc del*[T](s: var seq[T]; idx: int) {.nodestroy.} =
  let L = s.len
  `=destroy`(s.data[idx])
  if idx != L-1:
    (s.data[idx]) = s.data[L-1]
  dec s.len

proc shrink*[T](s: var seq[T]; newLen: int) =
  var i = s.len-1
  while i >= newLen:
    `=destroy`(s.data[i])
    dec i
  s.len = newLen

proc growUnsafe*[T](s: var seq[T]; newLen: int) =
  {.keepOverflowFlag.}:
    let newSize = newLen * sizeof(T)
    if overflowFlag():
      oomHandler high(int)
      return
  if s.capInBytes <= newSize:
    if not resize(s, newLen - s.len): return
  s.len = newLen

proc grow*[T](s: var seq[T]; newLen: int; val: T) {.nodestroy.} =
  var i = s.len
  growUnsafe(s, newLen)
  if s.data == nil: return
  while i < newLen:
    (s.data[i]) = `=dup`(val)
    inc i

proc high*[T](s: seq[T]): int {.inline.} = s.len - 1
proc low*[T](s: seq[T]): int {.inline.} = 0

proc pop*[T](s: var seq[T]): T {.requires: (s.len > 0), inline, nodestroy.} =
  let L = s.len-1
  result = s[L]
  s.len = L
