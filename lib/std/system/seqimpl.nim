# seq implementation.

type
  seq*[T] = object
    len: int
    data: ptr UncheckedArray[T]

proc cap[T](s: seq[T]): int {.inline.} =
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

proc newSeq*[T](size: int): seq[T] {.nodestroy.} =
  if size == 0:
    result = seq[T](len: size, data: nil)
  else:
    let memSize = size * sizeof(T)
    result = seq[T](len: size, data: cast[ptr UncheckedArray[T]](alloc(memSize)))
    if result.data != nil:
      var i = 0
      while i < size:
        (result.data[i]) = default(T)
        inc i
    else:
      oomHandler memSize

proc newSeqOf*[T](size: int; initValue: T): seq[T] {.nodestroy.} =
  if size == 0:
    result = seq[T](len: size, data: nil)
  else:
    let memSize = size * sizeof(T)
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
    let memSize = size * sizeof(T)
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

proc recalcCap(oldCap, addedSize: int): int {.inline.} =
  result = oldCap + (oldCap shr 1)
  if result < oldCap + addedSize:
    result = oldCap + addedSize

proc resize[T](dest: var seq[T]; addedSize: int) {.nodestroy.} =
  let newCap = recalcCap(dest.cap, addedSize)
  let memSize = newCap * sizeof(T)
  dest.data = cast[ptr UncheckedArray[T]](realloc(dest.data, memSize))
  if result.data == nil:
    result.len = 0
    oomHandler memSize

proc `=copy`*[T](dest: var seq[T]; src: seq[T]) {.nodestroy.} =
  if dest.data == src.data: return
  if src.len < dest.len:
    var i = src.len
    while i < dest.len:
      `=destroy`(dest.data[i])
      inc i
  elif dest.cap < src.len:
    let newCap = recalcCap(dest.cap, src.len - dest.cap)
    let memSize = newCap * sizeof(T)
    dest.data = cast[ptr UncheckedArray[T]](realloc(dest.data, memSize))
    if result.data == nil:
      result.len = 0
      oomHandler memSize
      return
  dest.len = src.len
  var i = 0
  while i < dest.len:
    dest.data[i] = `=dup`(src.data[i])
    inc i

proc add*[T](s: var seq[T]; elem: sink T) {.inline, nodestroy.} =
  let L = s.len
  if s.cap <= L:
    resize s, 1
    if s.data == nil: return
  inc s.len
  (s.data[L]) = elem

proc len*[T](s: seq[T]): int {.inline.} = s.len

proc `[]`*[T](s: seq[T]; i: int): var T {.requires: (i < s.len and i >= 0), inline.} = s.data[i]

proc `[]=`*[T](s: var seq[T]; i: int; elem: sink T) {.requires: (i < s.len and i >= 0), inline.} =
  (s.data[i]) = elem

proc `[]`*[T](s: seq[T]; i: uint): var T {.requires: (i < s.len), inline.} = s.data[int i]

proc `[]=`*[T](s: var seq[T]; i: uint; elem: sink T) {.requires: (i < s.len), inline.} =
  (s.data[int i]) = elem

proc `@`*[I, T](a: array[I, T]): seq[T] {.nodestroy.} =
  result = newSeqUninit[T](a.len)
  if result.data != nil:
    var i = 0
    while i < result.len:
      (result.data[i]) = `=dup`(a[i])
      inc i

template `@`*[T](a: array[0, T]): seq[T] = newSeqUninit[T](0)

converter toOpenArray*[T](x: seq[T]): openArray[T] {.inline.} =
  toOpenArray(x.data, len(x))

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
  if s.cap <= newLen:
    resize s, newLen - s.len
    if s.data == nil: return
  s.len = newLen

proc grow*[T](s: var seq[T]; newLen: int; val: T) =
  var i = s.len
  growUnsafe(s, newLen)
  if s.data == nil: return
  while i < newLen:
    (s.data[i]) = val
    inc i

proc high*[T](s: seq[T]): int {.inline.} = s.len - 1

proc pop*[T](s: var seq[T]): T {.requires: (s.len > 0), inline, nodestroy.} =
  let L = s.len-1
  result = s[L]
  s.len = L
