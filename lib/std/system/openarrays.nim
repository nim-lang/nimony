## Openarray implementation.

type
  openArray*[T] {.view.} = object
    a: ptr UncheckedArray[T]
    len: int

proc `[]`*[T](x: openArray[T]; idx: int): var T {.inline, requires: idx >= 0 and idx < x.len.} = x.a[idx]

proc `[]=`*[T](x: openArray[T]; i: int; elem: sink T) {.inline, requires: i >= 0 and i < x.len.} =
  (x[i]) = elem

converter toOpenArray*[I, T](x {.byref.}: array[I, T]): openArray[T] {.inline.} =
  if len(x) == 0:
    openArray[T](a: nil, len: 0)
  else:
    openArray[T](a: cast[ptr UncheckedArray[T]](addr(x)), len: len(x))

converter toOpenArray*[T](s: seq[T]): openArray[T] {.inline.} =
  openArray[T](a: rawData(s), len: s.len)

converter toOpenArray*(s: string): openArray[char] {.inline.} =
  openArray[char](a: rawData(s), len: s.len)

func len*[T](a: openArray[T]): int {.inline.} = a.len

type
  Equatable* = concept
    proc `==`(a, b: Self): bool

func find*[T: Equatable](a: openArray[T]; elem: T): int =
  var i = 0
  while i < len(a):
    if a[i] == elem: return i
    inc i
  return -1

func contains*[T: Equatable](a: openArray[T]; elem: T): bool {.inline.} =
  find(a, elem) >= 0

iterator items*[T](a: openArray[T]): var T =
  var i = 0
  while i < len(a):
    yield a[i]
    inc i

proc `==`*[T: Equatable](a, b: openArray[T]): bool =
  if a.len == b.len:
    for i in 0..<a.len:
      if a[i] != b[i]: return false
    return true
  return false
