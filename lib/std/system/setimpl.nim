func `[]`[T](a: set[T]; i: int): var uint8 {.magic: "ArrAt".}
template `[]=`[T](a: set[T]; i: int; val: uint8) =
  (a[i]) = val

func `+`*[T](a, b: set[T]): set[T] {.inline, noinit.} =
  for i in 0 ..< sizeof(a): result[i] = a[i] or b[i]

func `*`*[T](a, b: set[T]): set[T] {.inline, noinit.} =
  for i in 0 ..< sizeof(a): result[i] = a[i] and b[i]

func `-`*[T](a, b: set[T]): set[T] {.inline, noinit.} =
  for i in 0 ..< sizeof(a): result[i] = a[i] and not b[i]

func `==`*[T](a, b: set[T]): bool {.inline.} =
  for i in 0 ..< sizeof(a):
    if a[i] != b[i]: return false
  return true

func `<=`*[T](a, b: set[T]): bool {.inline.} =
  for i in 0 ..< sizeof(a):
    if (a[i] and not b[i]) != 0'u8: return false
  return true

func `<`*[T](a, b: set[T]): bool = (a <= b) and not (a == b)

func contains*[T](a: set[T], b: T): bool =
  a * {b} != {}
