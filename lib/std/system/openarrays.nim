## Openarray implementation.

{.feature: "lenientnils".}

type
  openArray*[T] {.view.} = object
    a: ptr UncheckedArray[T]
    len: int

func `[]`*[T](x: openArray[T]; idx: int): var T {.inline, requires: idx >= 0 and idx < x.len.} = x.a[idx]

func `[]=`*[T](x: var openArray[T]; i: int; elem: sink T) {.inline, requires: i >= 0 and i < x.len.} =
  (x[i]) = elem

func high*[T](a: openArray[T]): int {.inline.} = len(a) - 1
func low*[T](a: openArray[T]): int {.inline.} = 0
func len*[T](a: openArray[T]): int {.inline, ensures: (0 <= result and result <= MaxContainerLen).} =
  result = a.len
  # a view is made from a container's length, or by `viewLen`: either way
  # within `0 .. MaxContainerLen`
  {.assume: 0 <= result and result <= MaxContainerLen.}

# special cased in compiler as "toOpenArray.0.<system suffix>" for empty array type inference:
converter toOpenArray*[I, T](x {.byref.}: array[I, T]): openArray[T] {.inline, establishesBorrow,
    ensures: len(result) == len(x).} =
  if len(x) == 0:
    result = openArray[T](a: nil, len: 0)
  else:
    result = openArray[T](a: cast[ptr UncheckedArray[T]](addr(x)), len: len(x))
  # the view covers the whole array; the prover does not read constructor fields
  {.assume: len(result) == len(x).}

# special cased in compiler as "toOpenArray.1.<system suffix>" for empty seq type inference:
converter toOpenArray*[T](s: seq[T]): openArray[T] {.inline, establishesBorrow,
    ensures: len(result) == len(s).} =
  result = openArray[T](a: rawData(s), len: s.len)
  {.assume: len(result) == len(s).}

converter toOpenArray*(s {.byref.}: string): openArray[char] {.inline, establishesBorrow,
    ensures: len(result) == len(s).} =
  result = openArray[char](a: readRawData(s), len: s.len)
  {.assume: len(result) == len(s).}

type
  Equatable* = concept
    func `==`(a, b: Self): bool

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

iterator mitems*[T](a: var openArray[T]): var T =
  var i = 0
  while i < len(a):
    yield a[i]
    inc i

iterator pairs*[T](a: openArray[T]): (int, var T) =
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc i

iterator mpairs*[T](a: var openArray[T]): (int, var T) =
  var i = 0
  while i < len(a):
    yield (i, a[i])
    inc i

func `==`*[T: Equatable](a, b: openArray[T]): bool =
  if a.len == b.len:
    for i in 0..<a.len:
      if a[i] != b[i]: return false
    return true
  return false

func viewLen(first, last: int): int =
  ## The length of the view `first .. last`: empty for `last < first`, and a
  ## view no memory can hold — longer than `MaxContainerLen`, or a span whose
  ## length wraps — is reported like an exhausted heap and is empty too. So
  ## every openArray's `len` lies in `0 .. MaxContainerLen`, as for the
  ## containers.
  if last < first: return 0
  # `last >= first`, so the distance is exact as a `uint`
  let d = uint(last) - uint(first)
  if d >= uint(MaxContainerLen):
    {.cast(noSideEffect).}:
      oomHandler high(int)
    result = 0
  else:
    result = int(d) + 1

# no `establishesBorrow` here: the view is built from a raw pointer, so there is
# no owner to borrow from and the caller is on its own for the lifetime.
func toOpenArray*[T](x: ptr UncheckedArray[T]; first, last: int): openArray[T] =
  openArray[T](a: cast[ptr UncheckedArray[T]](cast[uint](x) + uint(first * sizeof(T))), len: viewLen(first, last))

func toOpenArray*[T](x: openArray[T]; first, last: int): openArray[T] {.establishesBorrow.} =
  openArray[T](a: cast[ptr UncheckedArray[T]](cast[uint](x.a) + uint(first * sizeof(T))), len: viewLen(first, last))

proc `@`*[T](a: openArray[T]): seq[T] {.nodestroy.} =
  ## Turns an *openArray* into a sequence.
  ##
  ## This is not as efficient as turning a fixed length array into a sequence
  ## as it always copies every element of `a`.
  result = newSeqUninit[T](a.len)
  if result.data != nil:
    for i in 0 .. a.high:
      (result.data[i]) = `=dup`(a[i])
