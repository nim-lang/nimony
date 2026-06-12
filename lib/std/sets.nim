
import tables

type
  HashSet*[T: Keyable] = object ## Hash-backed set of distinct values (implemented as `Table[T, bool]`).
    t: Table[T, bool]

func initHashSet*[T: Keyable](): HashSet[T] =
  ## Constructs an empty `HashSet`.
  HashSet[T](t: initTable[T, bool]())

func incl*[T: Keyable](s: var HashSet[T]; x: sink T) =
  ## Inserts `x` into `s`.
  s.t[x] = true

func excl*[T: Keyable](s: var HashSet[T]; x: T) =
  ## Removes `x` from `s` if present.
  s.t[x] = false

func contains*[T: Keyable](s: HashSet[T]; x: T): bool =
  ## True if `x` is in `s`.
  s.t.getOrDefault(x)

func containsOrIncl*[T: Keyable](s: var HashSet[T]; x: T): bool =
  ## True if `x` was already present; otherwise inserts it and returns false.
  result = contains(s, x)
  if not result:
    incl s, x

func missingOrExcl*[T: Keyable](s: var HashSet[T]; x: T): bool =
  ## Returns true if `x` was *not* in `s` (the element was missing).
  ## Otherwise removes `x` from `s` and returns false.
  result = not contains(s, x)
  if not result:
    excl s, x

iterator items*[T: Keyable](s: HashSet[T]): lent T =
  ## Yields each element currently stored in `s`.
  for k, v in pairs(s.t):
    if v: yield k

func len*[T: Keyable](s: HashSet[T]): int =
  ## Number of elements in the set.
  result = 0
  for k, v in pairs(s.t):
    if v: inc result

func intersection*[T: Keyable](a, b: HashSet[T]): HashSet[T] =
  ## Elements present in both `a` and `b`.
  result = initHashSet[T]()
  for x in items(a):
    if x in b:
      result.incl x

func incl*[T: Keyable](s: var HashSet[T]; other: HashSet[T]) =
  ## Union `other` into `s`.
  for x in items(other):
    s.incl x

func excl*[T: Keyable](s: var HashSet[T]; other: HashSet[T]) =
  ## Remove every element of `other` from `s`.
  for x in items(other):
    s.excl x
