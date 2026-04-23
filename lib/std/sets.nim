
import tables

type
  HashSet*[T] = object
    t: Table[T, bool]

func initHashSet*[T](): HashSet[T] =
  HashSet[T](t: initTable[T, bool]())

func incl*[T](s: var HashSet[T]; x: sink T) =
  s.t[x] = true

func excl*[T](s: var HashSet[T]; x: T) =
  s.t[x] = false

func contains*[T](s: HashSet[T]; x: T): bool =
  s.t.getOrDefault(x)

func containsOrIncl*[T](s: var HashSet[T]; x: T): bool =
  result = contains(s, x)
  if not result:
    incl s, x

func missingOrExcl*[T](s: var HashSet[T]; x: T): bool =
  ## Returns true if `x` was *not* in `s` (the element was missing).
  ## Otherwise removes `x` from `s` and returns false.
  result = not contains(s, x)
  if not result:
    excl s, x

iterator items*[T](s: HashSet[T]): lent T =
  for k, v in pairs(s.t):
    if v: yield k

func len*[T](s: HashSet[T]): int =
  ## Number of elements in the set.
  result = 0
  for k, v in pairs(s.t):
    if v: inc result

func intersection*[T](a, b: HashSet[T]): HashSet[T] =
  ## Elements present in both `a` and `b`.
  result = initHashSet[T]()
  for x in items(a):
    if x in b:
      result.incl x

func incl*[T](s: var HashSet[T]; other: HashSet[T]) =
  ## Union `other` into `s`.
  for x in items(other):
    s.incl x

func excl*[T](s: var HashSet[T]; other: HashSet[T]) =
  ## Remove every element of `other` from `s`.
  for x in items(other):
    s.excl x
