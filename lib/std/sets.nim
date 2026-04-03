
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

iterator items*[T](s: HashSet[T]): lent T =
  for k, v in pairs(s.t):
    if v: yield k
