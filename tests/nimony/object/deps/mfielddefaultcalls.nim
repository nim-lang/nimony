proc helper(): seq[int] = @[4, 5]

proc ones[T](n: int): seq[T] =
  result = @[]
  for i in 0 ..< n: result.add T(1)

type
  P* = object
    s*: seq[int] = helper()
    w*: int = 2 * 21
    q*: Q = Q()
  Q* = object
    s*: seq[int] = @[6]
    k*: int = 3 + 4
  GD*[T] = object # the importer constructs instances of it
    s*: seq[T] = ones[T](2)
    n*: int = sizeof(T) + 1
