# `static` parameters declared in another module: the `staticParam` tag is
# part of the published signature, so the importer enforces the restriction.

proc rep*[T](a: T; n: static[int] = 2): seq[T] =
  result = @[]
  for i in 0 ..< n: result.add a

proc pick*(i: static[int]): string =
  if i == 0: "zero" else: "other"
