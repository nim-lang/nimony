proc foo*[R, C: static[int]; T](elems: array[R * C, T]): array[R * C, T] = elems

proc foo*[R, C: static[int]; T](fill: T): array[R * C, T] {.noinit.} =
  for i in 0 ..< R * C:
    result[i] = fill
