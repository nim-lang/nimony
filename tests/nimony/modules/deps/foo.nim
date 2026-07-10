proc foo*[N: static[int]; T](elems: array[N, T]): array[N, T] =
  elems
