type
  A = A
  B = C
  C = D
  D = B
  R = ref R
  P = proc (x: P)
  G[T] = G[T]
  H[T] = K[T]
  K[T] = H[T]

proc f(x: B) = discard
var g: G[int]
var h: H[int]
