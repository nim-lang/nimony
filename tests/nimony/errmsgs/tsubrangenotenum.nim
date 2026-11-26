type
  H = enum
    h0, h1, h2
  RH = range[h0..h1]

proc g[T: not enum](x: T) = discard

var x: RH = h1
g(x)
