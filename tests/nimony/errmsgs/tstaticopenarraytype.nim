# A `static[openArray[int]]` type parameter is satisfied by an `array[N, int]`
# literal, so a plainly wrong argument (a string) is a constraint mismatch, not
# a vague "constant expression" error.
type Bad[S: static[openArray[int]]] = object
  n: int

var x: Bad["not an array"]
