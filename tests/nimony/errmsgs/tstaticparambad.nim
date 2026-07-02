# A value ("static") generic parameter must name an explicit element type;
# a bare `static` has no value type to bind against.
type
  Bad[N: static] = object
    data: array[N, int]
