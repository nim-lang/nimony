# see issue #2012: a parameter without a type and without a default value
# must report a proper error instead of crashing the compiler.
proc f(x) =
  discard
