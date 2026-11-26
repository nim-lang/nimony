# The initializer of a routine's `.global` runs once at module scope, where
# the routine's locals and parameters do not exist.

proc f(a: int): int =
  let b = a + 1
  var g {.global.} = b
  var h {.global.} = a
  result = g + h

discard f(1)
