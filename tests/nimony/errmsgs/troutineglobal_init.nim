# The initializer of a routine's `.global` runs once at module scope, where
# the routine's locals and parameters do not exist.

proc f(a: int): int =
  let b = a + 1
  var g {.global.} = b
  var h {.global.} = a
  var k {.global.} = a + 1
  result = g + h + k

discard f(1)

# A template's global belongs to the routine it expands in: the check runs at
# the expansion, where the template's parameter is the caller's argument.
template counted(start: int): int =
  var cached {.global.} = start
  inc cached
  cached

proc fromLocal(a: int): int =
  result = counted(a)

proc fromLocalExpr(a: int): int =
  result = counted(a + 1)

proc fromLiteral(): int =
  result = counted(3)

discard fromLocal(1)
discard fromLocalExpr(1)
discard fromLiteral()
