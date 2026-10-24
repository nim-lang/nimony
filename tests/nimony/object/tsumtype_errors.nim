import std/syncio

type
  Node = ref object
    case
    of AddOpr, SubOpr:
      a, b: Node
    of Value:
      val: int

# Missing branch (non-exhaustive)
proc incomplete(n: Node): int =
  case n
  of Value(val):
    result = val
  of AddOpr(a, b):
    result = 0

# Duplicate branch
proc duplicate(n: Node): int =
  case n
  of Value(val):
    result = val
  of AddOpr(a, b):
    result = 0
  of SubOpr(a, b):
    result = 1
  of Value(val):
    result = 2

# Set pattern mixing branches from different `of` declarations
proc badSet(n: Node): int =
  case n
  of {AddOpr, Value}(a, b):
    result = 0
  else:
    result = 1

# The expected type of a constructor is not a way around a type mismatch.
type
  Res[T, E] = object
    case
    of Ok: v: T
    of Err: e: E

proc show(r: Res[int, string]): int = 0
discard show(Err(e: 5))

proc text(): int = "text"
