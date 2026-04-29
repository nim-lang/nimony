import std/syncio

type
  MyObj = object
    case
    of B1:
      a: int
    of B2:
      b: int

var obj = B1(a: 42)

# Error: guarded fields cannot be accessed via dot notation
echo obj.a
echo obj.b

# But allowed inside {.cast(uncheckedAssign).}:
{.cast(uncheckedAssign).}:
  echo obj.a
  echo obj.b
