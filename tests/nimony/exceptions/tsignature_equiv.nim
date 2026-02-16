import std/syncio

type
  Base = object of RootObj
  Derived = object of Base

# Base method uses .raises (defaults to ErrorCode)
method foo(b: Base) {.raises.} =
  echo "Base.foo"

# Override with explicit .raises: ErrorCode - should have same signature
method foo(d: Derived) {.raises: ErrorCode.} =
  echo "Derived.foo"

echo "SUCCESS: .raises and .raises: ErrorCode have equivalent signatures"
