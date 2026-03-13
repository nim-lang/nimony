## Test forward declarations

# Forward declaration
proc foo(): int

proc bar(): int =
  result = foo()  # Call forward declared proc

# Implementation
proc foo(): int =
  result = 42

# Test
import std/assertions
assert foo() == 42
assert bar() == 42
