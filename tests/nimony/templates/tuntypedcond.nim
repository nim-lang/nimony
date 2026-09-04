# A value built from an `untyped` parameter is not typed until the template is
# expanded. Three places used to insist on knowing its type too early.

import std/[syncio, assertions]

# --- 1. an `untyped` value in a condition ---------------------------------
# The template's own body cannot say whether `call < 0` is a `bool`; the
# expansion can. Demanding it here reported "expected `bool` but got: untyped"
# against code that is perfectly fine, and the error node it appended made the
# `elif` a three-child node — so the next pass to walk that stored body took
# the error for the branch and crashed on the leftover.

template clampLow(dest: var int; call: untyped) =
  if call < 0:
    dest = 0
  else:
    dest = 1

proc negate(x: int): int = -x

proc viaCondition(x: int): int =
  result = -1
  clampLow result, negate(x)

assert viaCondition(5) == 0        # negate(5) = -5, so the `then` branch
assert viaCondition(-3) == 1       # negate(-3) = 3, so the `else` branch

# --- 2. a local whose type comes from the argument ------------------------
# `let n = call` is `untyped` in the body and `int` in every expansion. The
# body's type must not travel with it, or `n` goes on claiming `untyped` over
# a value that has a real type and every later use of it mismatches.

template emit(j: var int; call: untyped) =
  let n = call
  if n < 0: return n
  j += n

proc twice(x: int): int = x + x

proc viaLocal(x: int): int =
  var j = 0
  emit j, twice(x)
  emit j, twice(x)
  result = j

assert viaLocal(3) == 12
assert viaLocal(-1) == -2          # the `return` inside the expansion wins

# --- 3. the same local in a `for` and a nested template -------------------

template sumInto(acc: var int; call: untyped) =
  let n = call
  for i in 0 ..< n:
    acc += i

proc viaLoop(x: int): int =
  result = 0
  sumInto result, twice(x)

assert viaLoop(2) == 6             # 0+1+2+3

echo "ok"
