# Sound `or` (sum-type) constraints: a generic body may only use operations
# present in *every* alternative concept (their intersection); `and` exposes the
# union. nim-lang/nimony#2029.
import std/assertions
import std/syncio

type
  Additive = concept     # has `+` and `*`
    proc `+`(a, b: Self): Self
    proc `*`(a, b: Self): Self
  Subtractive = concept  # has `-` and `*`
    proc `-`(a, b: Self): Self
    proc `*`(a, b: Self): Self
  Scalable = concept     # has only `*`
    proc `*`(a, b: Self): Self

  Money = object
    cents: int

func `+`(a, b: Money): Money = Money(cents: a.cents + b.cents)
func `-`(a, b: Money): Money = Money(cents: a.cents - b.cents)
func `*`(a, b: Money): Money = Money(cents: a.cents * b.cents)

# `or`: only the shared `*` is guaranteed, so only `*` may be used.
func scaleOr[T: Additive or Subtractive](a, b: T): T = a * b

# `and`: the union `+`, `-`, `*` is available.
func combineAnd[T: Additive and Subtractive](a, b: T): T = (a + b) * (a - b)

# three-way `or`: `*` is the only operation common to all three.
func scale3[T: Additive or Subtractive or Scalable](a, b: T): T = a * b

# nested `(A and B) or C`: guaranteed ops are (ops(A) ∪ ops(B)) ∩ ops(C) = {*}.
func scaleNested[T: (Additive and Subtractive) or Scalable](a, b: T): T = a * b

# the original issue example: `int` satisfies both, `*` is shared.
func foo[T: Additive or Subtractive](a, b: T): T = a * b

assert scaleOr(Money(cents: 6), Money(cents: 7)).cents == 42
assert combineAnd(Money(cents: 5), Money(cents: 2)).cents == 21   # (7)*(3)
assert scale3(Money(cents: 3), Money(cents: 4)).cents == 12
assert scaleNested(Money(cents: 3), Money(cents: 5)).cents == 15
assert foo(6, 7) == 42
echo "or-sumtype-constraint: OK"
