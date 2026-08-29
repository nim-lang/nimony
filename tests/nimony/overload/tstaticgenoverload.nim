import std/[assertions, syncio]

# Orphan static type parameter on a generic overload must not compete with a
# non-generic overload at an un-instantiated call site.
func foo[Z: static[int]](x: float): int = Z
func foo(x: float): int = foo[1](x)

assert foo[1](1.0) == 1
assert foo(1.0) == 1

# Same rule for orphan regular type parameters.
proc bar[T](x: int): string = "generic"
proc bar(x: int): string = "concrete"

assert bar(1) == "concrete"

# Static type parameters that appear in the callable surface are still inferred.
proc len[N: static[int]](x: array[N, int]): int = N

var a: array[3, int]
assert len(a) == 3
assert len[3](a) == 3

echo "ok"
