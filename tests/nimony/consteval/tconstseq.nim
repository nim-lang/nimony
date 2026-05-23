import std/assertions

# `const c: seq[T] = @[...]` is compile-time evaluable via the sub-compile
# in `exprexec`. The runtime `@` runs in the sub-compile, then the
# resulting `seq[T]` is serialised back into NIF using the ptr-to-nif rule
# for the `data: ptr UncheckedArray[T]` field — emitted as
# `(addr (aconstr (uarray T) e1 ... eN))`. Hexer's nifcgen hoists the
# aconstr to an anonymous module-level static array and rewrites the
# `addr` to a `cast` pointing at it.

const
  c: seq[int] = @[1, 2, 3]
  big: seq[int] = @[10, 20, 30, 40, 50]
  single: seq[int] = @[42]

assert c.len == 3
assert c[0] == 1
assert c[1] == 2
assert c[2] == 3

assert big.len == 5
assert big[0] == 10
assert big[4] == 50

assert single.len == 1
assert single[0] == 42

# Distinct const seqs end up as distinct anonymous static arrays.
const
  a: seq[int] = @[1, 2, 3]
  b: seq[int] = @[4, 5, 6]
assert a[0] == 1 and a[2] == 3
assert b[0] == 4 and b[2] == 6
