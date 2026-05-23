import std/assertions
import std/tables

# `const t: Table[K, V] = expr` flows through the same path as const
# seqs: the sub-compile runs the expression for real (allocating with
# the system allocator), then the `ptr UncheckedArray[T]` fields of the
# resulting Table's two seqs (`data`, `hashes`) get serialised via the
# ptr-to-nif rule as `(addr (aconstr (uarray T) …))`. Hexer's nifcgen
# hoists each aconstr to an anonymous module-level static array.
#
# Two things that the seq tests don't exercise but this one does:
#   1. Element types that are themselves objects (HashEntry, which is
#      *private* in tables.nim — kept-as-Symbol rewrite in exprexec
#      lets the sub-compile resolve it without a re-export).
#   2. Recursive annotation through `(addr (aconstr (uarray T) …))`:
#      `annotateConstantType` recurses element-wise so each inner
#      oconstr (e.g. HashEntry instances) gets its type slot normalised
#      to a Symbol — sem rejects oconstrs with inline `(object …)` slots.

proc makeIt(): Table[string, int] =
  result = initTable[string, int]()
  result["a"] = 1
  result["b"] = 2
  result["c"] = 3

const t: Table[string, int] = makeIt()

assert t.len == 3
assert t.getOrDefault("a", -1) == 1
assert t.getOrDefault("b", -1) == 2
assert t.getOrDefault("c", -1) == 3
assert t.getOrDefault("missing", -1) == -1
assert t.contains("a")
assert not t.contains("missing")

# An empty Table also goes through the path (both seqs empty, so each
# anonArr static is a zero-length array).
const empty: Table[string, int] = initTable[string, int]()
assert empty.len == 0
assert not empty.contains("anything")
