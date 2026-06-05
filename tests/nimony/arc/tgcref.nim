import std/[assertions, syncio]

# Verifies the GC_ref / GC_unref primitives added to system/arcops:
#   GC_ref   -> +1 on the reference count (via the `=dup` hook, `nodestroy`)
#   GC_unref -> -1, freeing the object when it crosses zero (via `=destroy`)
#
# A fresh ref has rc=0 (== 1 logical reference); arcDec frees when rc goes
# negative. `assertRc` reads the rc field at offset 0 directly.

type
  NodeObj = object
    x: int
  Node = ref NodeObj

var destroyed = 0
proc `=destroy`(n: NodeObj) =
  inc destroyed

proc main =
  var n = Node(x: 42)
  assertRc(n, 0, "fresh")        # one logical reference

  GC_ref(n)
  assertRc(n, 1, "after 1 ref")  # +1
  GC_ref(n)
  assertRc(n, 2, "after 2 ref")  # +1

  GC_unref(n)
  assertRc(n, 1, "after 1 unref")
  GC_unref(n)
  assertRc(n, 0, "after 2 unref")
  assert n.x == 42               # still alive, untouched
  assert destroyed == 0

  # Consume the var's own logical reference manually: this crosses zero and
  # must run the base `=destroy` + free the block (mirrors how nifcore's
  # CursorOwner hands ownership to raw `alloc`'d memory).
  GC_unref(n)
  assert destroyed == 1
  `=wasMoved`(n)                 # avoid a double-free at scope exit

main()

# An extra GC_ref with no matching GC_unref keeps the object alive past the
# owning variable's scope-exit destructor (the deliberate "leak").
proc leaks =
  var n = Node(x: 7)
  GC_ref(n)                      # rc=1; scope-exit arcDec 1->0 won't free

destroyed = 0
leaks()
assert destroyed == 0           # survived: still referenced

echo "ok"
