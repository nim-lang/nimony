import std/[assertions, syncio]

# The whole directory is compiled with `--mm:arc` (see `nimony.args`), so
# `include "$MM"` in system.nim resolved to `system/arc` instead of the default
# `system/atomicarc`. Checks the switch took effect and that reference counting
# still behaves identically — the two strategies differ only in whether the
# counter updates are atomic.

assert defined(gcArc), "--mm:arc must define gcArc"
assert not defined(gcAtomicArc), "--mm:arc must not define gcAtomicArc"

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
  block:
    let m = n                    # copy: rc 0 -> 1
    assertRc(m, 1, "after copy")
    assert m.x == 42
  assertRc(n, 0, "copy destroyed")
  assert destroyed == 0

  GC_ref(n)
  assertRc(n, 1, "after ref")
  GC_unref(n)
  assertRc(n, 0, "after unref")
  assert destroyed == 0

main()
assert destroyed == 1            # freed at scope exit

var s = @[1, 2, 3]
var s2 = s
s2.add 4
assert s.len == 3
assert s2.len == 4

echo "ok"
