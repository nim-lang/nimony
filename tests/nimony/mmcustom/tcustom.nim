import std/[assertions, syncio]

# The whole directory is compiled with `--mm:tests/nimony/mmcustom/rt/counting`
# (see `nimony.args`): a strategy given by PATH rather than by name, so
# `include "$MM"` in system.nim resolved to a file outside `lib/std/system/`.
# Checks that the custom runtime is the one that got included, that it is used,
# and that `defined(gc<Name>)` follows the module's basename.

assert defined(gcCounting), "--mm:<path> must define gc<basename>"
assert not defined(gcAtomicArc), "the default strategy must not be included too"
assert customRuntimeMarker == 42   # only `rt/counting.nim` defines this

type
  NodeObj = object
    x: int
  Node = ref NodeObj

proc main =
  var n = Node(x: 42)
  assertRc(n, 0, "fresh")
  let incs = customArcIncs
  block:
    let m = n                    # copy: rc 0 -> 1, through the custom `arcInc`
    assertRc(m, 1, "after copy")
    assert m.x == 42
  assertRc(n, 0, "copy destroyed")
  assert customArcIncs > incs, "the custom arcInc must be the one that runs"
  assert customArcDecs > 0

main()
echo "ok"
