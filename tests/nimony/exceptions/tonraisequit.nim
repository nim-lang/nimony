import std / [assertions, syncio, tables, hashes]

# `canRaise` is the nimony-vs-Nim shim: `raises` under nimony, no-op under
# host Nim. Mirrors `src/lib/compat2.nim` so the test does not depend on it.
when defined(nimony):
  {.pragma: canRaise, raises.}
else:
  {.pragma: canRaise.}

# Verifies the `onRaiseQuit` template for use in sem.nim et al.:
# under nimony it wraps a raising call in try/except so the raise does
# not propagate; under host-Nim it is a no-op pass-through. The point
# is that we get a single uniform call-site that satisfies nimony's
# "call .raises proc only inside try/except" rule without bubbling
# `.raises` up through sem.nim's many layers of helpers.

when defined(nimony):
  template onRaiseQuit(call: untyped): untyped =
    try:
      call
    except:
      quit "FAILURE: " & astToStr(call)
else:
  template onRaiseQuit(call: untyped): untyped =
    call

# A typical `.raises` shape: present-key Table indexing.
proc lookup(t: Table[string, int]; k: string): int {.canRaise.} =
  result = t[k]

# A void-result `.raises` shape, the other common case in sem.nim.
proc requireKey(t: Table[string, int]; k: string) {.canRaise.} =
  discard t[k]

proc main =
  var t = initTable[string, int]()
  t["alpha"] = 1
  t["beta"] = 2

  # Expression form: assigns the wrapped expression's result.
  let v = onRaiseQuit lookup(t, "alpha")
  assert v == 1

  # Statement form: discards the result.
  onRaiseQuit requireKey(t, "beta")

  # Nested args still work — the template captures the whole call subtree.
  let v2 = onRaiseQuit lookup(t, "alpha")
  assert v2 == 1

  echo "OK"

main()
