## Handle GC-integration: JsValue owns its runtime table slot, so transient
## values are reclaimed at scope exit (=destroy) and copying a handle allocates
## an independent slot to the same JS value (=dup/=copy) — no leaks, no double free.
import std/syncio
import jsffi

# 1. Transient handles are reclaimed each iteration; the table does not grow.
let base = liveHandles()
for i in 0 ..< 1000:
  let s = toJs("hello")
  discard toStr(s)
  let m = global("Math")
  discard toInt(m.call("max", toJs(1), toJs(i)))
echo "leak after 1000 iters: " & $(liveHandles() - base)

# 2. A copy is an independent slot to the same value (=dup), not a shared alias.
let mark = liveHandles()
let a = toJs("shared")
let b = a
echo "a == b (===): " & $(a == b)
echo "live handles from a + its copy: " & $(liveHandles() - mark)

echo "ok"
