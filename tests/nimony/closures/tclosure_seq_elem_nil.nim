# A closure value read straight out of a seq element — the `for` iterator's
# deref and the indexed form — compared against nil. The nil-compare must
# project the fn slot for these shapes too, not only for a symbol or a field:
# otherwise the C backend is handed a cast of the whole (fn, env) struct to a
# pointer and rejects it.
import std/syncio

type
  Hook = proc(x: int) {.closure.}
  Owner = ref object
    hooks: seq[Hook]

let o = Owner(hooks: @[])
var total = 0
o.hooks.add(proc(x: int) {.closure.} = total += x)

for h in o.hooks:
  if h != nil: h(21)
for i in 0 ..< o.hooks.len:
  if o.hooks[i] != nil: o.hooks[i](21)

echo total
