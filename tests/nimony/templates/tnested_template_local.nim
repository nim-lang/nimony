# A template declared by the expansion of another one: its locals are
# gensym'd already, and re-checking its body must keep the declaration bound
# to the symbol its uses refer to (it minted a fresh one, orphaning them).
import std/syncio

template outer(k: untyped) {.untyped.} =
  let s = k
  template inner(t: untyped) {.untyped.} =
    let tmp = t + s
    result = result + tmp
  result = 0
  inner(1)
  inner(2)

proc f(): int =
  outer(10)

echo f()
