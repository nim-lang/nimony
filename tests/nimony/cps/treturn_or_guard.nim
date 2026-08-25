import std/syncio

# A `return` guarded by an `or` condition leaves the return guard active for
# the rest of the proc, so the tail is wrapped in a borrowed guard ite. A later
# `if c: return <suspending call>` inside that tail else-exploits; its open
# else branch used to swallow the guard's `) . )` closure, giving the exploited
# ite a bogus fourth child (a stray dot) — the coroutine transform's bounded
# walker then failed with "into: body did not consume all children (left 1)".

proc leaf(x: int): int {.passive.} =
  suspend()
  result = x

proc handler(p: string; c: bool): int {.passive.} =
  if p == "/a" or p == "/b":
    return 1
  if c:
    return leaf(2)
  result = 3

proc main() {.passive.} =
  echo handler("/a", false)
  echo handler("/x", false)

main()
