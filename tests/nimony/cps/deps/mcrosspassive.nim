import std/syncio

# `.passive` procs defined here, called from another module: their coro
# helpers must be mangled with THIS module's suffix (hexer/coro_transform).

proc innerStep*() {.passive.} =
  echo "inner a"
  suspend()
  echo "inner b"

proc pingpong*() {.passive.} =
  echo "ping"
  innerStep()
  echo "pong"

proc addUp*(a, b: int): int {.passive.} =
  return a + b
