import std/syncio

# `.passive` procs defined here, called from another module. Their coro
# helpers (`coro` frame, `init` wrapper, `s<state>` procs) are generated
# once, by THIS module's hexer run, so the caller has to mangle them with
# this module's suffix to reach them.

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
