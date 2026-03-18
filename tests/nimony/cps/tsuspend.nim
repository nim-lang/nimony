import std/syncio

proc ioOp() {.passive.} =
  echo "io operation"
  suspend()
  echo "unreachable after suspend"

proc main1() {.passive.} =
  echo "before io"
  discard delay ioOp()
  echo "after delay"

main1()

proc nestedSuspend() {.passive.} =
  echo "1"
  suspend()
  echo "2"

proc main2() {.passive.} =
  echo "start"
  nestedSuspend()
  echo "end"

main2()

proc suspendInLoop() {.passive.} =
  for i in 0..<2:
    echo "loop " & $i
    suspend()

proc main3() {.passive.} =
  echo "before loop"
  suspendInLoop()
  echo "after loop"

main3()


proc suspendBranch(x: int) {.passive.} =
  if x == 0:
    suspend()
  else:
    echo "x != 0"

proc main4(x: int) {.passive.} =
  echo "before branch"
  suspendBranch(x)
  echo "after branch"

main4(0)
main4(1)