import std/syncio

var resumeCont: Continuation

proc suspendingProc() {.passive.} =
  echo "1. before suspend"
  let c = delay()
  resumeCont = c
  echo "2. between delay and suspend"
  suspend()
  echo "5. after suspend (resumed)"

proc main() {.passive.} =
  echo "calling suspendingProc"
  suspendingProc()
  echo "3. back in main"

main()
echo "4. after main"
resumeCont.complete()
echo "6. done"
