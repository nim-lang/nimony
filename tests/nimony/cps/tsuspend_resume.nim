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

# `main` parks and is resumed by this same thread, so it is started as a
# continuation: a plain `main()` would run it to completion, and wait for a
# resume that only this thread could perform.
complete(delay main())
echo "4. after main"
resumeCont.complete()
echo "6. done"
