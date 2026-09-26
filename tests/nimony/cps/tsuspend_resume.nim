import std/syncio

proc drive(c: Continuation) =
  ## Runs `c` until it finishes or parks. `complete` would wait out the park,
  ## for a resume that only this thread can perform.
  var c = c
  while not stopping(c): c = advance(c)

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

drive(delay main())  # parks; a plain `main()` would wait for the resume below
echo "4. after main"
drive(resumeCont)
echo "6. done"
