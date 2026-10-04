## Test: `std/contextvars` — dynamic scoping that follows a `.passive` proc
## through a park and across a hop to a pool thread.
##
## The interesting part is not the push and pop, it is that a coroutine's
## context lives on its frame, and only on the frames that need one: a `.passive`
## proc that writes a context gets a slot of its own, and one that only reads
## shares whatever encloses it. Nothing in this file says which is which. Every
## case below that goes through `withCtx` and then parks, or is resumed on a
## different thread than it started on, fails if the context is kept in a
## `.threadvar.` instead.

import std / [contextvars, syncio, threadpool, atomics]

var plain = newContextVar(10)
var untyped1: ContextVar[int]  ## a bare `var`: no default, `get` raises
var untyped2: ContextVar[int]  ## a second bare `var`, to keep the ids apart
var name = newContextVar("nobody")

proc show(tag: string; v: ContextVar[int]; alt: int) =
  ## `v` in the running context, or `alt` where it is bound nowhere.
  ##
  ## The value is worked out before it is printed rather than in the `echo`
  ## itself: `echo` writes its arguments as it goes, so an `echo tag, v.get()`
  ## prints `tag`, raises, and the handler prints `tag` a second time.
  var text: string
  try:
    text = $v.get()
  except ErrorCode as e:
    case e
    of KeyError: text = $alt
    else: text = "unexpected " & $e
  echo tag, text

proc showName(tag: string) =
  var text: string
  try:
    text = name.get()
  except ErrorCode as e:
    text = "unexpected " & $e
  echo tag, text

# --- plain scoping, no coroutine involved ---

proc regular =
  show("default: ", plain, -1)
  plain.set(20)
  show("after set: ", plain, -1)
  withCtx(plain, 30):
    show("in block: ", plain, -1)
    withCtx(plain, 40):
      show("in nested: ", plain, -1)
    show("back one: ", plain, -1)
  show("after block: ", plain, -1)

# --- a callee's `set` must not reach its caller ---

proc calleeSets() {.passive.} =
  plain.set(99)
  show("callee sees: ", plain, -1)

proc callerOfSetter() {.passive.} =
  plain.set(50)
  calleeSets()
  show("caller still sees: ", plain, -1)

# --- a call inherits the caller's chain, `get` still raises where it should ---

proc reader() {.passive.} =
  show("inherited: ", plain, -1)
  show("inherited bare: ", untyped1, -1)
  showName("inherited name: ")

proc callerOfReader() {.passive.} =
  show("before call: ", untyped1, -1)
  plain.set(70)
  name.set("outer")
  reader()

# --- a park, and a resume on another thread ---

var resumeCont: Continuation
var finished: int  # accessed atomically
var seenAfterResume, seenAfterBlock: int
  ## What `parked` saw after the resume. Written by a pool worker and read by
  ## the thread that waits for `finished`, which orders them. Nothing is printed
  ## from the worker: `echo` buffers per thread, so output from a worker can
  ## still be sitting in the buffer when the process exits, and a golden that
  ## depends on winning that race is a flaky golden.

proc parked() {.passive.} =
  plain.set(80)
  withCtx(plain, 90):
    show("before park: ", plain, -1)
    resumeCont = delay()
    suspend()
    seenAfterResume = plain.getOrDefault(-1)
  seenAfterBlock = plain.getOrDefault(-1)
  discard atomicFetchAdd(finished, 1, moRelease)

proc drive(c: Continuation) =
  ## Runs `c` until it finishes or parks. `complete` would wait out the park,
  ## for a resume that only the pool can perform.
  var c = c
  while not stopping(c): c = advance(c)

# --- a coroutine that only reads shares the slot around it ---

proc sharedReader() {.passive.} =
  ## This proc only READS a context, so the compiler found no `ctxSlot` in its
  ## body and gave it no slot of its own: what it sees is whatever encloses it.
  ## That is the free half of the rule -- a coroutine that does not care about
  ## its dynamic context pays nothing for one and writes nothing to say so -- and
  ## the case below fails outright if the frame grew one anyway, because the
  ## caller would then read back its own 40 through a slot it had adopted.
  show("shared sees: ", plain, -1)
  showName("shared name: ")

proc callerOfShared() {.passive.} =
  plain.set(40)
  sharedReader()
  show("caller after shared: ", plain, -1)

# --- the two bare vars are two variables ---

proc distinctVars() {.passive.} =
  untyped1.set(1)
  untyped2.set(2)
  show("first: ", untyped1, -1)
  show("second: ", untyped2, -1)

# --- a GC-managed value survives a round trip ---

proc strings() {.passive.} =
  # Long enough to sit on the heap rather than in a ref's short-string area,
  # so a `set` that dropped the reference or a `get` that copied carelessly
  # would show up here rather than in a `.output` that happens to match.
  name.set("a reasonably long string that will not sit in the short-string area")
  showName("outer name: ")
  withCtx(name, "another long string, also past the short-string limit"):
    showName("nested name: ")
  showName("outer name again: ")

# --- a heap-allocated frame ---

proc recursing(n: int) {.passive.} =
  ## A `.passive` proc that calls itself gets a frame from the heap rather than
  ## from its caller's scope, and `deallocFrame` frees it -- so this is the path
  ## where the chain the frame is holding has to be released on the way out.
  withCtx(plain, n):
    if n > 0: recursing(n - 1)
    show("recursed to: ", plain, -1)

proc callerOfRecursion() {.passive.} =
  recursing(3)
  show("after recursion: ", plain, -1)

# --- a coroutine started by `complete` ---

var viaComplete = newContextVar(0)

proc completeLeaf() {.passive.} =
  show("complete inherits: ", viaComplete, -1)
  viaComplete.set(61)
  show("complete after set: ", viaComplete, -1)

proc startedByComplete =
  ## `complete` hands its continuation a `Join` as the caller rather than a real
  ## frame, and a `Join` carries no chain of its own. So the chain has to be
  ## found from further up than the immediate caller. `startedByComplete` is a
  ## regular proc, so its `set` landed on the thread's chain, and it has to be
  ## visible in the callee.
  ##
  ## A `Join` of its own gets a context var that no other test reads, so that
  ## the `set` here cannot reach back into the cases above.
  viaComplete.set(60)
  complete(delay completeLeaf())
  show("after complete: ", viaComplete, -1)

proc main =
  regular()
  # `regular` is not part of any coroutine, so its `set` landed on the thread's
  # own chain and outlives it. `getOrDefault` answers instead of raising, which
  # is what a top-level statement can afford.
  echo "after regular: ", plain.getOrDefault(-1)

  drive(delay callerOfSetter())
  drive(delay callerOfReader())
  drive(delay callerOfShared())
  drive(delay distinctVars())
  drive(delay callerOfRecursion())
  drive(delay strings())

  initPool()
  drive(delay parked())
  echo "parked on the main thread"
  submit(resumeCont, 0)  # resumes on a pool thread, not the one that parked
  while atomicLoad(finished, moAcquire) == 0: discard
  shutdownPool()
  echo "after resume, on the worker: ", seenAfterResume
  echo "after the block, on the worker: ", seenAfterBlock
  echo "resumed and finished"
  startedByComplete()

main()
