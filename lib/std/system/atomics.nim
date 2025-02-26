
type AtomMemModel* = distinct cint

var ATOMIC_RELAXED* {.importc: "__ATOMIC_RELAXED", nodecl.}: AtomMemModel
  ## No barriers or synchronization.
var ATOMIC_CONSUME* {.importc: "__ATOMIC_CONSUME", nodecl.}: AtomMemModel
  ## Data dependency only for both barrier and
  ## synchronization with another thread.
var ATOMIC_ACQUIRE* {.importc: "__ATOMIC_ACQUIRE", nodecl.}: AtomMemModel
  ## Barrier to hoisting of code and synchronizes with
  ## release (or stronger)
  ## semantic stores from another thread.
var ATOMIC_RELEASE* {.importc: "__ATOMIC_RELEASE", nodecl.}: AtomMemModel
  ## Barrier to sinking of code and synchronizes with
  ## acquire (or stronger)
  ## semantic loads from another thread.
var ATOMIC_ACQ_REL* {.importc: "__ATOMIC_ACQ_REL", nodecl.}: AtomMemModel
  ## Full barrier in both directions and synchronizes
  ## with acquire loads
  ## and release stores in another thread.
var ATOMIC_SEQ_CST* {.importc: "__ATOMIC_SEQ_CST", nodecl.}: AtomMemModel
  ## Full barrier in both directions and synchronizes
  ## with acquire loads
  ## and release stores in all threads.

proc atomicAddFetch*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  importc: "__atomic_add_fetch", nodecl.}
proc atomicSubFetch*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  importc: "__atomic_sub_fetch", nodecl.}

proc arcInc*(memLoc: var int) {.inline.} =
  ## Atomically increments the integer by some `x`.
  discard atomicAddFetch(memLoc.addr, 1, ATOMIC_SEQ_CST)

proc arcDec*(memLoc: var int): bool {.inline.} =
  ## Atomically decrements the integer by some `x`. It returns the new value.
  result = atomicSubFetch(memLoc.addr, 1, ATOMIC_SEQ_CST) < 0
