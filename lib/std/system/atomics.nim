
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

func atomicAddFetch*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  importc: "__atomic_add_fetch", nodecl.}
func atomicSubFetch*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  importc: "__atomic_sub_fetch", nodecl.}
func atomicLoadN*[T](p: ptr T, mem: AtomMemModel): T {.
  importc: "__atomic_load_n", nodecl.}

func arcInc*(memLoc: var int) {.inline.} =
  ## Atomically increments the reference count.
  {.cast(noSideEffect).}:
    discard atomicAddFetch(memLoc.addr, 1, ATOMIC_SEQ_CST)

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Atomically decrements the reference count. Returns true when it reaches zero.
  {.cast(noSideEffect).}:
    result = atomicSubFetch(memLoc.addr, 1, ATOMIC_SEQ_CST) < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Atomically loads the reference count and returns true if it equals 1.
  {.cast(noSideEffect).}:
    result = atomicLoadN(memLoc.addr, ATOMIC_ACQUIRE) == 1
