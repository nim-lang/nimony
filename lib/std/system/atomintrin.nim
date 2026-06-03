## GCC/Clang `__atomic_*` builtin layer, shared by the allocator (memory.nim)
## and the ARC reference-counting ops (arcops.nim). Kept as a leaf include so
## both consumers see one set of declarations regardless of include order.

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
func atomicStoreN*[T](p: ptr T, val: T, mem: AtomMemModel) {.
  importc: "__atomic_store_n", nodecl.}
func atomicExchangeN*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  importc: "__atomic_exchange_n", nodecl.}
func atomicCompareExchangeN*[T](p: ptr T, expected: ptr T, desired: T,
    weak: bool, succ, fail: AtomMemModel): bool {.
  importc: "__atomic_compare_exchange_n", nodecl.}
