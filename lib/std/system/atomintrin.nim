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

# `{.intrinsic: "AtomicX".}`, not `{.importc: "__atomic_x".}`: an atomic is an
# OPCODE, and the C builtin is one spelling of it rather than its definition. The
# C back end still emits exactly `__atomic_add_fetch(...)` (see `cBuiltinFor`), so
# nothing about the generated C changes; what changes is that a native back end
# now sees `(instr …)` and knows this is an inline instruction sequence, not a
# call. Calling it a call cost every consumer a special case — an ABI call point
# where none exists, a frame it does not need, a clobber set it does not have.
#
# The `mem` argument stays in the signature because the C builtin takes it. The
# native lowerings are sequentially consistent regardless (see `IntrinsicRows`),
# so they do not evaluate it.

func atomicAddFetch*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  intrinsic: "AtomicAddFetch".}
func atomicSubFetch*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  intrinsic: "AtomicSubFetch".}
func atomicLoadN*[T](p: ptr T, mem: AtomMemModel): T {.
  intrinsic: "AtomicLoad".}
func atomicStoreN*[T](p: ptr T, val: T, mem: AtomMemModel) {.
  intrinsic: "AtomicStore".}
func atomicExchangeN*[T](p: ptr T, val: T, mem: AtomMemModel): T {.
  intrinsic: "AtomicExchange".}
func atomicCompareExchangeN*[T](p: ptr T, expected: ptr T, desired: T,
    weak: bool, succ, fail: AtomMemModel): bool {.
  intrinsic: "AtomicCompareExchange".}
