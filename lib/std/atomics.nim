## Atomic operations for Nimony.
## Uses GCC/Clang `__atomic_*` builtins directly -- no `_Atomic` types
## in the generated C code. Atomics are operations, not type properties.

type
  MemoryOrder* = enum
    moRelaxed
    moConsume
    moAcquire
    moRelease
    moAcquireRelease
    moSequentiallyConsistent

  Trivial* = SomeInteger | bool | enum | ptr | pointer

  AtomicFlag* = distinct bool

template toMem(order: MemoryOrder): cint =
  cint(ord(order))

proc builtinLoadN[T](p: ptr T; mem: cint): T {.intrinsic: "AtomicLoad".}
proc builtinStoreN[T](p: ptr T; val: T; mem: cint) {.intrinsic: "AtomicStore".}
proc builtinExchangeN[T](p: ptr T; val: T; mem: cint): T {.intrinsic: "AtomicExchange".}
proc builtinCompareExchangeN[T](p: ptr T; expected: ptr T; desired: T;
  weak: bool; succ, fail: cint): bool {.intrinsic: "AtomicCompareExchange".}

proc builtinFetchAdd[T](p: ptr T; val: T; mem: cint): T {.intrinsic: "AtomicFetchAdd".}
proc builtinFetchSub[T](p: ptr T; val: T; mem: cint): T {.intrinsic: "AtomicFetchSub".}
proc builtinFetchAnd[T](p: ptr T; val: T; mem: cint): T {.intrinsic: "AtomicFetchAnd".}
proc builtinFetchOr[T](p: ptr T; val: T; mem: cint): T {.intrinsic: "AtomicFetchOr".}
proc builtinFetchXor[T](p: ptr T; val: T; mem: cint): T {.intrinsic: "AtomicFetchXor".}

# The flag pair takes a bare `pointer`: a flag is a byte with no pointee type to
# read a width from. No native back end lowers them yet — the rows' `targets` is
# empty, so a native build reports that at the call site instead of guessing.
proc builtinTestAndSet(p: pointer; mem: cint): bool {.intrinsic: "AtomicTestAndSet".}
proc builtinClear(p: pointer; mem: cint) {.intrinsic: "AtomicClear".}

proc builtinThreadFence(mem: cint) {.intrinsic: "AtomicThreadFence".}
proc builtinSignalFence(mem: cint) {.intrinsic: "AtomicSignalFence".}

# The spin-wait hint. A row rather than `{.emit.}` for the reason the atomics are
# rows: `emit` is a C-backend feature, so a native build had nothing to lower and
# `cpuRelax` was the one thing in this module `nimony n` could not compile at all.
proc builtinCpuRelax() {.intrinsic: "CpuRelax".}

# Access operations

proc atomicLoad*[T: Trivial](location: var T;
    order: MemoryOrder = moSequentiallyConsistent): T {.inline.} =
  builtinLoadN(addr(location), toMem(order))

proc atomicStore*[T: Trivial](location: var T; desired: T;
    order: MemoryOrder = moSequentiallyConsistent) {.inline.} =
  builtinStoreN(addr(location), desired, toMem(order))

proc atomicExchange*[T: Trivial](location: var T; desired: T;
    order: MemoryOrder = moSequentiallyConsistent): T {.inline.} =
  builtinExchangeN(addr(location), desired, toMem(order))

proc atomicCompareExchange*[T: Trivial](location: var T; expected: var T;
    desired: T; success, failure: MemoryOrder): bool {.inline.} =
  builtinCompareExchangeN(addr(location), addr(expected), desired, false,
    toMem(success), toMem(failure))

proc atomicCompareExchange*[T: Trivial](location: var T; expected: var T;
    desired: T;
    order: MemoryOrder = moSequentiallyConsistent): bool {.inline.} =
  atomicCompareExchange(location, expected, desired, order, order)

proc atomicCompareExchangeWeak*[T: Trivial](location: var T; expected: var T;
    desired: T; success, failure: MemoryOrder): bool {.inline.} =
  builtinCompareExchangeN(addr(location), addr(expected), desired, true,
    toMem(success), toMem(failure))

proc atomicCompareExchangeWeak*[T: Trivial](location: var T; expected: var T;
    desired: T;
    order: MemoryOrder = moSequentiallyConsistent): bool {.inline.} =
  atomicCompareExchangeWeak(location, expected, desired, order, order)

# Numerical operations

proc atomicFetchAdd*[T: SomeInteger](location: var T; value: T;
    order: MemoryOrder = moSequentiallyConsistent): T {.inline.} =
  builtinFetchAdd(addr(location), value, toMem(order))

proc atomicFetchSub*[T: SomeInteger](location: var T; value: T;
    order: MemoryOrder = moSequentiallyConsistent): T {.inline.} =
  builtinFetchSub(addr(location), value, toMem(order))

proc atomicFetchAnd*[T: SomeInteger](location: var T; value: T;
    order: MemoryOrder = moSequentiallyConsistent): T {.inline.} =
  builtinFetchAnd(addr(location), value, toMem(order))

proc atomicFetchOr*[T: SomeInteger](location: var T; value: T;
    order: MemoryOrder = moSequentiallyConsistent): T {.inline.} =
  builtinFetchOr(addr(location), value, toMem(order))

proc atomicFetchXor*[T: SomeInteger](location: var T; value: T;
    order: MemoryOrder = moSequentiallyConsistent): T {.inline.} =
  builtinFetchXor(addr(location), value, toMem(order))

# Flag operations

proc testAndSet*(location: var AtomicFlag;
    order: MemoryOrder = moSequentiallyConsistent): bool {.inline.} =
  builtinTestAndSet(addr(location), toMem(order))

proc clear*(location: var AtomicFlag;
    order: MemoryOrder = moSequentiallyConsistent) {.inline.} =
  builtinClear(addr(location), toMem(order))

# Fences

proc atomicFence*(order: MemoryOrder) {.inline.} =
  builtinThreadFence(toMem(order))

proc atomicSignalFence*(order: MemoryOrder) {.inline.} =
  builtinSignalFence(toMem(order))

# CPU pause hint for spin loops

proc cpuRelax*() {.inline.} =
  ## Hints the CPU that we are in a spin-wait loop.
  ## Reduces power consumption and avoids memory-order violations
  ## on hyper-threaded cores.
  ##
  ## `pause` on x86-64, `yield` on AArch64, and nothing at all on a target with
  ## neither — a hint that does not exist is a hint that can be skipped, so no
  ## `when` guards this call and no target has to be enumerated here.
  builtinCpuRelax()

# Convenience

proc atomicInc*[T: SomeInteger](location: var T; value: T = 1) {.inline.} =
  discard atomicFetchAdd(location, value)

proc atomicDec*[T: SomeInteger](location: var T; value: T = 1) {.inline.} =
  discard atomicFetchSub(location, value)
