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

# GCC/Clang __atomic builtins. They operate on any 1/2/4/8-byte type directly.

proc builtinLoadN[T](p: ptr T; mem: cint): T {.
  importc: "__atomic_load_n", nodecl.}
proc builtinStoreN[T](p: ptr T; val: T; mem: cint) {.
  importc: "__atomic_store_n", nodecl.}
proc builtinExchangeN[T](p: ptr T; val: T; mem: cint): T {.
  importc: "__atomic_exchange_n", nodecl.}
proc builtinCompareExchangeN[T](p: ptr T; expected: ptr T; desired: T;
    weak: bool; succ, fail: cint): bool {.
  importc: "__atomic_compare_exchange_n", nodecl.}

proc builtinFetchAdd[T](p: ptr T; val: T; mem: cint): T {.
  importc: "__atomic_fetch_add", nodecl.}
proc builtinFetchSub[T](p: ptr T; val: T; mem: cint): T {.
  importc: "__atomic_fetch_sub", nodecl.}
proc builtinFetchAnd[T](p: ptr T; val: T; mem: cint): T {.
  importc: "__atomic_fetch_and", nodecl.}
proc builtinFetchOr[T](p: ptr T; val: T; mem: cint): T {.
  importc: "__atomic_fetch_or", nodecl.}
proc builtinFetchXor[T](p: ptr T; val: T; mem: cint): T {.
  importc: "__atomic_fetch_xor", nodecl.}

proc builtinTestAndSet(p: pointer; mem: cint): bool {.
  importc: "__atomic_test_and_set", nodecl.}
proc builtinClear(p: pointer; mem: cint) {.
  importc: "__atomic_clear", nodecl.}

proc builtinThreadFence(mem: cint) {.
  importc: "__atomic_thread_fence", nodecl.}
proc builtinSignalFence(mem: cint) {.
  importc: "__atomic_signal_fence", nodecl.}

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

# Convenience

proc atomicInc*[T: SomeInteger](location: var T; value: T = 1) {.inline.} =
  discard atomicFetchAdd(location, value)

proc atomicDec*[T: SomeInteger](location: var T; value: T = 1) {.inline.} =
  discard atomicFetchSub(location, value)
