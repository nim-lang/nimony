import std/[syncio, atomics, assertions]

var loc: int
atomicStore(loc, 4)
assert atomicLoad(loc) == 4
atomicStore(loc, 2)
assert atomicLoad(loc, moRelaxed) == 2
atomicStore(loc, 9)
assert atomicLoad(loc, moAcquire) == 9
atomicStore(loc, 0, moRelease)
assert atomicLoad(loc) == 0

assert atomicExchange(loc, 7) == 0
assert atomicLoad(loc) == 7

var expected = 7
assert atomicCompareExchange(loc, expected, 5, moRelaxed, moRelaxed)
assert expected == 7
assert atomicLoad(loc) == 5

assert not atomicCompareExchange(loc, expected, 12, moRelaxed, moRelaxed)
assert expected == 5
assert atomicLoad(loc) == 5

assert atomicFetchAdd(loc, 1) == 5
assert atomicFetchAdd(loc, 2) == 6
assert atomicFetchSub(loc, 3) == 8

atomicInc(loc, 1)
assert atomicLoad(loc) == 6

var flag: AtomicFlag

assert not flag.testAndSet
assert flag.testAndSet
flag.clear(moRelaxed)
assert not flag.testAndSet

echo "atomics_basics: OK"
