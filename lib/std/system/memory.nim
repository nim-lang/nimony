## Low-level memory primitives and the default allocator for Nimony.
##
## The default allocator is the mimalloc shim (`include mimalloc`). A native
## allocator — a literal port of Nim 2's `lib/system/{alloc,osalloc}.nim`
## (page-chunk TLSF: segregated small cells, coalescing big chunks, huge mmap;
## owner-stamped lock-free deferred free for cross-thread deallocations) — is
## available behind `-d:nimNativeAlloc`. It is not yet the default: it still
## regresses a couple of arc tests (see project notes), so mimalloc stays the
## default until those are root-caused.
##
## The user-facing `alloc`/`dealloc`/`realloc`/`allocatedSize` wrappers are
## `func` (noSideEffect) so they remain usable inside the `func`s of pure data
## structures (see seqimpl.nim / stringimpl.nim). Mutating the per-thread heap
## is an implementation detail invisible to callers, so each wrapper launders
## the side-effecting MemRegion proc through `{.cast(noSideEffect).}`.
##
## Compile with `-d:nimNativeAlloc` to use the native ported allocator.

# --- C memory intrinsics (needed by the allocator, hence defined first) ----
func c_memcpy(dest, src: pointer; size: csize_t) {.importc: "memcpy", header: "<string.h>".}
func c_memcmp(a, b: pointer; size: csize_t): cint {.importc: "memcmp", header: "<string.h>".}
func c_memset(dest: pointer; val: cint; size: csize_t) {.importc: "memset", header: "<string.h>".}

func copyMem*(dest, src: pointer; size: int) {.inline.} =
  ## Copies `size` bytes from `src` to `dest`. The regions must not overlap.
  c_memcpy(dest, src, csize_t size)

func moveMem*(dest, src: pointer; size: int) =
  ## Copies `size` bytes from `src` to `dest`, correctly handling the case
  ## where the two regions overlap (unlike `copyMem`).
  ##
  ## Implemented entirely on top of `copyMem`/`memcpy` so the backend only has
  ## to support one memory-copy intrinsic. Disjoint regions (the common case)
  ## go straight through a single `copyMem`. Overlapping regions are copied in
  ## chunks that bounce through a small stack buffer: `src -> tmp -> dest`. The
  ## temporary is disjoint from both regions, so each `copyMem` is well-defined;
  ## the chunks are still processed in the direction that does not clobber bytes
  ## that later chunks still have to read (front-to-back when `dest` is below
  ## `src`, back-to-front otherwise).
  if size <= 0 or dest == src: return
  let d = cast[uint](dest)
  let s = cast[uint](src)
  if d + uint(size) <= s or s + uint(size) <= d:
    # regions are disjoint: a single memcpy is safe and fast
    c_memcpy(dest, src, csize_t size)
  else:
    const ChunkSize = 256
    var tmp {.noinit.}: array[ChunkSize, byte]
    let dp = cast[ptr UncheckedArray[byte]](dest)
    let sp = cast[ptr UncheckedArray[byte]](src)
    if d < s:
      # dest below src: each chunk's dest write stays below the next chunk's
      # src read, so go front-to-back.
      var off = 0
      while off < size:
        let c = min(ChunkSize, size - off)
        c_memcpy(addr tmp[0], addr sp[off], csize_t c)
        c_memcpy(addr dp[off], addr tmp[0], csize_t c)
        inc off, c
    else:
      # dest above src: go back-to-front.
      var off = size
      while off > 0:
        let c = min(ChunkSize, off)
        off -= c
        c_memcpy(addr tmp[0], addr sp[off], csize_t c)
        c_memcpy(addr dp[off], addr tmp[0], csize_t c)

func cmpMem*(a, b: pointer; size: int): int {.inline.} =
  ## Lexicographically compares `size` bytes at `a` and `b`.
  result = c_memcmp(a, b, csize_t size)

func zeroMem*(dest: pointer; size: int) {.inline.} =
  ## Sets `size` bytes at `dest` to zero.
  c_memset(dest, 0, csize_t size)

# --- optional memory budget (`-d:nimMaxHeap=10`) ---------------------------

const nimMaxHeap {.intdefine.}: int = 0
  ## Megabytes of heap this thread may hold at once. `0` (the default) means no
  ## limit and compiles the accounting away entirely.
  ##
  ## This is a LIVE budget: `dealloc` gives its bytes back, so a program that
  ## frees what it allocates keeps running. That is what makes the recovery
  ## paths observable -- a string that hits the cap releases its buffer and
  ## becomes the OOM cookie, which frees enough budget for the program to go on
  ## and report what happened. A cumulative "bytes ever handed out" counter
  ## cannot do that: once tripped, nothing can ever allocate again, not even to
  ## print the diagnosis.
  ##
  ## Everything is accounted at the allocator's USABLE size, on both ends, so
  ## the counter does not drift.
  ##
  ## It is a fault-injection and hardening knob: `tests/nimony/oom` uses it to
  ## reach the out-of-memory paths on purpose. See
  ## `doc/internals/failure_modes.md`.
  ##
  ## Name, unit and default are Nim's (`nimMaxHeap` in `lib/system/alloc.nim`),
  ## so `-d:nimMaxHeap=10` means the same thing to both compilers. The behaviour
  ## on hitting the cap differs: Nim checks it inside its page allocator and
  ## calls `raiseOutOfMem()`, which aborts, while here `alloc` returns `nil` so
  ## the recovery paths this runtime is built around actually run. It also
  ## applies to whichever allocator is in use, because it sits in the `alloc`
  ## wrappers rather than in one allocator's page layer.

when nimMaxHeap > 0:
  const NimMaxHeapBytes = nimMaxHeap * 1024 * 1024

  var liveMem {.threadvar.}: int

  func memBudgetTake(size: int): bool =
    ## Charges `size` bytes, or answers false and charges nothing when that
    ## would take this thread over the cap -- which is what makes `alloc`
    ## return `nil`. A negative `size` (a shrinking `realloc`) always succeeds.
    ## Written to stay inside `int` for any `size`.
    {.cast(noSideEffect).}:
      if size > 0 and liveMem > NimMaxHeapBytes - size:
        return false
      liveMem = liveMem + size
      return true

  func memBudgetGive(size: int) {.inline.} =
    ## Returns `size` bytes to the budget. Also used with a negative `size` to
    ## correct a charge upwards once the usable size is known.
    {.cast(noSideEffect).}:
      liveMem = liveMem - size

when not defined(nimNativeAlloc):
  # NOTE: `-d:valgrind` does nothing here. It instruments the NATIVE allocator
  # (`system/valgrind.nim`), and this build uses mimalloc, whose heap valgrind
  # already understands through mimalloc's own `-DMI_TRACK_VALGRIND=1`.
  #
  # This ought to be a `{.error.}` — a flag that silently produces a build which
  # looks instrumented and tracks nothing is the worst of both. Nimony rejects a
  # standalone `{.error.}` pragma inside the `system` include chain
  # ("unsupported pragma"), so the check cannot live here yet. `hastur
  # nativevalgrind`, which is how the flag is meant to be reached, always
  # compiles through `nimony n` and so cannot land in this branch.
  include mimalloc
else:
  # ----------------------------------------------------------------------
  # Prelude: the handful of symbols the ported allocator expects from the
  # rest of Nim's `system`/`mmdisp`. (Constants come from Nim's bitmasks.nim.)
  # ----------------------------------------------------------------------
  const
    PageShift = 12
    PageSize = 1 shl PageShift
    PageMask = PageSize - 1
    MemAlignShift = 4
    MemAlign = 1 shl MemAlignShift # 16
    BitsPerPage = PageSize div MemAlign
    UnitsPerPage = BitsPerPage div (sizeof(int) * 8)
    TrunkShift = 9
    BitsPerTrunk = 1 shl TrunkShift
    TrunkMask = BitsPerTrunk - 1
    IntsPerTrunk = BitsPerTrunk div (sizeof(int) * 8)
  when sizeof(int) == 8:
    const IntShift = 6
  else:
    const IntShift = 5
  const IntMask = (1 shl IntShift) - 1

  const
    overwriteFree = false
    coalescRight = true
    coalescLeft = true
    logAlloc = false
    hasThreadSupport = true    # owner-stamped lock-free deferred free
    reallyOsDealloc = false    # keep pages mapped (matches Nim's macosx/arm default)
    UseDestructors = true      # Nimony is always destructor-based; selects the
                               # gcDestructors code paths in the ported alloc.nim

  template sysAssert(cond, msg: untyped) = discard

  proc raiseOutOfMem() {.noinline, noreturn.} =
    # Reached only when the OS itself refuses to map pages; nothing to recover.
    # `noreturn` because `cAbort` is, and saying so is what lets a caller whose
    # only other arm assigns `result` be proved to initialise it — which is how
    # the bare-metal `osAllocPages` typechecks at all.
    cAbort()

  template `+!`(p: pointer; x: int): pointer = cast[pointer](cast[int](p) + x)
  template `-!`(p: pointer; x: int): pointer = cast[pointer](cast[int](p) - x)

  proc align(address, alignment: int): int {.inline.} =
    result = (address + (alignment - 1)) and not (alignment - 1)

  # unsigned-wraparound operators the allocator relies on
  template `+%`(x, y: int): int = cast[int](cast[uint](x) + cast[uint](y))
  template `-%`(x, y: int): int = cast[int](cast[uint](x) - cast[uint](y))
  template `*%`(x, y: int): int = cast[int](cast[uint](x) * cast[uint](y))
  template `%%`(x, y: int): int = cast[int](cast[uint](x) mod cast[uint](y))
  template `<%`(x, y: int): bool = cast[uint](x) < cast[uint](y)
  template `<=%`(x, y: int): bool = cast[uint](x) <= cast[uint](y)
  template `>%`(x, y: int): bool = cast[uint](x) > cast[uint](y)

  # The atomics the deferred-free path needs under `hasThreadSupport`
  # (`atomicLoadN`/`atomicStoreN`/`atomicExchangeN`/`atomicCompareExchangeN`
  # + `ATOMIC_*` orders) come from `system/atomintrin`, included earlier. The
  # explicit-`ptr T` builtin style also sidesteps Nimony's var-aliasing
  # rejection on `atomicCompareExchangeN(addr head, addr elem.next, ...)`.

  # alloc.nim's dirty templates and intrusive generics carry per-routine
  # `{.untyped.}` so their bodies are checked at instantiation. We deliberately
  # do NOT enable the `untyped` feature module-wide here — it would leak into
  # seqimpl/stringimpl and the mm strategy and miscompile their `{.cast(noSideEffect).}`.

  # --- the ported allocator (alloc.nim itself `include`s osalloc) ----------
  include "alloc"

  # --- user-facing API: `func` over a per-thread global MemRegion ----------
  var allocator {.threadvar.}: MemRegion

  when nimMaxHeap > 0:
    func budgetSize(p: pointer): int {.inline.} =
      ## The allocator's usable size for `p`. `ptrSize` is side-effecting and
      ## the budget only reads it, so launder it the way the wrappers below do.
      {.cast(noSideEffect).}: result = ptrSize(p)

  func alloc*(size: int): pointer =
    ## Allocates `size` bytes of uninitialized memory.
    when nimMaxHeap > 0:
      if not memBudgetTake(size): return nil
    {.cast(noSideEffect).}:
      result = alloc(allocator, size)
    when nimMaxHeap > 0:
      if result == nil: memBudgetGive(size)
      else: memBudgetGive(size - budgetSize(result))

  func alloc0*(size: int): pointer =
    ## Allocates `size` bytes of zero-initialized memory.
    when nimMaxHeap > 0:
      if not memBudgetTake(size): return nil
    {.cast(noSideEffect).}:
      result = alloc0(allocator, size)
    when nimMaxHeap > 0:
      if result == nil: memBudgetGive(size)
      else: memBudgetGive(size - budgetSize(result))

  func realloc*(p: pointer; size: int): pointer =
    ## Grows or shrinks the allocation `p` to `size` bytes, preserving contents.
    when nimMaxHeap > 0:
      let oldSize = if p != nil: budgetSize(p) else: 0
      # Authorize the delta BEFORE resizing: a `realloc` cannot be undone, so
      # the budget has to say no while the old block is still the only one.
      if not memBudgetTake(size - oldSize): return nil
    {.cast(noSideEffect).}:
      result = realloc(allocator, p, size)
    when nimMaxHeap > 0:
      if result == nil: memBudgetGive(size - oldSize)
      else: memBudgetGive(size - budgetSize(result))

  func dealloc*(p: pointer) =
    ## Frees memory previously returned by `alloc`/`alloc0`/`realloc`.
    when nimMaxHeap > 0:
      if p != nil: memBudgetGive(budgetSize(p))
    {.cast(noSideEffect).}:
      dealloc(allocator, p)

  func allocatedSize*(p: pointer): int =
    ## Usable bytes of the allocation `p` (what seq/string capacity relies on).
    {.cast(noSideEffect).}:
      result = ptrSize(p)

  proc getOccupiedMem*(): int =
    ## Bytes owned by the current thread's heap that currently hold data.
    result = getOccupiedMem(allocator)

  proc getFreeMem*(): int =
    ## Bytes owned by the current thread's heap that are free for reuse.
    result = getFreeMem(allocator)

  proc getTotalMem*(): int =
    ## Total bytes the current thread's heap has obtained from the OS.
    result = getTotalMem(allocator)

# --- fixed-size allocation (emitted by the compiler for `ref` objects) -----
func allocFixed*(size: int): pointer =
  ## Allocates `size` bytes of uninitialized memory (compiler `new`/ref hook).
  ## Not `inline`: the compiler emits cross-module references to this symbol
  ## from generated `=destroy` hooks, so it must have external linkage.
  result = alloc(size)

func deallocFixed*(p: pointer) =
  ## Frees memory allocated by `allocFixed`.
  dealloc(p)

# --- out-of-memory handling ------------------------------------------------
var missingBytes {.threadvar.}: int

proc continueAfterOutOfMem*(size: int) {.nimcall.} =
  ## Default out-of-memory handler: accumulates missing bytes so runtime code can react gracefully.
  if missingBytes < high(int) - size:
    missingBytes = missingBytes + size
  else:
    missingBytes = high(int)

proc threadOutOfMem*(): bool =
  ## True if the current thread previously ran out of memory (recorded by the OOM handler).
  missingBytes > 0

var oomHandler: proc (size: int) {.nimcall.} = continueAfterOutOfMem

proc setOomHandler*(handler: proc (size: int) {.nimcall.}) {.inline.} =
  ## Installs the procedure invoked when allocation fails (`size` is the attempted allocation).
  # XXX needs atomic store here
  oomHandler = handler
