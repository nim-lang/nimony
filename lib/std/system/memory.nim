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

when not defined(nimNativeAlloc):
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

  proc raiseOutOfMem() {.noinline.} =
    # Reached only when the OS itself refuses to map pages; nothing to recover.
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
  # seqimpl/stringimpl/arcops and miscompile their `{.cast(noSideEffect).}`.

  # --- the ported allocator (alloc.nim itself `include`s osalloc) ----------
  include "alloc"

  # --- user-facing API: `func` over a per-thread global MemRegion ----------
  var allocator {.threadvar.}: MemRegion

  func alloc*(size: int): pointer =
    ## Allocates `size` bytes of uninitialized memory.
    {.cast(noSideEffect).}:
      result = alloc(allocator, size)

  func alloc0*(size: int): pointer =
    ## Allocates `size` bytes of zero-initialized memory.
    {.cast(noSideEffect).}:
      result = alloc0(allocator, size)

  func realloc*(p: pointer; size: int): pointer =
    ## Grows or shrinks the allocation `p` to `size` bytes, preserving contents.
    {.cast(noSideEffect).}:
      result = realloc(allocator, p, size)

  func dealloc*(p: pointer) =
    ## Frees memory previously returned by `alloc`/`alloc0`/`realloc`.
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
var
  missingBytes {.threadvar.}: int

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
