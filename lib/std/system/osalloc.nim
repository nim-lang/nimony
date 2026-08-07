#
#
#            Nim's Runtime Library
#        (c) Copyright 2016 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

{.push raises: [], gcsafe.}

proc roundup(x, v: int): int {.inline.} =
  result = (x + (v-1)) and not (v-1)
  sysAssert(result >= x, "roundup: result < x")
  #return ((-x) and (v-1)) +% x

sysAssert(roundup(14, PageSize) == PageSize, "invalid PageSize")
sysAssert(roundup(15, 8) == 16, "roundup broken")
sysAssert(roundup(65, 8) == 72, "roundup broken 2")

# ------------ platform specific chunk allocation code -----------

# some platforms have really weird unmap behaviour:
# unmap(blockStart, PageSize)
# really frees the whole block. Happens for Linux/PowerPC for example. Amd64
# and x86 are safe though; Windows is special because MEM_RELEASE can only be
# used with a size of 0. We also allow unmapping to be turned off with
# -d:nimAllocNoUnmap:
const doNotUnmap = not (defined(amd64) or defined(i386)) or
                   defined(windows) or defined(nimAllocNoUnmap)


when defined(nimAllocPagesViaMalloc):
  when not defined(gcArc) and not defined(gcOrc) and not defined(gcAtomicArc) and not defined(gcYrc):
    {.error: "-d:nimAllocPagesViaMalloc is only supported with --mm:arc or --mm:atomicArc or --mm:orc or --mm:yrc".}

  proc osTryAllocPages(size: int): pointer {.inline.} =
    let base = c_malloc(csize_t size + PageSize - 1 + sizeof(uint32))
    if base == nil: raiseOutOfMem()
    # memory layout: padding + offset (4 bytes) + user_data
    # in order to deallocate: read offset at user_data - 4 bytes,
    # then deallocate user_data - offset
    let offset = PageSize - (cast[int](base) and (PageSize - 1))
    cast[ptr uint32](base +! (offset - sizeof(uint32)))[] = uint32(offset)
    result = base +! offset

  proc osAllocPages(size: int): pointer {.inline.} =
    result = osTryAllocPages(size)
    if result == nil: raiseOutOfMem()

  proc osDeallocPages(p: pointer, size: int) {.inline.} =
    # read offset at p - 4 bytes, then deallocate (p - offset) pointer
    let offset = cast[ptr uint32](p -! sizeof(uint32))[]
    c_free(p -! offset)

elif defined(emscripten) and not defined(StandaloneHeapSize):
  const
    PROT_READ  = 1             # page can be read
    PROT_WRITE = 2             # page can be written
    MAP_PRIVATE = 2'i32        # Changes are private

  var MAP_ANONYMOUS {.importc: "MAP_ANONYMOUS", header: "<sys/mman.h>".}: cint
  type
    PEmscriptenMMapBlock = ptr EmscriptenMMapBlock
    EmscriptenMMapBlock {.pure, inheritable.} = object
      realSize: int        # size of previous chunk; for coalescing
      realPointer: pointer     # if < PageSize it is a small chunk

  proc mmap(adr: pointer, len: int, prot, flags, fildes: cint,
            off: int): pointer {.header: "<sys/mman.h>".}

  proc munmap(adr: pointer, len: int) {.header: "<sys/mman.h>".}

  proc osAllocPages(block_size: int): pointer {.inline.} =
    let realSize = block_size + sizeof(EmscriptenMMapBlock) + PageSize + 1
    result = mmap(nil, realSize, PROT_READ or PROT_WRITE,
                             MAP_PRIVATE or MAP_ANONYMOUS, -1, 0)
    if result == nil or result == cast[pointer](-1):
      raiseOutOfMem()

    let realPointer = result
    let pos = cast[int](result)

    # Convert pointer to PageSize correct one.
    var new_pos = cast[int](pos) +% (PageSize - (pos %% PageSize))
    if (new_pos-pos) < sizeof(EmscriptenMMapBlock):
      new_pos = new_pos +% PageSize
    result = cast[pointer](new_pos)

    var mmapDescrPos = cast[int](result) -% sizeof(EmscriptenMMapBlock)

    var mmapDescr = cast[EmscriptenMMapBlock](mmapDescrPos)
    mmapDescr.realSize = realSize
    mmapDescr.realPointer = realPointer

    #c_fprintf(stdout, "[Alloc] size %d %d realSize:%d realPos:%d\n", block_size, cast[int](result), realSize, cast[int](realPointer))

  proc osTryAllocPages(size: int): pointer = osAllocPages(size)

  proc osDeallocPages(p: pointer, size: int) {.inline.} =
    var mmapDescrPos = cast[int](p) -% sizeof(EmscriptenMMapBlock)
    var mmapDescr = cast[EmscriptenMMapBlock](mmapDescrPos)
    munmap(mmapDescr.realPointer, mmapDescr.realSize)

elif defined(genode) and not defined(StandaloneHeapSize):
  include genode/alloc # osAllocPages, osTryAllocPages, osDeallocPages

elif defined(posix) and not defined(StandaloneHeapSize):
  const
    PROT_READ  = 1             # page can be read
    PROT_WRITE = 2             # page can be written

  when defined(netbsd) or defined(openbsd):
    # OpenBSD security for setjmp/longjmp coroutines
    var MAP_STACK {.importc: "MAP_STACK", header: "<sys/mman.h>".}: cint
  else:
    const MAP_STACK = 0             # avoid sideeffects

  when defined(macosx) or defined(freebsd):
    const MAP_ANONYMOUS = 0x1000
    const MAP_PRIVATE = 0x02        # Changes are private
  elif defined(solaris):
    const MAP_ANONYMOUS = 0x100
    const MAP_PRIVATE = 0x02        # Changes are private
  elif defined(linux):
    # asm-generic value, shared by amd64/arm64/i386 (all supported Linux
    # arches; mips/alpha/sparc diverge but std/posix rejects those upfront).
    const MAP_ANONYMOUS = 0x20
    const MAP_PRIVATE = 0x02        # Changes are private
  elif defined(haiku):
    const MAP_ANONYMOUS = 0x08
    const MAP_PRIVATE = 0x02
  else:  # posix including netbsd or openbsd
    var
      MAP_ANONYMOUS {.importc: "MAP_ANONYMOUS", header: "<sys/mman.h>".}: cint
      MAP_PRIVATE {.importc: "MAP_PRIVATE", header: "<sys/mman.h>".}: cint

  proc mmap(adr: pointer, len: csize_t, prot, flags, fildes: cint,
            off: int): pointer {.importc: "mmap".}

  proc munmap(adr: pointer, len: csize_t): cint {.importc: "munmap".}

  proc osAllocPages(size: int): pointer {.inline.} =
    result = mmap(nil, cast[csize_t](size), cint(PROT_READ or PROT_WRITE),
                             cint(MAP_ANONYMOUS or MAP_PRIVATE or MAP_STACK), cint(-1), 0)
    if result == nil or result == cast[pointer](-1):
      raiseOutOfMem()

  proc osTryAllocPages(size: int): pointer {.inline.} =
    result = mmap(nil, cast[csize_t](size), cint(PROT_READ or PROT_WRITE),
                             cint(MAP_ANONYMOUS or MAP_PRIVATE or MAP_STACK), cint(-1), 0)
    if result == cast[pointer](-1): result = nil

  proc osDeallocPages(p: pointer, size: int) {.inline.} =
    when reallyOsDealloc: discard munmap(p, cast[csize_t](size))

elif defined(windows) and not defined(StandaloneHeapSize):
  const
    # Typed `int32` to match `VirtualAlloc`/`VirtualFree`'s flag params (Nimony
    # does not implicitly narrow the default `int` literals to `int32`).
    MEM_RESERVE = 0x2000'i32
    MEM_COMMIT = 0x1000'i32
    MEM_TOP_DOWN = 0x100000'i32
    PAGE_READWRITE = 0x04'i32

    MEM_DECOMMIT = 0x4000'i32
    MEM_RELEASE = 0x8000'i32

  proc virtualAlloc(lpAddress: pointer, dwSize: int, flAllocationType,
                    flProtect: int32): pointer {.
                    stdcall, importc: "VirtualAlloc".}

  proc virtualFree(lpAddress: pointer, dwSize: int,
                   dwFreeType: int32): cint {.stdcall,
                   importc: "VirtualFree".}

  proc osAllocPages(size: int): pointer {.inline.} =
    result = virtualAlloc(nil, size, MEM_RESERVE or MEM_COMMIT,
                          PAGE_READWRITE)
    if result == nil: raiseOutOfMem()

  proc osTryAllocPages(size: int): pointer {.inline.} =
    result = virtualAlloc(nil, size, MEM_RESERVE or MEM_COMMIT,
                          PAGE_READWRITE)

  proc osDeallocPages(p: pointer, size: int) {.inline.} =
    # according to Microsoft, 0 is the only correct value for MEM_RELEASE:
    # This means that the OS has some different view over how big the block is
    # that we want to free! So, we cannot reliably release the memory back to
    # Windows :-(. We have to live with MEM_DECOMMIT instead.
    # Well that used to be the case but MEM_DECOMMIT fragments the address
    # space heavily, so we now treat Windows as a strange unmap target.
    when reallyOsDealloc:
      if virtualFree(p, 0, MEM_RELEASE) == 0:
        cprintf "virtualFree failing!"
        rawQuit 1
    #VirtualFree(p, size, MEM_DECOMMIT)

elif defined(wasm32) and defined(standalone):
  # Growing page provider over wasm linear memory (ward-bridge B0): the
  # allocator draws pages from the END of linear memory, growing it on
  # demand via memory.grow — no fixed reservation, so a small module
  # stays small and a viewer's caches can budget in the hundreds of MB.
  # An optional CEILING (set by the host at startup from device signals —
  # D8: budgets are device-derived) turns growth failures into ordinary
  # out-of-memory before the browser's own tab limit does it for us.
  const WasmPageSize = 65536
  var
    wasmBump: int = 0        # next free byte; 64 KiB-aligned start (> PageSize)
    heapCeiling: int = 0     # 0 = uncapped (the host's tab limit rules)

  proc wasmMemorySize(): int32 {.importc: "__builtin_wasm_memory_size".}
  proc wasmMemoryGrow(delta: int32): int32 {.importc: "__builtin_wasm_memory_grow".}

  proc setWasmHeapCeiling*(bytes: int) =
    ## Host-set upper bound on total linear memory (bytes; 0 = uncapped).
    ## Public Nim API; apps that let the host set it export a main-module
    ## wrapper (ithaqua export roots are main-module exportc procs only).
    heapCeiling = bytes

  proc ensureCapacity(needEnd: int): bool =
    let haveEnd = int(wasmMemorySize()) * WasmPageSize
    if needEnd <= haveEnd: return true
    if heapCeiling > 0 and needEnd > heapCeiling: return false
    let deltaPages = (needEnd - haveEnd + WasmPageSize - 1) div WasmPageSize
    result = wasmMemoryGrow(int32(deltaPages)) >= 0

  proc osAllocPages(size: int): pointer {.inline.} =
    if wasmBump == 0:
      # first use: start at the current end of memory (above the module's
      # static data + shadow stack), 64 KiB-page aligned by construction
      wasmBump = int(wasmMemorySize()) * WasmPageSize
    if not ensureCapacity(wasmBump + size):
      raiseOutOfMem()
    result = cast[pointer](wasmBump)
    inc wasmBump, size

  proc osTryAllocPages(size: int): pointer {.inline.} =
    result = nil   # explicit: nimony's init prover rejects the implicit zero
    if wasmBump == 0:
      wasmBump = int(wasmMemorySize()) * WasmPageSize
    if not ensureCapacity(wasmBump + size): return nil
    result = cast[pointer](wasmBump)
    inc wasmBump, size

  proc osDeallocPages(p: pointer, size: int) {.inline.} =
    # bump arena: only the most recent block can be returned
    if wasmBump - size == cast[int](p):
      dec wasmBump, size

elif defined(standalone) or defined(StandaloneHeapSize):
  # nimony has no {.intdefine.}; plain const for now (config override can
  # return as a proper -d: hook when someone needs a different heap size).
  # 128 MB: generous for native-standalone test binaries (virtual .bss).
  # The wasm32 target uses the growing memory.grow provider above.
  const StandaloneHeapSize: int = 32768 * PageSize
  var
    theHeap: array[StandaloneHeapSize div sizeof(float64), float64] # 'float64' for alignment
    # The allocator above derives chunk headers by masking pointers down to
    # PageSize boundaries (pageAddr), so the page provider MUST hand out
    # page-ALIGNED addresses — mmap guarantees that on the hosted targets,
    # the standalone heap has to round up itself (costs < one page).
    bumpPointer = (cast[int](addr theHeap) + PageSize - 1) and not (PageSize - 1)

  proc osAllocPages(size: int): pointer {.inline.} =
    if size+bumpPointer < cast[int](addr theHeap) + sizeof(theHeap):
      result = cast[pointer](bumpPointer)
      inc bumpPointer, size
    else:
      raiseOutOfMem()

  proc osTryAllocPages(size: int): pointer {.inline.} =
    result = nil   # explicit: nimony's init prover rejects the implicit zero
    if size+bumpPointer < cast[int](addr theHeap) + sizeof(theHeap):
      result = cast[pointer](bumpPointer)
      inc bumpPointer, size

  proc osDeallocPages(p: pointer, size: int) {.inline.} =
    if bumpPointer-size == cast[int](p):
      dec bumpPointer, size

else:
  {.error: "Port memory manager to your platform".}

{.pop.}
