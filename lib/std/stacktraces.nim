## Stack traces for the native (arkham + nifasm) backend.
##
## The walk itself is four lines, because arkham's frames are regular: the
## prologue lowers SP **once** and SP is constant until the epilogue
## (`nativenif/src/arkham/design.md`). So "where is my return address" is a
## per-PROC constant, and nifasm writes those constants — with each proc's code
## range and name — into a table at the end of `.text`
## (`nativenif/src/nifasm/tracetable.nim`). Given that table, unwinding is
## repeated address lookup; no DWARF interpreter, no frame pointer, and nothing
## that allocates before it has something to say.
##
## Two machine facts are all this needs from the back end, and each is one
## instruction:
##
## * `traceTable()` — a RIP-relative `lea` at the table. Referencing it is also
##   what makes nifasm emit the table at all, so a program that never asks for a
##   stack trace carries none of this.
## * `stackPointer()` inside a `{.naked.}` proc — with no prologue, SP on entry
##   still points at the return address the `call` pushed, which is the CALLER's
##   frame. A proc with a prologue can only describe its own.

const maxStackTraceFrames* = 128
  ## A hard bound on the walk. A stack trace is usually wanted while the program
  ## is already misbehaving, and a corrupt frame chain must produce a short trace
  ## rather than an endless one. A truncated trace ends with a `...` line.

const stackTracesAvailable* = defined(nimNoLibc) and defined(amd64)
  ## Whether `getStackTrace` can answer. False on the C backend — its frames are
  ## the C compiler's, described by its unwind tables, and reading those is a
  ## different implementation rather than a missing branch of this one.

when stackTracesAvailable:
  proc stackPointer(): pointer {.intrinsic: "StackPointer".}
    ## The value of SP, copied into a register.

  proc traceTable(): pointer {.intrinsic: "TraceTable".}
    ## The address of nifasm's per-proc metadata blob.

  proc callerFrame(): pointer {.assembler, naked.} =
    ## The address of OUR CALLER's return-address slot. `{.naked.}` is what makes
    ## this true: with no prologue, SP on entry is exactly where the `call`
    ## pushed the return address, so the value handed back describes the frame
    ## above rather than this one.
    result = stackPointer()

  const
    TraceMagic = 0x4352544E'u32   ## 'N','T','R','C'
    HeaderSize = 16
    EntrySize = 16
    MaxStackSpan = 256'u * 1024 * 1024
      ## How far up the stack the walk may travel before it declares the chain
      ## broken. The frame cap alone is not quite enough: the last `pc` a walk
      ## reads is whatever the entry stub left above `main`'s frame, and if that
      ## garbage happens to land inside some proc's code range, the walk
      ## continues with that proc's (perfectly valid) frame size into memory
      ## that is no longer a stack. Faulting is the one thing a stack trace must
      ## not do, since it is usually being taken because something already went
      ## wrong. The bound is far above any stack anyone configures.

  proc readU32(base: uint; off: int): uint32 {.inline.} =
    cast[ptr uint32](base + uint(off))[]

  proc readWord(at: uint): uint {.inline.} =
    cast[ptr uint](at)[]

  type
    TraceTableRef = object
      ## The table's address plus its header, read once per walk.
      base: uint      ## every offset in the table is relative to this
      count: int

  proc openTraceTable(): TraceTableRef =
    result = TraceTableRef(base: 0, count: 0)
    let base = cast[uint](traceTable())
    if base == 0: return
    if readU32(base, 0) != TraceMagic: return
    result = TraceTableRef(base: base, count: int(readU32(base, 8)))

  proc signed32(v: uint32): int {.inline.} =
    ## Two's-complement widening, spelled out. A stored offset is 32 bits (a
    ## program's text is) and `int` is 64, and the widening is done here by hand
    ## rather than through `int(cast[int32](v))` because it is the one step where
    ## a zero-extension instead of a sign-extension turns every backward offset
    ## into an address four gigabytes away — a failure that reads as "no such
    ## proc" rather than as a wrong number.
    result = int(v)
    if result >= 2147483648: result = result - 4294967296

  proc entryStart(t: TraceTableRef; i: int): uint {.inline.} =
    ## The runtime address of entry `i`'s first code byte. The stored field is a
    ## signed distance from the table, which is what lets the table need no
    ## relocation: it is correct under PIE, ASLR and every image base.
    let off = signed32(readU32(t.base, HeaderSize + i * EntrySize))
    cast[uint](cast[int](t.base) + off)

  proc entryLen(t: TraceTableRef; i: int): uint {.inline.} =
    uint(readU32(t.base, HeaderSize + i * EntrySize + 4))

  proc entryCfaOff(t: TraceTableRef; i: int): uint {.inline.} =
    ## `CFA = SP + this` anywhere past the proc's prologue. Since the return
    ## address sits at `CFA - 8` and a `call` pushed it at `SP - 8`, this is also
    ## the distance from one frame's return-address slot to the next one's.
    uint(readU32(t.base, HeaderSize + i * EntrySize + 8))

  proc entryName(t: TraceTableRef; i: int): string =
    ## The proc's NIF symbol, copied out of the table's name blob.
    let at = t.base + uint(readU32(t.base, HeaderSize + i * EntrySize + 12))
    let s = cast[ptr UncheckedArray[char]](at)
    result = ""
    var k = 0
    while s[k] != '\0':
      result.add s[k]
      inc k

  proc findProc(t: TraceTableRef; pc: uint): int =
    ## The entry whose code range contains `pc`, or -1. The rows are sorted by
    ## address, so this is a binary search — the walk does one per frame and a
    ## linear scan would make a deep trace quadratic in the size of the program.
    result = -1
    var lo = 0
    var hi = t.count - 1
    while lo <= hi:
      let mid = (lo + hi) div 2
      let start = entryStart(t, mid)
      if pc < start:
        hi = mid - 1
      elif pc >= start + entryLen(t, mid):
        lo = mid + 1
      else:
        return mid

  proc addPrettyName(dest: var string; sym: string) =
    ## `foo.3.mymod` → `foo`. A NIF symbol is `name.<disambiguator>[.<module>]`,
    ## and neither trailing part helps a reader: the disambiguator is an internal
    ## counter, and the module suffix is the cache's mangling of a path
    ## (`stagd0hts`, `sysvq0asl`) rather than a name anyone typed. The frame
    ## SEQUENCE is what disambiguates two procs that share a name, and it is
    ## right there in the trace.
    ##
    ## A leading backtick goes too: NIF quotes a symbol whose name would not
    ## tokenize as an identifier, and the compiler's own module initializer
    ## (`` `ini ``) shows up in every trace between the top-level code and
    ## `main`. The quote is syntax, not part of the name.
    ##
    ## Anything that does not parse is emitted verbatim — a stack trace must
    ## never be less informative than the raw symbol.
    var first = 0
    if sym.len > 0 and sym[0] == '`': first = 1
    var dot = -1                       # the '.' that starts `.<digits>`
    var i = sym.len - 2
    while i > first:
      if sym[i] == '.' and sym[i+1] >= '0' and sym[i+1] <= '9':
        dot = i
        break
      dec i
    if dot <= first:
      dest.add sym
    else:
      for k in first ..< dot: dest.add sym[k]

  proc getStackTrace*(skip = 0): string =
    ## The call stack of the caller, innermost frame first, one proc per line.
    ##
    ## `skip` drops that many innermost frames — a panic handler that wraps this
    ## passes 1 so its own frame does not head the trace. `getStackTrace` itself
    ## is never listed.
    result = ""
    let t = openTraceTable()
    if t.count == 0: return
    # `callerFrame()` is naked, so this is the address of the return-address slot
    # in OUR frame: the `pc` read from it lies in `getStackTrace` itself, and
    # each step then moves to the frame above.
    var slot = cast[uint](callerFrame())
    let origin = slot
    # Depth 0 is `getStackTrace`'s own frame, which is never reported; `skip`
    # drops that many more.
    let firstDepth = 1 + (if skip > 0: skip else: 0)
    var depth = 0
    while depth < maxStackTraceFrames:
      let pc = readWord(slot)
      let idx = findProc(t, pc)
      if idx < 0: break                # the return address left our code: done
      let step = entryCfaOff(t, idx)
      if step < 8: break               # a frame smaller than its own return address
      if depth >= firstDepth:
        result.addPrettyName entryName(t, idx)
        result.add '\n'
      slot = slot + step
      if slot - origin > MaxStackSpan: break
      inc depth
    if depth >= maxStackTraceFrames:
      # Truncated, not finished. Saying so is the difference between "the stack
      # is this deep" and "the walk gave up", which is exactly what a reader
      # needs to know when the cause is runaway recursion.
      result.add "...\n"

else:
  proc getStackTrace*(skip = 0): string =
    ## No stack trace on this target — see `stackTracesAvailable`.
    result = ""
