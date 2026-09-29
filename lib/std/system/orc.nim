# The `orc` memory management strategy: the reference counting of `arc` plus a
# cycle collector. Selected by `--mm:orc` via `include "$MM"` in `system.nim`.
#
# The collector is Nim's `orc.nim`: synchronous trial deletion after
# Bacon & Rajan, "Concurrent Cycle Collection in Reference Counted Systems"
# (https://www.cs.purdue.edu/homes/hosking/690M/Bacon01Concurrent.pdf, Fig. 2),
# with Lins' observation that only a decrement that does NOT free the cell can
# leave garbage behind.
#
# Like `arc`, the counter updates are not atomic: a `ref` must not be shared
# between threads, and every thread collects its own cycles.
#
# What the compiler does for a runtime whose `nimTraceRef` carries
# `.enableTrace` (`hexer/lifter.runtimeEnablesTrace`) -- this one, or a custom
# runtime that implements the same protocol:
#
# * A cell carries a second header word after `rc`, `rootIdx` (see `OrcHeader`).
# * `=destroy` of a `ref T` whose `T` can form a cycle calls `nimDecRefCyclic`
#   instead of `arcDec`, handing it the cell and `T`'s *cell operation*, a
#   compiler-generated `proc (cell, env: pointer)` that traces `T`'s payload
#   (`env != nil`) or destroys the payload and frees the cell (`env == nil`).
#   The cell operation IS the type descriptor: it is all the collector ever
#   needs to know about a type.
# * `=trace` of a `ref T` field calls `nimTraceRef` with the field's address
#   and `T`'s cell operation. `=trace` of every other type visits exactly the
#   `ref`s it owns (never `.cursor` fields, never raw pointers unless the type
#   has a hand-written `=trace`, like `seq`).
#
# `arcInc`/`arcDec`/`arcIsUnique` stay the three primitives every strategy
# supplies; refs to types that cannot form a cycle use them exactly like `arc`.

{.feature: "lenientnils".}

func arcInc*(memLoc: var int) {.inline.} =
  ## Increments the reference count.
  {.cast(noSideEffect).}:
    inc memLoc

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Decrements the reference count. Returns true when it reaches zero.
  ## A fresh object starts at 0, so "reached zero" is `< 0` (as in `arc`).
  {.cast(noSideEffect).}:
    dec memLoc
    result = memLoc < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Returns true if the reference count is 0 (no extra references).
  {.cast(noSideEffect).}:
    result = memLoc == 0

const
  colBlack = 0b00
  colGray = 0b01
  colWhite = 0b10
  colorMask = 0b11
  rootShift = 2

  defaultThreshold = 128

type
  OrcHeader = object
    ## Must match the cell layout `hexer/lengcgen.trRefBody` emits in `orc`
    ## mode: `(rc, rootIdx, payload)`.
    rc: int
      ## zero-based: 0 is one reference. The same counter `arc` keeps, so
      ## `arcInc`/`arcDec` and `assertRc` read it unchanged.
    rootIdx: int64
      ## `(index in roots + 1) shl rootShift`, or'ed with the cell's color.
      ## 64 bits on every target (the compiler's layout, which `yrc` needs).
      ## 0 = not a root and black: what a fresh cell starts with.
  OrcCell = ptr OrcHeader

  CellOp* = proc (cell: pointer; env: pointer) {.nimcall.}
    ## Traces the payload of `cell` into `env` (a `ptr GcEnv`), or, with
    ## `env == nil`, destroys the payload and frees the cell.

  CellEntry = object
    p: pointer   ## a cell (roots, toFree), or the address of a `ref` field (traceStack)
    op: CellOp

  CellSeq = object
    len, cap: int
    d: ptr UncheckedArray[CellEntry]

  GcEnv = object
    traceStack: CellSeq
    toFree: CellSeq
    freed, touched, edges, rcSum: int
    keepThreshold: bool

func color(c: OrcCell): int {.inline.} = int(c.rootIdx and colorMask)

func setColor(c: OrcCell; col: int) {.inline.} =
  c.rootIdx = (c.rootIdx and not int64(colorMask)) or int64(col)

func rootPos(c: OrcCell): int {.inline.} = int(c.rootIdx shr rootShift)

func setRootPos(c: OrcCell; pos: int) {.inline.} =
  c.rootIdx = (int64(pos) shl rootShift) or (c.rootIdx and colorMask)

proc orcOutOfMem() {.noinline.} =
  # The collector's own bookkeeping could not grow. A half-traced graph cannot
  # be collected safely, and there is no caller to report to: `=destroy`
  # hooks do not raise.
  cAbort()

proc init(s: var CellSeq; cap: int) =
  s.len = 0
  s.cap = cap
  s.d = cast[ptr UncheckedArray[CellEntry]](alloc(cap * sizeof(CellEntry)))
  if s.d == nil: orcOutOfMem()

proc deinit(s: var CellSeq) =
  if s.d != nil:
    dealloc(s.d)
    s.d = nil
  s.len = 0
  s.cap = 0

proc add(s: var CellSeq; p: pointer; op: CellOp) {.inline.} =
  if s.len >= s.cap:
    s.cap = s.cap div 2 + s.cap
    s.d = cast[ptr UncheckedArray[CellEntry]](realloc(s.d, s.cap * sizeof(CellEntry)))
    if s.d == nil: orcOutOfMem()
  s.d[s.len] = CellEntry(p: p, op: op)
  inc s.len

proc pop(s: var CellSeq): CellEntry {.inline.} =
  dec s.len
  result = s.d[s.len]

var
  roots {.threadvar.}: CellSeq
  rootsThreshold {.threadvar.}: int

proc trace(s: OrcCell; op: CellOp; j: var GcEnv) {.inline.} =
  op(s, addr j)

proc free(s: OrcCell; op: CellOp) {.inline.} =
  op(s, nil)

proc nimTraceRef*(q: pointer; op: CellOp; env: pointer) {.inline, enableTrace.} =
  ## Called by a compiler-generated `=trace`: `q` is the address of a `ref`
  ## field, `op` the cell operation of its target type.
  let p = cast[ptr pointer](q)
  if p[] != nil:
    let j = cast[ptr GcEnv](env)
    j.traceStack.add(p, op)

proc unregisterCycle(s: OrcCell) =
  # swap with the last element. O(1)
  let idx = s.rootPos - 1
  let last = roots.len - 1
  let moved = roots.d[last]
  roots.d[idx] = moved
  setRootPos(cast[OrcCell](moved.p), idx + 1)
  roots.len = last
  setRootPos(s, 0)

proc scanBlack(s: OrcCell; op: CellOp; j: var GcEnv) =
  #[
  proc scanBlack(s: Cell) =
    setColor(s, colBlack)
    for t in sons(s):
      t.rc = t.rc + 1
      if t.color != colBlack:
        scanBlack(t)
  ]#
  s.setColor colBlack
  let until = j.traceStack.len
  trace(s, op, j)
  while j.traceStack.len > until:
    let e = j.traceStack.pop()
    let t = cast[OrcCell](cast[ptr pointer](e.p)[])
    inc t.rc
    if t.color != colBlack:
      t.setColor colBlack
      trace(t, e.op, j)

proc markGray(s: OrcCell; op: CellOp; j: var GcEnv) =
  #[
  proc markGray(s: Cell) =
    if s.color != colGray:
      setColor(s, colGray)
      for t in sons(s):
        t.rc = t.rc - 1
        if t.color != colGray:
          markGray(t)
  ]#
  if s.color != colGray:
    s.setColor colGray
    inc j.touched
    # refcounts are zero based so add 1 here:
    j.rcSum = j.rcSum + s.rc + 1
    trace(s, op, j)
    while j.traceStack.len > 0:
      let e = j.traceStack.pop()
      let t = cast[OrcCell](cast[ptr pointer](e.p)[])
      dec t.rc
      inc j.edges
      if t.color != colGray:
        t.setColor colGray
        inc j.touched
        # we already decremented its refcount so account for that:
        j.rcSum = j.rcSum + t.rc + 2
        trace(t, e.op, j)

proc scan(s: OrcCell; op: CellOp; j: var GcEnv) =
  #[
  proc scan(s: Cell) =
    if s.color == colGray:
      if s.rc > 0:
        scanBlack(s)
      else:
        s.setColor(colWhite)
        for t in sons(s): scan(t)
  ]#
  if s.color == colGray:
    if s.rc >= 0:
      scanBlack(s, op, j)
    else:
      s.setColor(colWhite)
      trace(s, op, j)
      while j.traceStack.len > 0:
        let e = j.traceStack.pop()
        let t = cast[OrcCell](cast[ptr pointer](e.p)[])
        if t.color == colGray:
          if t.rc >= 0:
            scanBlack(t, e.op, j)
          else:
            t.setColor(colWhite)
            trace(t, e.op, j)

proc collectColor(s: OrcCell; op: CellOp; col: int; j: var GcEnv) =
  #[
    was: 'collectWhite'.

  proc collectWhite(s: Cell) =
    if s.color == colWhite and not buffered(s):
      s.setColor(colBlack)
      for t in sons(s):
        collectWhite(t)
      free(s) # watch out, a bug here!
  ]#
  if s.color == col and s.rootPos == 0:
    s.setColor(colBlack)
    j.toFree.add(s, op)
    trace(s, op, j)
    while j.traceStack.len > 0:
      let e = j.traceStack.pop()
      let field = cast[ptr pointer](e.p)
      let t = cast[OrcCell](field[])
      # the destructors below must not touch moribund objects:
      field[] = nil
      if t.color == col and t.rootPos == 0:
        j.toFree.add(t, e.op)
        t.setColor(colBlack)
        trace(t, e.op, j)

proc collectCyclesBacon(j: var GcEnv; lowMark: int) =
  # Fig. 2. Synchronous Cycle Collection:
  #[
    for s in roots:
      markGray(s)
    for s in roots:
      scan(s)
    for s in roots:
      remove s from roots
      s.buffered = false
      collectWhite(s)
  ]#
  let last = roots.len - 1
  var i = last
  while i >= lowMark:
    markGray(cast[OrcCell](roots.d[i].p), roots.d[i].op, j)
    dec i

  var colToCollect = colWhite
  if j.rcSum == j.edges:
    # short-cut: every reference into the subgraph is internal, it is all garbage:
    colToCollect = colGray
    j.keepThreshold = true
  else:
    i = last
    while i >= lowMark:
      scan(cast[OrcCell](roots.d[i].p), roots.d[i].op, j)
      dec i

  init j.toFree, 1024
  i = 0
  while i < roots.len:
    let s = cast[OrcCell](roots.d[i].p)
    setRootPos(s, 0)
    collectColor(s, roots.d[i].op, colToCollect, j)
    inc i

  # `free` runs destructors, which can append to `roots` (Nim bug #22927):
  # empty `roots` and raise the threshold so no collection starts while we
  # are inside this critical section.
  let oldThreshold = rootsThreshold
  rootsThreshold = high(int)
  roots.len = 0

  i = 0
  while i < j.toFree.len:
    free(cast[OrcCell](j.toFree.d[i].p), j.toFree.d[i].op)
    inc i

  rootsThreshold = oldThreshold
  j.freed = j.freed + j.toFree.len
  deinit j.toFree

proc collectCycles() =
  var j = GcEnv()
  init j.traceStack, 1024
  collectCyclesBacon(j, 0)
  deinit j.traceStack
  if roots.len == 0:
    deinit roots

  # Adapt the threshold to how effective the collector was: collecting at
  # least half of what we touched resets it, collecting less grows it.
  if j.keepThreshold:
    discard
  elif j.freed * 2 >= j.touched:
    rootsThreshold = max(rootsThreshold div 3 * 2, 16)
  elif rootsThreshold < high(int) div 4:
    if rootsThreshold <= 0: rootsThreshold = defaultThreshold
    rootsThreshold = rootsThreshold div 2 + rootsThreshold

proc registerCycle(s: OrcCell; op: CellOp) =
  if roots.d == nil: init(roots, 1024)
  roots.add(s, op)
  setRootPos(s, roots.len)
  if roots.len - defaultThreshold >= rootsThreshold:
    collectCycles()

proc rememberCycle(isDestroyAction: bool; s: OrcCell; op: CellOp) {.noinline.} =
  if isDestroyAction:
    if s.rootPos > 0:
      unregisterCycle(s)
  elif s.rootPos == 0 and op != nil:
    # not a root yet: remember it until an `incRef`-free collection decides.
    s.setColor colBlack
    registerCycle(s, op)

func nimDecRefCyclic*(p: pointer; op: CellOp): bool {.inline.} =
  ## `arcDec` for a cell whose type can form a cycle: a decrement that does not
  ## free the cell makes it a candidate root. Returns true when the caller must
  ## destroy and free the cell. `op` is the cell operation of the cell's static
  ## type, or nil for a type that cannot form a cycle but whose cells may still
  ## have been registered through another static type (a subclass).
  {.cast(noSideEffect).}:
    let cell = cast[OrcCell](p)
    if cell.rc == 0:
      result = true
    else:
      dec cell.rc
      result = false
    if result or op != nil:
      rememberCycle(result, cell, op)

proc GC_runOrc*() =
  ## Forces a cycle collection pass.
  collectCycles()

proc GC_fullCollect*() =
  ## Forces a full garbage collection pass. With `--mm:orc` triggers the cycle
  ## collector. This is an alias for `GC_runOrc`.
  collectCycles()

proc GC_enableOrc*() =
  ## Enables the cycle collector subsystem of `--mm:orc`. This is a `--mm:orc`
  ## specific API. Check with `when defined(gcOrc)` for its existence.
  rootsThreshold = 0

proc GC_disableOrc*() =
  ## Disables the cycle collector subsystem of `--mm:orc`. This is a `--mm:orc`
  ## specific API. Check with `when defined(gcOrc)` for its existence.
  rootsThreshold = high(int)

proc GC_prepareOrc*(): int {.inline.} = roots.len
