#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Alias-aware function summaries shared by late Hexer passes and the Leng
## optimizer (CSE). A summary is a Steensgaard-style partition of the
## parameters, the result and an implicit "outside" world, plus per-class
## may-read / may-write effects. It is *bounded in space by the arity* of the
## proc: independent of the body size.
##
## *Effects* are directed — `obj.field = x` writes `obj` and reads `x`, it does
## not write `x`. *Classes* are not. Any copy joins them, a load included:
## `var p = obj.field` copies a POINTER out of `obj`, so `p[] = 3` writes
## `obj`'s graph. This module has no type navigator and cannot tell that field
## from an `int` one, and under-merging is the unsound direction — gating the
## join on "the source is a bare symbol" once made a proc that stores through
## such a pointer report `isReadOnly`, and CSE then kept a stale load across
## the call. Over-merging only costs precision. (`aliasing.nim`, which does
## have the navigator, gates on `pointerBearing` instead — that is the sound
## way to buy the precision back, and the place to do it.)
##
## A `cast` to a pointer-bearing type from a value that does not carry pointer
## identity (`cast[ptr T](4000)`, `cast[ptr T](intParam)`) forges an untracked
## pointee: the proc is `writeGlobal`/`readGlobal`. Pointer-to-pointer casts,
## `cast[ptr T](addr x)`, and value puns (`cast[byte](v)`) are not forging.
##
## The wire format is NIF, for example:
## `(smry raises (param 0 0 writes slot) (param 1 0 reads) result 2)`
## means: may raise; params 0 and 1 are in the same partition class 0 (they may
## alias); param 0 is written and its slot reassigned; param 1 is read; the
## result is its own fresh class (`2 == params.len`). See `doc/tags.md`.

import std / [assertions, tables, sets]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lengc / [leng_model]

type
  ParamEffect* = object
    cls*: uint32        ## partition class id; params sharing a `cls` may alias.
                        ## Canonical = smallest interface index in the class.
    reads*: bool        ## call may read through this param's reachable graph
    writes*: bool       ## call may write through this param's reachable graph
    slotWritten*: bool  ## `var` param whose own binding is reassigned (not just
                        ## its pointee)
    escapes*: bool      ## param's graph is stored into a global or passed to a
                        ## callee with no summary

  FunctionSummary* = object
    writesGlobal*: bool
    readsGlobal*: bool
    callsUnknown*: bool
    raises*: bool
    resultCls*: uint32   ## class the result joins; `== params.len` ⇒ own fresh class
    resultEscapes*: bool
    params*: seq[ParamEffect]

  FunctionSummaryTable* = Table[SymId, FunctionSummary]

# ---- queries used by the optimizer ----------------------------------------

proc isReadOnly*(s: FunctionSummary): bool {.inline.} =
  if s.writesGlobal or s.callsUnknown: return false
  for p in s.params:
    if p.writes or p.slotWritten: return false
  result = true

proc paramMayWrite*(s: FunctionSummary; idx: int): bool {.inline.} =
  ## Conservative: an out-of-range index (e.g. a varargs actual) or an unknown
  ## callee is assumed to write.
  if s.callsUnknown: return true
  if idx < 0 or idx >= s.params.len: return true
  result = s.params[idx].writes or s.params[idx].slotWritten

proc paramDirectEscapes*(s: FunctionSummary; idx: int): bool {.inline.} =
  if s.callsUnknown: return true
  result = idx >= 0 and idx < s.params.len and s.params[idx].escapes

# ---------------------------------------------------------------------------
# Extraction: a lightweight, sound (over-approximating) Steensgaard partition.
# Interface elements are indexed 0..nParams: params are 0..nParams-1 and the
# result is `nParams`. `cls` values are the canonical (smallest) element index
# of a class; the result element being the largest index can never become a
# class root, so a param's `cls` is always a param index. Every copy merges
# classes — `p = q`, `return e`, `obj.field = e`, `var p = obj.field` — because
# the analysis is level-insensitive and cannot see which copies move a pointer.
# ---------------------------------------------------------------------------

const
  OutsideElem = -1                 ## untracked symbol: a global / unknown graph

type
  CallFact = object
    callee: SymId
    argRoots: seq[seq[int]]   ## per actual: the interface elements it may root at

  ProcAnalysis = object
    nParams: int
    uf: seq[int]                       ## union-find parent array, length nParams+1
    paramLookup: Table[SymId, int]
    localRoots: Table[SymId, seq[int]] ## local sym -> interface elements it aliases
    freshLocals: HashSet[SymId]        ## locals whose storage is provably this
                                       ## proc's own frame: declared here, and
                                       ## never assigned anything that could be
                                       ## a pointer we do not track. A store
                                       ## through one is not a global write.
    reads, writes, slotW, escapes: seq[bool]  ## per element, length nParams+1
    writesGlobal, readsGlobal, callsUnknown, raises: bool
    calls: seq[CallFact]

proc ufFind(u: var seq[int]; x: int): int =
  var r = x
  while u[r] != r: r = u[r]
  var i = x
  while u[i] != r:
    let nxt = u[i]
    u[i] = r
    i = nxt
  result = r

proc ufUnion(u: var seq[int]; a, b: int) =
  if a < 0 or b < 0 or a >= u.len or b >= u.len: return
  let ra = ufFind(u, a)
  let rb = ufFind(u, b)
  if ra != rb:
    if ra < rb: u[rb] = ra      # keep the smaller index as the canonical root
    else: u[ra] = rb

proc markReadElem(a: var ProcAnalysis; e: int) =
  if e < 0:
    a.readsGlobal = true        # reading a global / unknown graph
  elif e < a.nParams:
    a.reads[e] = true
  # e == nParams: reading this proc's result local, not a global

proc markWriteElem(a: var ProcAnalysis; e: int; slot = false) =
  if e < 0:
    a.writesGlobal = true       # write through a global / unknown graph
  elif e == a.nParams:
    discard                     # write through this proc's result — not a global
  else:
    a.writes[e] = true
    if slot: a.slotW[e] = true

proc markEscapeElem(a: var ProcAnalysis; e: int) =
  if e < 0: a.callsUnknown = true
  elif e <= a.nParams: a.escapes[e] = true

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc typeMayHoldPointer(typ: Cursor; depth = 0): bool =
  ## Structural stand-in for aliasing.pointerBearing without a type navigator.
  ## Named types (Symbol) are treated as pointer-bearing (conservative).
  if depth > 12: return true
  case typ.typeKind
  of IT, UT, FT, CT, BoolT, EnumT, VoidT:
    result = false
  of PtrT, AptrT, ProctypeT, FlexarrayT:
    result = true
  of ArrayT:
    result = typeMayHoldPointer(firstChild(typ), depth+1)
  of ObjectT, UnionT:
    result = false
    var body = typ
    body.into:
      if typ.typeKind == ObjectT and body.kind == Symbol:
        if typeMayHoldPointer(body, depth+1): return true
        inc body
      while body.hasMore:
        if body.substructureKind == FldU:
          let fld = takeFieldDecl(body)
          if typeMayHoldPointer(fld.typ, depth+1): return true
        else:
          skip body
  else:
    result = true                  # Symbol / unknown

proc sourceMayBePointer(n: Cursor): bool =
  ## True unless `n` is provably a pointer-free value. Integer casts and
  ## pointer arithmetic still carry the inner pointer's identity.
  var n = n
  while n.kind == TagLit and n.exprKind == ParC:
    n = n.childCursor
  case n.kind
  of Symbol, SymbolDef:
    result = true                  # no types: a symbol might be a pointer
  of IntLit, UIntLit, FloatLit, CharLit, StrLit:
    result = false
  of TagLit:
    case n.exprKind
    of AddrC, HaddrC, DerefC, PatC, NilC, CallC:
      result = true
    of TrueC, FalseC, InfC, NeginfC, NanC, SizeofC, AlignofC, OffsetofC:
      result = false
    of ConvC, CastC:
      var inner = n
      inc inner
      skip inner
      result = sourceMayBePointer(inner)
    of DotC, AtC:
      result = sourceMayBePointer(n.childCursor)
    of AddC, SubC, BitandC, BitorC, BitxorC:
      result = false
      var r = n
      var idx = 0
      r.into:
        while r.hasMore:
          if idx > 0 and sourceMayBePointer(r): return true
          inc idx
          skip r
    else:
      result = true
  else:
    result = false

proc isForgingCast(n: Cursor): bool =
  if n.kind != TagLit or n.exprKind != CastC: return false
  var t = firstChild(n)
  var inner = t
  skip inner
  result = typeMayHoldPointer(t) and not sourceMayBePointer(inner)

proc unwrapConv(n: Cursor): Cursor =
  ## Strip `(conv)`/`(baseobj)`/`(par)` wrappers, and non-forging `(cast)`,
  ## so the head is the underlying value. A forging cast is not identity.
  result = n
  while result.kind == TagLit:
    case result.exprKind
    of ConvC:
      inc result
      skip result                # type
    of CastC:
      if isForgingCast(result): break
      inc result
      skip result
    of BaseobjC:
      inc result
      skip result                # type
      skip result                # inheritance-depth
    of ParC:
      result = result.childCursor
    else:
      break

proc isFreshValue(a: ProcAnalysis; roots: seq[int]; e: Cursor): bool {.inline.} =
  ## `e` yields storage this proc owns: it names no tracked graph AND cannot be
  ## a pointer at all. A call, a deref or a bare symbol fails the second test —
  ## `f()` may hand back a pointer into a global whose roots are empty, so
  ## "no roots" alone does NOT mean "fresh".
  roots.len == 0 and not sourceMayBePointer(e)

proc noteFresh(a: var ProcAnalysis; sym: SymId; roots: seq[int]; e: Cursor) =
  if sym == SymId(0): return
  if isFreshValue(a, roots, e): a.freshLocals.incl sym
  else: a.freshLocals.excl sym

proc baseSymOf(n: Cursor): SymId =
  ## Innermost symbol of an lvalue path, or `SymId(0)` if the path is computed.
  var n = unwrapConv(n)
  while n.kind == TagLit:
    case n.exprKind
    of DotC, DerefC, PatC, AtC, AddrC, HaddrC, ParC:
      n = unwrapConv(n.childCursor)
    else:
      return SymId(0)
  if n.kind == Symbol: result = n.symId
  else: result = SymId(0)

proc collectParamSyms(params: Cursor): seq[SymId] =
  result = @[]
  if not params.isTagLit: return
  var p = params
  p.into:
    while p.hasMore:
      if p.substructureKind == ParamU:
        var q = p
        inc q
        if q.kind == SymbolDef:
          result.add q.symId
      skip p

proc exprRoots(a: var ProcAnalysis; n: Cursor): seq[int] =
  ## The interface elements the value of `n` may be rooted at. Constructors,
  ## literals and unrecognised forms yield the empty set (a "fresh" value).
  result = @[]
  var n = n
  case n.kind
  of Symbol:
    if a.paramLookup.hasKey(n.symId):
      result.add a.paramLookup.getOrQuit(n.symId)
    elif a.localRoots.hasKey(n.symId):
      result = a.localRoots.getOrQuit(n.symId)
    else:
      result.add OutsideElem     # global / unknown symbol
  of TagLit:
    case n.exprKind
    of DotC, DerefC, PatC, AtC, AddrC, HaddrC:
      result = exprRoots(a, n.childCursor)
    of ConvC:
      var r = n
      inc r
      skip r                 # skip the type operand
      result = exprRoots(a, r)
    of CastC:
      if isForgingCast(n):
        result = @[]         # untracked pointee, not the inner integer
      else:
        var r = n
        inc r
        skip r
        result = exprRoots(a, r)
    of BaseobjC:
      var r = n
      inc r
      skip r                 # type
      skip r                 # inheritance-depth intlit
      result = exprRoots(a, r)
    of CallC:
      # Conservative "select": without the callee's summary yet, a pointer
      # result may be one of the arguments. Value results (bool, int) still
      # get these roots, but `handleRet` / slot rebind refuse to *join classes*
      # on a non-identity expression, so `return f(a, b)` does not make the
      # result alias `a`.
      var r = n
      r = sub(r)  # throwaway copy; no leaveScope needed
      skip r                 # callee
      while r.hasMore:
        for e in exprRoots(a, r): result.add e
        skip r
    else:
      discard
  else:
    discard

proc walkStmt(a: var ProcAnalysis; n: var Cursor)

proc lastChildStart(n: Cursor): Cursor =
  ## Returns a cursor to the last child of the `(tag ...)` node `n`.
  result = n
  var c = n
  c = sub(c)  # throwaway copy; no leaveScope needed
  while c.hasMore:
    result = c
    skip c

proc handleLocalDecl(a: var ProcAnalysis; n: var Cursor) =
  var c = n
  inc c
  let sym = if c.kind == SymbolDef: c.symId else: SymId(0)
  let initStart = lastChildStart(n)
  let roots = exprRoots(a, initStart)
  if sym != SymId(0):
    # `x` names every graph the initializer can reach — NOT just an identity
    # `var x = p`. A load is level-insensitive here: `var p = obj.fld` copies a
    # POINTER out of obj, so `p[] = 3` writes obj. Without types we cannot tell
    # that field from an `int` one, and under-merging is the unsound direction
    # (it made a proc that stores through such a pointer report `isReadOnly`).
    a.localRoots[sym] = roots
    noteFresh(a, sym, roots, initStart)
  for e in roots: markReadElem(a, e)
  var ic = initStart
  walkStmt(a, ic)            # capture nested calls / addr in the initializer
  skip n

proc handleAssign(a: var ProcAnalysis; n: var Cursor; reversed: bool) =
  var c = n
  inc c
  let firstStart = c
  skip c
  let secondStart = c
  let destStart = if reversed: secondStart else: firstStart
  let valStart = if reversed: firstStart else: secondStart

  let valRoots = exprRoots(a, valStart)
  let destBareSym = destStart.kind == Symbol

  if destBareSym and a.paramLookup.hasKey(destStart.symId):
    # Slot rebind of a param: `p = …`. Writes p's binding, not p's old pointee
    # graph — but p now names whatever the value reaches, so the classes join.
    let e = a.paramLookup.getOrQuit(destStart.symId)
    markWriteElem(a, e, slot = true)
    for r in valRoots: ufUnion(a.uf, e, r)
  elif destBareSym and a.localRoots.hasKey(destStart.symId):
    a.localRoots[destStart.symId] = valRoots   # rebind local to value's graph
    noteFresh(a, destStart.symId, valRoots, valStart)
  elif destBareSym:
    # A bare symbol that is neither a param nor a local declared here: a
    # global. The write is a global write, and the value's graph escapes into
    # it (this is what `escapes` means — see the module doc).
    a.writesGlobal = true
    for r in valRoots: markEscapeElem(a, r)
  else:
    # Store through dest: `obj.field = x`, `p[] = x`, `a[i] = x`. Writes dest's
    # object and reads x; the cell now names x's graph, so the classes join
    # (`x.obj = y` then `x.obj.a = 3` mutates y).
    let destRoots = exprRoots(a, destStart)
    if destRoots.len == 0:
      # No tracked root. A store into a local whose storage is provably this
      # proc's own frame (`var result; result.fld = x`) is invisible to the
      # caller; anything else is a write through memory we cannot name.
      if baseSymOf(destStart) notin a.freshLocals:
        a.writesGlobal = true
    else:
      for e in destRoots:
        markWriteElem(a, e)
        for r in valRoots: ufUnion(a.uf, e, r)
    var dc = destStart
    walkStmt(a, dc)                            # index exprs / nested calls in dest

  for r in valRoots: markReadElem(a, r)
  var vc = valStart
  walkStmt(a, vc)                              # nested calls in the value
  skip n

proc handleRet(a: var ProcAnalysis; n: var Cursor) =
  var c = n
  c = sub(c)  # throwaway copy; no leaveScope needed
  if c.hasMore and c.kind != DotToken:
    let roots = exprRoots(a, c)
    for r in roots:
      ufUnion(a.uf, a.nParams, r)              # result aliases the returned graph
      markReadElem(a, r)
    var rc = c
    walkStmt(a, rc)
  skip n

proc handleCall(a: var ProcAnalysis; n: var Cursor) =
  var c = n
  c = sub(c)  # throwaway copy; no leaveScope needed
  let calleeStart = c
  var fact = CallFact(callee: SymId(0), argRoots: @[])
  if calleeStart.kind == Symbol:
    fact.callee = calleeStart.symId
  skip c                                       # past callee
  while c.hasMore:
    let roots = exprRoots(a, c)
    fact.argRoots.add roots
    for r in roots: markReadElem(a, r)
    if fact.callee == SymId(0):
      for r in roots:
        markWriteElem(a, r)                    # unknown callee may write & leak
        markEscapeElem(a, r)
    var ac = c
    walkStmt(a, ac)
    skip c
  if fact.callee != SymId(0):
    a.calls.add ensureMove fact
  else:
    a.callsUnknown = true
  skip n

proc walkStmt(a: var ProcAnalysis; n: var Cursor) =
  if n.kind == Symbol:
    if a.paramLookup.hasKey(n.symId):
      markReadElem(a, a.paramLookup.getOrQuit(n.symId))
    inc n
    return
  if not n.isTagLit:
    inc n
    return
  case n.stmtKind
  of VarS:
    handleLocalDecl(a, n)
  of AsgnS:
    handleAssign(a, n, reversed = false)
  of StoreS:
    handleAssign(a, n, reversed = true)        # `(store value dest)`
  of RetS:
    handleRet(a, n)
  of CallS:
    handleCall(a, n)
  of RaiseS:
    a.raises = true
    n.loopInto:
      walkStmt(a, n)
  else:
    if n.exprKind in AddrKinds:
      let escRoots = exprRoots(a, n.childCursor)
      for r in escRoots: markEscapeElem(a, r)
    elif n.exprKind == CastC and isForgingCast(n):
      a.writesGlobal = true
      a.readsGlobal = true
    n.loopInto:
      walkStmt(a, n)

proc markAllUnknown(a: var ProcAnalysis) =
  a.callsUnknown = true
  a.writesGlobal = true
  a.readsGlobal = true
  for i in 0 ..< a.nParams:
    a.reads[i] = true
    a.writes[i] = true
    a.slotW[i] = true
    a.escapes[i] = true

proc computeProcAnalysis(procDecl: Cursor): ProcAnalysis =
  var p = procDecl
  let d = takeProcDecl(p)
  let paramSyms = collectParamSyms(d.params)
  result = ProcAnalysis(nParams: paramSyms.len)
  result.uf = newSeq[int](paramSyms.len + 1)
  for i in 0 .. paramSyms.len: result.uf[i] = i
  result.reads = newSeq[bool](paramSyms.len + 1)
  result.writes = newSeq[bool](paramSyms.len + 1)
  result.slotW = newSeq[bool](paramSyms.len + 1)
  result.escapes = newSeq[bool](paramSyms.len + 1)
  for i, s in paramSyms: result.paramLookup[s] = i
  if d.body.isTagLit:
    var body = d.body
    walkStmt(result, body)
  else:
    markAllUnknown result

proc finalizeSummary(a: var ProcAnalysis): FunctionSummary =
  result = FunctionSummary(
    writesGlobal: a.writesGlobal, readsGlobal: a.readsGlobal,
    callsUnknown: a.callsUnknown, raises: a.raises)
  result.params = newSeq[ParamEffect](a.nParams)
  for i in 0 ..< a.nParams:
    result.params[i] = ParamEffect(
      cls: uint32(ufFind(a.uf, i)),
      reads: a.reads[i], writes: a.writes[i],
      slotWritten: a.slotW[i], escapes: a.escapes[i])
  result.resultCls = uint32(ufFind(a.uf, a.nParams))
  result.resultEscapes = a.escapes[a.nParams]

proc computeFunctionSummary*(procDecl: Cursor): FunctionSummary =
  ## Intra-procedural summary only (no callee summaries available).
  var a = computeProcAnalysis(procDecl)
  result = finalizeSummary(a)

proc applyCallee(a: var ProcAnalysis; call: CallFact; callee: FunctionSummary) =
  a.raises = a.raises or callee.raises
  a.writesGlobal = a.writesGlobal or callee.writesGlobal
  a.readsGlobal = a.readsGlobal or callee.readsGlobal
  a.callsUnknown = a.callsUnknown or callee.callsUnknown
  var byCls = initTable[uint32, int]()        # callee class -> a representative caller element
  for k in 0 ..< callee.params.len:
    let pe = callee.params[k]
    let roots = if k < call.argRoots.len: call.argRoots[k] else: newSeq[int]()
    if pe.writes or pe.slotWritten:
      # No root: the actual is a computed address we cannot name, so the write
      # lands somewhere unknown. Dropping it silently (the effect has nowhere
      # to go) would under-report — same shape as `escapes` just below.
      if roots.len == 0: a.writesGlobal = true
      else:
        for r in roots: markWriteElem(a, r, pe.slotWritten)
    if pe.reads:
      if roots.len == 0: a.readsGlobal = true
      else:
        for r in roots: markReadElem(a, r)
    if pe.escapes:
      if roots.len == 0: a.callsUnknown = true
      else:
        for r in roots: markEscapeElem(a, r)
    # Union caller actuals the callee placed in one class (identity aliases
    # only — the callee no longer merges classes on field store/load).
    for r in roots:
      if r < 0: continue
      if byCls.hasKey(pe.cls): ufUnion(a.uf, byCls.getOrQuit(pe.cls), r)
      else: byCls[pe.cls] = r

proc applyExternalCall(a: var ProcAnalysis; call: CallFact) =
  a.callsUnknown = true
  for roots in call.argRoots:
    for r in roots:
      markWriteElem(a, r)
      markEscapeElem(a, r)

proc resolveSummaries(analyses: var Table[SymId, ProcAnalysis]): FunctionSummaryTable =
  ## Least fixpoint over the in-module call graph. Effects are monotone (bits
  ## only get set, classes only merge), so iteration converges; the cap is a
  ## safety net.
  const MaxIters = 10
  result = initTable[SymId, FunctionSummary]()
  for sym, a in analyses.mpairs:
    result[sym] = finalizeSummary(a)
  for _ in 0 ..< MaxIters:
    var changed = false
    for sym, a in analyses.mpairs:
      for call in a.calls:
        if analyses.hasKey(call.callee):
          applyCallee(a, call, result.getOrDefault(call.callee))
        else:
          applyExternalCall(a, call)
      let s = finalizeSummary(a)
      if s != result.getOrDefault(sym):
        result[sym] = s
        changed = true
    if not changed: break

proc collectProcAnalyses(buf: var TokenBuf): Table[SymId, ProcAnalysis] =
  result = initTable[SymId, ProcAnalysis]()
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.isTagLit and n.stmtKind == ProcS:
          let p = n
          var d = n
          inc d
          if d.isSymbolDef:
            result[d.symId] = computeProcAnalysis(p)
        skip n

# ---- serialization --------------------------------------------------------

proc readParamSummary(n: var Cursor; outSummary: var FunctionSummary) =
  # no early `return` here: it would skip `into`'s epilogue and leave the
  # caller's cursor mid-scope
  n.into:
    var idx = -1
    if n.kind == IntLit:
      idx = int(n.intVal)
      inc n
    if idx < 0:
      while n.hasMore: skip n
    else:
      while outSummary.params.len <= idx:
        outSummary.params.add ParamEffect()
      var cls = uint32(idx)
      if n.kind == IntLit:
        cls = uint32(n.intVal)
        inc n
      outSummary.params[idx].cls = cls
      while n.hasMore:
        if n.kind == Ident:
          case pool.strings[n.strId]
          of "reads": outSummary.params[idx].reads = true
          of "writes": outSummary.params[idx].writes = true
          of "slot": outSummary.params[idx].slotWritten = true
          of "escapes": outSummary.params[idx].escapes = true
          else: discard
          inc n
        else:
          skip n

proc readSummary(n: var Cursor; outSummary: var FunctionSummary): bool =
  result = true
  outSummary = FunctionSummary()
  var sawResult = false
  n.into:
    while n.hasMore:
      if n.kind == Ident:
        case pool.strings[n.strId]
        of "writeGlobal": outSummary.writesGlobal = true; inc n
        of "readGlobal": outSummary.readsGlobal = true; inc n
        of "callsUnknown": outSummary.callsUnknown = true; inc n
        of "raises": outSummary.raises = true; inc n
        of "resultEscapes": outSummary.resultEscapes = true; inc n
        of "result":
          inc n
          if n.kind == IntLit:
            outSummary.resultCls = uint32(n.intVal)
            sawResult = true
            inc n
        else: inc n
      elif n.isTagLit and n.substructureKind == ParamU:
        readParamSummary(n, outSummary)
      else:
        skip n
  if not sawResult:
    outSummary.resultCls = uint32(outSummary.params.len)

proc readSummaryPragma*(pragmas: Cursor; outSummary: var FunctionSummary): bool =
  if not pragmas.isTagLit: return false
  var p = pragmas
  p.into:
    while p.hasMore:
      if p.isTagLit and p.pragmaKind == SmryP:
        return readSummary(p, outSummary)
      skip p
  result = false

proc collectFunctionSummaries*(buf: var TokenBuf): FunctionSummaryTable =
  result = initTable[SymId, FunctionSummary]()
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.isTagLit and n.stmtKind == ProcS:
          let d = takeProcDecl(n)
          var summary = FunctionSummary()
          if d.name.isSymbolDef and readSummaryPragma(d.pragmas, summary):
            result[d.name.symId] = summary
        else:
          skip n

proc addSummaryPragma(dest: var TokenBuf; summary: FunctionSummary; info: NifLineInfo) =
  dest.addParLe(TagId(SmryP), info)
  if summary.writesGlobal: dest.addIdent "writeGlobal", info
  if summary.readsGlobal: dest.addIdent "readGlobal", info
  if summary.callsUnknown: dest.addIdent "callsUnknown", info
  if summary.raises: dest.addIdent "raises", info
  for i, p in summary.params:
    dest.addParLe(TagId(ParamU), info)
    dest.addIntLit i, info
    dest.addIntLit int(p.cls), info
    if p.reads: dest.addIdent "reads", info
    if p.writes: dest.addIdent "writes", info
    if p.slotWritten: dest.addIdent "slot", info
    if p.escapes: dest.addIdent "escapes", info
    dest.addParRi()
  if int(summary.resultCls) != summary.params.len or summary.resultEscapes:
    dest.addIdent "result", info
    dest.addIntLit int(summary.resultCls), info
    if summary.resultEscapes: dest.addIdent "resultEscapes", info
  dest.addParRi()

proc writePragmasWithSummary(dest: var TokenBuf; pragmas: Cursor;
                             summary: FunctionSummary; info: NifLineInfo) =
  var p = pragmas
  if p.isDotToken:
    dest.addParLe(TagId(PragmasU), info)
    addSummaryPragma(dest, summary, info)
    dest.addParRi()
  elif p.isTagLit:
    dest.addParLe(p.cursorTagId, p.info)
    var hadSummary = false
    p.into:
      while p.hasMore:
        if p.isTagLit and p.pragmaKind == SmryP:
          addSummaryPragma(dest, summary, p.info)
          skip p
          hadSummary = true
        else:
          dest.takeTree p
    if not hadSummary:
      addSummaryPragma(dest, summary, info)
    dest.addParRi()
  else:
    dest.addSubtree p

proc annotateSummaries(dest: var TokenBuf; n: var Cursor;
                       summaries: FunctionSummaryTable) =
  case n.kind
  of TagLit:
    if n.stmtKind == ProcS:
      let tag = n.cursorTagId
      let info = n.info
      let d = takeProcDecl(n)
      dest.addParLe(tag, info)
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      if d.name.isSymbolDef and summaries.hasKey(d.name.symId):
        writePragmasWithSummary(dest, d.pragmas,
                                summaries.getOrQuit(d.name.symId), info)
      else:
        dest.addSubtree d.pragmas
      dest.addSubtree d.body
      dest.addParRi()
    else:
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          annotateSummaries(dest, n, summaries)
      dest.addParRi()
  else:
    dest.takeTree n

proc annotateFunctionSummaries*(buf: var TokenBuf) =
  var analyses = collectProcAnalyses(buf)
  let summaries = resolveSummaries(analyses)

  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len + buf.len div 16)
  annotateSummaries(dest, n, summaries)
  buf = ensureMove(dest)

when isMainModule:
  proc summaryOf(src: string): FunctionSummary =
    var buf = parseFromBuffer("(stmts " & src & ")", "M")
    var n = beginRead(buf)
    n.into:
      doAssert n.stmtKind == ProcS
      result = computeFunctionSummary(n)
      skip n

  proc twoParams(body: string): FunctionSummary =
    summaryOf("""
      (proc :f.0
        (params
          (param :obj.0 . . . .)
          (param :x.0 . . . .))
        . . (stmts """ & body & "))")

  block field_store_writes_obj_not_x:
    # `obj.field = x` writes obj, reads x. x is not itself written, but the
    # field cell may now name x's graph (`x.obj = y` then `x.obj.a` mutates y),
    # so the classes join.
    let s = twoParams("(asgn (dot obj.0 fld.0) x.0)")
    doAssert not s.writesGlobal
    doAssert not s.readsGlobal
    doAssert s.params.len == 2
    doAssert s.params[0].writes
    doAssert s.params[0].reads
    doAssert not s.params[0].slotWritten
    doAssert s.params[1].reads
    doAssert not s.params[1].writes
    doAssert s.params[0].cls == s.params[1].cls
    doAssert int(s.resultCls) == 2

  block pointer_field_store_then_write_through:
    # `x.obj = y; x.obj.a = 3` writes y's graph (same class as x).
    let s = twoParams("""
      (asgn (dot obj.0 fld.0) x.0)
      (asgn (dot (deref (dot obj.0 fld.0)) a.0) 3)""")
    doAssert s.params[0].writes
    doAssert s.params[0].cls == s.params[1].cls

  block field_load_copies_into_slot:
    # `x = obj.field` reads obj and rebinds x's slot. It does not WRITE obj —
    # effects are directed — but x may now point into obj's graph, so the two
    # land in one class.
    let s = twoParams("(asgn x.0 (dot obj.0 fld.0))")
    doAssert not s.writesGlobal
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert s.params[1].slotWritten
    doAssert s.params[0].cls == s.params[1].cls

  block field_load_into_local_is_readonly:
    let s = twoParams("""
      (var :tmp.0 . . . .)
      (asgn tmp.0 (dot obj.0 fld.0))""")
    doAssert not s.writesGlobal
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert not s.params[1].writes
    doAssert not s.params[1].slotWritten
    doAssert isReadOnly(s)

  block load_then_store_through_writes_the_source:
    # THE regression. `var p = obj.fld; p[] = 3` stores through a pointer read
    # out of obj, so obj's graph is written. Gating the class join on "the
    # initializer is a bare symbol" reported this proc as `isReadOnly`.
    let s = twoParams("""
      (var :p.0 . . (dot obj.0 fld.0))
      (asgn (deref p.0) 3)""")
    doAssert s.params[0].writes
    doAssert not isReadOnly(s)

  block load_into_param_then_store_through_writes_the_source:
    # Same hole one level up: `x = obj.fld; x[] = 3`. Here the write lands on
    # x's OWN element, and obj is covered by the class relation instead —
    # `invalidateForCall` clobbers every actual whose class is written, so
    # joining obj and x is what makes the caller drop obj's cached loads.
    # Gating the join on identity broke exactly that link.
    let s = twoParams("""
      (asgn x.0 (dot obj.0 fld.0))
      (asgn (deref x.0) 3)""")
    doAssert s.params[1].writes
    doAssert s.params[0].cls == s.params[1].cls

  block call_result_into_local_then_store_through:
    # A call result carries the conservative "may be any pointer argument"
    # roots; storing through it writes them.
    let s = twoParams("""
      (var :p.0 . . (call g.0 obj.0))
      (asgn (deref p.0) 3)""")
    doAssert s.params[0].writes

  block identity_copy_joins_classes:
    let s = twoParams("(asgn obj.0 x.0)")
    doAssert s.params[0].slotWritten
    doAssert s.params[1].reads
    doAssert s.params[0].cls == s.params[1].cls

  block result_field_store_is_not_global:
    let s = summaryOf("""
      (proc :f.0
        (params (param :x.0 . . . .))
        . . (stmts
          (var :result.0 . . . .)
          (asgn (dot result.0 fld.0) x.0)
          (ret result.0)))""")
    doAssert not s.writesGlobal
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert int(s.resultCls) == 1

  block return_field_joins_result:
    # `return obj.field` may hand the caller a pointer into obj's graph, so the
    # result joins obj's class. (Imprecise for a value field; unsound the other
    # way round, and this module has no types to tell them apart.)
    let s = twoParams("(ret (dot obj.0 fld.0))")
    doAssert s.params[0].reads
    doAssert not s.params[0].writes
    doAssert int(s.resultCls) == 0

  block return_identity_joins_result:
    let s = twoParams("(ret obj.0)")
    doAssert s.params[0].reads
    doAssert int(s.resultCls) == 0

  block return_call_joins_args:
    # `return f(obj, x)` may return either argument, so both join the result
    # class — and thereby each other. Over-merging: costs CSE, never correctness.
    let s = twoParams("(ret (call g.0 obj.0 x.0))")
    doAssert s.params[0].cls == s.params[1].cls
    doAssert int(s.resultCls) == 0

  block forging_int_to_ptr_is_alias_unsafe:
    # `cast[ptr T](4000)` forges a pointer to untracked memory.
    let s = twoParams("(asgn obj.0 (cast (ptr (i +32)) 4000))")
    doAssert s.writesGlobal
    doAssert s.readsGlobal

  block cast_byte_is_not_unsafe:
    let s = twoParams("(asgn x.0 (cast (u +8) obj.0))")
    doAssert not s.writesGlobal
    doAssert not s.readsGlobal

  block cast_ptr_from_addr_is_not_unsafe:
    let s = twoParams("(asgn obj.0 (cast (ptr (i +32)) (addr x.0)))")
    doAssert not s.writesGlobal
    doAssert s.params[0].cls == s.params[1].cls

  block cast_ptr_pun_joins:
    let s = twoParams("(asgn obj.0 (cast (ptr (i +32)) x.0))")
    doAssert not s.writesGlobal
    doAssert s.params[0].cls == s.params[1].cls

  block cast_ptr_arith_is_not_unsafe:
    # `cast[ptr T](cast[uint](p) + n)` is pointer arithmetic, not a forge.
    let s = twoParams(
      "(asgn obj.0 (cast (ptr (i +32)) (add (u +64) (cast (u +64) x.0) 8)))")
    doAssert not s.writesGlobal

  echo "ok"
