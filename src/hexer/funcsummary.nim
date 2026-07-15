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
## The wire format is NIF, for example:
## `(smry raises (param 0 0 writes slot) (param 1 0 reads) result 2)`
## means: may raise; params 0 and 1 are in the same partition class 0 (they may
## alias); param 0 is written and its slot reassigned; param 1 is read; the
## result is its own fresh class (`2 == params.len`). See `doc/tags.md`.

import std / [assertions, tables]
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
# class root, so a param's `cls` is always a param index.
# ---------------------------------------------------------------------------

type
  CallFact = object
    callee: SymId
    argRoots: seq[seq[int]]   ## per actual: the interface elements it may root at

  ProcAnalysis = object
    nParams: int
    uf: seq[int]                       ## union-find parent array, length nParams+1
    paramLookup: Table[SymId, int]
    localRoots: Table[SymId, seq[int]] ## local sym -> interface elements it aliases
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
  let ra = ufFind(u, a)
  let rb = ufFind(u, b)
  if ra != rb:
    if ra < rb: u[rb] = ra      # keep the smaller index as the canonical root
    else: u[ra] = rb

proc markReadElem(a: var ProcAnalysis; e: int) =
  if e >= 0 and e < a.nParams: a.reads[e] = true
  else: a.readsGlobal = true        # reading the result graph or unknown memory

proc markWriteElem(a: var ProcAnalysis; e: int; slot = false) =
  if e < 0 or e == a.nParams:
    a.writesGlobal = true          # write through unknown memory or the result graph
  else:
    a.writes[e] = true
    if slot: a.slotW[e] = true

proc markEscapeElem(a: var ProcAnalysis; e: int) =
  if e < 0: a.callsUnknown = true
  elif e <= a.nParams: a.escapes[e] = true

proc collectParamSyms(params: Cursor): seq[SymId] =
  result = @[]
  if params.kind != ParLe: return
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
  of ParLe:
    case n.exprKind
    of DotC, DerefC, PatC, AtC, AddrC:
      result = exprRoots(a, n.firstSon)
    of ConvC, CastC:
      var r = n
      inc r
      skip r                 # skip the type operand
      result = exprRoots(a, r)
    of BaseobjC:
      var r = n
      inc r
      skip r                 # type
      skip r                 # inheritance-depth intlit
      result = exprRoots(a, r)
    of CallC:
      # Steensgaard "select": the result may alias any pointer argument.
      var r = n
      discard enterScope(r)  # throwaway copy; no leaveScope needed
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
  discard enterScope(c)  # throwaway copy; no leaveScope needed
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
    a.localRoots[sym] = roots
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
    let e = a.paramLookup.getOrQuit(destStart.symId)
    markWriteElem(a, e, slot = true)
    for r in valRoots: ufUnion(a.uf, e, r)
  elif destBareSym:
    a.localRoots[destStart.symId] = valRoots   # rebind local to value's graph
  else:
    let destRoots = exprRoots(a, destStart)
    if destRoots.len == 0:
      a.writesGlobal = true                    # write through unknown memory
    else:
      for e in destRoots:
        markWriteElem(a, e)
        for r in valRoots: ufUnion(a.uf, e, r) # new heap edge dest -> val
    var dc = destStart
    walkStmt(a, dc)                            # index exprs / nested calls in dest

  for r in valRoots: markReadElem(a, r)
  var vc = valStart
  walkStmt(a, vc)                              # nested calls in the value
  skip n

proc handleRet(a: var ProcAnalysis; n: var Cursor) =
  var c = n
  discard enterScope(c)  # throwaway copy; no leaveScope needed
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
  discard enterScope(c)  # throwaway copy; no leaveScope needed
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
  if n.kind != ParLe:
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
    if n.exprKind == AddrC:
      let escRoots = exprRoots(a, n.firstSon)
      for r in escRoots: markEscapeElem(a, r)
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
  if d.body.kind == ParLe:
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
    # union caller actuals the callee placed in one class
    for r in roots:
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
        if n.kind == ParLe and n.stmtKind == ProcS:
          let p = n
          var d = n
          inc d
          if d.kind == SymbolDef:
            result[d.symId] = computeProcAnalysis(p)
        skip n
  endRead(buf)

# ---- serialization --------------------------------------------------------

proc readParamSummary(n: var Cursor; outSummary: var FunctionSummary) =
  # no early `return` here: it would skip `into`'s epilogue and leave the
  # caller's cursor mid-scope
  n.into:
    var idx = -1
    if n.kind == IntLit:
      idx = int(pool.integers[n.intId])
      inc n
    if idx < 0:
      while n.hasMore: skip n
    else:
      while outSummary.params.len <= idx:
        outSummary.params.add ParamEffect()
      var cls = uint32(idx)
      if n.kind == IntLit:
        cls = uint32(pool.integers[n.intId])
        inc n
      outSummary.params[idx].cls = cls
      while n.hasMore:
        if n.kind == Ident:
          case pool.strings[n.litId]
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
        case pool.strings[n.litId]
        of "writeGlobal": outSummary.writesGlobal = true; inc n
        of "readGlobal": outSummary.readsGlobal = true; inc n
        of "callsUnknown": outSummary.callsUnknown = true; inc n
        of "raises": outSummary.raises = true; inc n
        of "resultEscapes": outSummary.resultEscapes = true; inc n
        of "result":
          inc n
          if n.kind == IntLit:
            outSummary.resultCls = uint32(pool.integers[n.intId])
            sawResult = true
            inc n
        else: inc n
      elif n.kind == ParLe and n.substructureKind == ParamU:
        readParamSummary(n, outSummary)
      else:
        skip n
  if not sawResult:
    outSummary.resultCls = uint32(outSummary.params.len)

proc readSummaryPragma*(pragmas: Cursor; outSummary: var FunctionSummary): bool =
  if pragmas.kind != ParLe: return false
  var p = pragmas
  p.into:
    while p.hasMore:
      if p.kind == ParLe and p.pragmaKind == SmryP:
        return readSummary(p, outSummary)
      skip p
  result = false

proc collectFunctionSummaries*(buf: var TokenBuf): FunctionSummaryTable =
  result = initTable[SymId, FunctionSummary]()
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.kind == ParLe and n.stmtKind == ProcS:
          let d = takeProcDecl(n)
          var summary = FunctionSummary()
          if d.name.kind == SymbolDef and readSummaryPragma(d.pragmas, summary):
            result[d.name.symId] = summary
        else:
          skip n
  endRead(buf)

proc addSummaryPragma(dest: var TokenBuf; summary: FunctionSummary; info: PackedLineInfo) =
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
                             summary: FunctionSummary; info: PackedLineInfo) =
  var p = pragmas
  if p.kind == DotToken:
    dest.addParLe(TagId(PragmasU), info)
    addSummaryPragma(dest, summary, info)
    dest.addParRi()
  elif p.kind == ParLe:
    dest.add p
    var hadSummary = false
    p.into:
      while p.hasMore:
        if p.kind == ParLe and p.pragmaKind == SmryP:
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
  of ParLe:
    if n.stmtKind == ProcS:
      let tag = n.tagId
      let info = n.info
      let d = takeProcDecl(n)
      dest.addParLe(tag, info)
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      if d.name.kind == SymbolDef and summaries.hasKey(d.name.symId):
        writePragmasWithSummary(dest, d.pragmas,
                                summaries.getOrQuit(d.name.symId), info)
      else:
        dest.addSubtree d.pragmas
      dest.addSubtree d.body
      dest.addParRi()
    else:
      dest.add n.load()
      n.into:
        while n.hasMore:
          annotateSummaries(dest, n, summaries)
      dest.addParRi()
  of ParRi:
    assert false, "ParRi should not be encountered here"
  else:
    dest.takeToken n

proc annotateFunctionSummaries*(buf: var TokenBuf) =
  var analyses = collectProcAnalyses(buf)
  let summaries = resolveSummaries(analyses)

  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len + buf.len div 16)
  annotateSummaries(dest, n, summaries)
  endRead(buf)
  buf = ensureMove(dest)
