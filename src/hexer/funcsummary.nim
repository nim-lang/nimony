#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Small NIFC function summaries shared by late Hexer passes and the NIFC
## optimizer. The wire format is NIF, for example:
## `(smry read writeGlobal (param 0 read directEscape))`.

import std / [assertions, tables]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nifc / [nifc_model]

type
  ParamSummary* = object
    read*, written*, directEscape*, returned*: bool

  FunctionSummary* = object
    readsMemory*, writesMemory*, writesGlobal*, callsUnknown*, raises*: bool
    params*: seq[ParamSummary]

  FunctionSummaryTable* = Table[SymId, FunctionSummary]

  CallFact = object
    callee: SymId
    args: seq[int]       # actual argument -> caller param index, or -1

  ProcAnalysis = object
    summary: FunctionSummary
    calls: seq[CallFact]

proc isReadOnly*(s: FunctionSummary): bool {.inline.} =
  not s.writesMemory and not s.writesGlobal

proc paramMayWrite*(s: FunctionSummary; idx: int): bool {.inline.} =
  idx < 0 or idx >= s.params.len or s.params[idx].written

proc paramDirectEscapes*(s: FunctionSummary; idx: int): bool {.inline.} =
  idx >= 0 and idx < s.params.len and s.params[idx].directEscape

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

proc firstSymbolIn(c: Cursor): SymId =
  if not c.hasMore: return SymId(0)
  case c.kind
  of Symbol: c.symId
  of ParLe:
    var n = c
    n.loopInto:
      let s = firstSymbolIn(n)
      if s != SymId(0): return s
      skip n
    SymId(0)
  else: SymId(0)

proc paramIndex(params: Table[SymId, int]; c: Cursor): int =
  let s = firstSymbolIn(c)
  if s != SymId(0) and params.hasKey(s): params.getOrQuit(s) else: -1

proc markRead(s: var FunctionSummary; idx: int) =
  if idx >= 0 and idx < s.params.len:
    s.params[idx].read = true
    s.readsMemory = true

proc markWrite(s: var FunctionSummary; idx: int) =
  s.writesMemory = true
  if idx >= 0 and idx < s.params.len:
    s.params[idx].written = true
  else:
    s.writesGlobal = true

proc markDirectEscape(s: var FunctionSummary; idx: int) =
  if idx >= 0 and idx < s.params.len:
    s.params[idx].directEscape = true
  else:
    s.callsUnknown = true

proc markUnknownCall(s: var FunctionSummary) =
  s.readsMemory = true
  s.writesMemory = true
  s.writesGlobal = true
  s.callsUnknown = true

proc markUnknownParamEffects(s: var FunctionSummary) =
  markUnknownCall s
  for p in mitems(s.params):
    p.read = true
    p.written = true
    p.directEscape = true

proc analyzeExpr(a: var ProcAnalysis; params: Table[SymId, int]; n: var Cursor)

proc analyzeCall(a: var ProcAnalysis; params: Table[SymId, int]; n: var Cursor) =
  let callee = n.firstSon
  var fact = CallFact(callee: SymId(0), args: @[])
  if callee.kind == Symbol:
    fact.callee = callee.symId
  else:
    markUnknownCall a.summary
  n.into:
    if n.hasMore: skip n
    while n.hasMore:
      let idx = paramIndex(params, n)
      if fact.callee == SymId(0):
        markDirectEscape(a.summary, idx)
      else:
        fact.args.add idx
      analyzeExpr(a, params, n)
  if fact.callee != SymId(0):
    a.calls.add ensureMove fact

proc analyzeExpr(a: var ProcAnalysis; params: Table[SymId, int]; n: var Cursor) =
  case n.kind
  of Symbol:
    if params.hasKey(n.symId):
      markRead(a.summary, params.getOrQuit(n.symId))
    inc n
  of ParLe:
    case n.exprKind
    of AddrC:
      let idx = paramIndex(params, n.firstSon)
      markDirectEscape(a.summary, idx)
      skip n
    of CallC:
      analyzeCall(a, params, n)
    else:
      n.loopInto:
        analyzeExpr(a, params, n)
  else:
    inc n

proc analyzeStmt(a: var ProcAnalysis; params: Table[SymId, int]; n: var Cursor) =
  case n.kind
  of ParLe:
    case n.stmtKind
    of AsgnS, StoreS:
      n.into:
        if n.hasMore:
          let lhs = n
          markWrite(a.summary, paramIndex(params, lhs))
          skip n
        if n.hasMore:
          analyzeExpr(a, params, n)
        while n.hasMore: skip n
    of CallS:
      analyzeCall(a, params, n)
    of RetS:
      n.into:
        if n.hasMore:
          let idx = paramIndex(params, n)
          if idx >= 0 and idx < a.summary.params.len:
            a.summary.params[idx].returned = true
          analyzeExpr(a, params, n)
        while n.hasMore: skip n
    of RaiseS:
      a.summary.raises = true
      skip n
    else:
      n.loopInto:
        analyzeStmt(a, params, n)
  else:
    inc n

proc computeProcAnalysis(procDecl: Cursor): ProcAnalysis =
  result = ProcAnalysis()
  var p = procDecl
  let d = takeProcDecl(p)
  let paramSyms = collectParamSyms(d.params)
  result.summary.params = newSeq[ParamSummary](paramSyms.len)
  var lookup = initTable[SymId, int]()
  for i, s in paramSyms: lookup[s] = i
  if d.body.kind == ParLe:
    var body = d.body
    analyzeStmt(result, lookup, body)
  else:
    markUnknownParamEffects result.summary

proc computeFunctionSummary*(procDecl: Cursor): FunctionSummary =
  result = computeProcAnalysis(procDecl).summary

proc propagateCall(caller: FunctionSummary; call: CallFact;
                   callee: FunctionSummary): FunctionSummary =
  result = caller
  result.readsMemory = result.readsMemory or callee.readsMemory
  result.writesMemory = result.writesMemory or callee.writesMemory
  result.writesGlobal = result.writesGlobal or callee.writesGlobal
  result.callsUnknown = result.callsUnknown or callee.callsUnknown
  result.raises = result.raises or callee.raises

  for i, p in callee.params:
    let arg = if i < call.args.len: call.args[i] else: -1
    if p.read:
      markRead(result, arg)
    if p.written:
      if arg >= 0:
        markWrite(result, arg)
      else:
        result.writesMemory = true
    if p.directEscape:
      markDirectEscape(result, arg)
    if p.returned and arg >= 0 and arg < result.params.len:
      result.params[arg].returned = true

proc resolveSummaries(analyses: var Table[SymId, ProcAnalysis]) =
  const MaxIters = 8
  for _ in 0 ..< MaxIters:
    var changed = false
    for sym, a in analyses.mpairs:
      var next = a.summary
      for call in a.calls:
        if analyses.hasKey(call.callee):
          next = propagateCall(next, call, analyses.getOrQuit(call.callee).summary)
        else:
          markUnknownCall next
          for arg in call.args:
            markDirectEscape(next, arg)
      if next != a.summary:
        a.summary = ensureMove next
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

proc readParamSummary(n: var Cursor; outSummary: var FunctionSummary) =
  n.into:
    if n.kind != IntLit:
      while n.hasMore: skip n
      return
    let idx = int(pool.integers[n.intId])
    inc n
    if idx < 0:
      while n.hasMore: skip n
      return
    while outSummary.params.len <= idx:
      outSummary.params.add ParamSummary()
    while n.hasMore:
      if n.kind == Ident:
        case pool.strings[n.litId]
        of "read": outSummary.params[idx].read = true
        of "write": outSummary.params[idx].written = true
        of "directEscape": outSummary.params[idx].directEscape = true
        of "returned": outSummary.params[idx].returned = true
        else: discard
        inc n
      else:
        skip n

proc readSummary(n: var Cursor; outSummary: var FunctionSummary): bool =
  result = true
  outSummary = FunctionSummary()
  n.into:
    while n.hasMore:
      if n.kind == Ident:
        case pool.strings[n.litId]
        of "read": outSummary.readsMemory = true
        of "write": outSummary.writesMemory = true
        of "writeGlobal": outSummary.writesGlobal = true
        of "callsUnknown": outSummary.callsUnknown = true
        of "raises": outSummary.raises = true
        else: discard
        inc n
      elif n.kind == ParLe and n.substructureKind == ParamU:
        readParamSummary(n, outSummary)
      else:
        skip n

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
  if summary.readsMemory: dest.addIdent "read", info
  if summary.writesMemory: dest.addIdent "write", info
  if summary.writesGlobal: dest.addIdent "writeGlobal", info
  if summary.callsUnknown: dest.addIdent "callsUnknown", info
  if summary.raises: dest.addIdent "raises", info
  for i, p in summary.params:
    dest.addParLe(TagId(ParamU), info)
    dest.addIntLit i, info
    if p.read: dest.addIdent "read", info
    if p.written: dest.addIdent "write", info
    if p.directEscape: dest.addIdent "directEscape", info
    if p.returned: dest.addIdent "returned", info
    dest.addParRi()
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
    inc p
    var hadSummary = false
    while p.kind != ParRi:
      if p.kind == ParLe and p.pragmaKind == SmryP:
        addSummaryPragma(dest, summary, p.info)
        skip p
        hadSummary = true
      else:
        dest.takeTree p
    if not hadSummary:
      addSummaryPragma(dest, summary, info)
    dest.takeToken p
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
      dest.takeToken n
      while n.hasMore:
        annotateSummaries(dest, n, summaries)
      dest.takeToken n
  of ParRi:
    assert false, "ParRi should not be encountered here"
  else:
    dest.takeToken n

proc annotateFunctionSummaries*(buf: var TokenBuf) =
  var analyses = collectProcAnalyses(buf)
  resolveSummaries(analyses)
  var summaries = initTable[SymId, FunctionSummary]()
  for sym, a in analyses.pairs:
    summaries[sym] = a.summary

  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len + buf.len div 16)
  annotateSummaries(dest, n, summaries)
  endRead(buf)
  buf = ensureMove(dest)
