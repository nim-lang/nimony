#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## ARC-specific optimizer.
##
## Elides `=wasMoved(x); =destroy(x)` pairs when analysis can prove that
## all relevant paths move the same location before destruction.

import std / [assertions, intsets]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, programs]
from ".." / lengc / leng_model import takeProcDecl
# `lab`/`jmp` are Leng-only tags, so `nimony_model.stmtKind` reports `NoStmt`
# for them. Compare raw tag ids instead of routing them through a `StmtKind`.
# Qualified-only: `LengStmt` shares field names such as `CallS` with
# `NimonyStmt`, so an unqualified import would be ambiguous.
from ".." / models / leng_tags import nil
import ".." / lib / symparser
import passes

type
  BasicBlockKind = enum
    BbOther, BbWhileStmt, BbBlockStmt

  WasMovedLoc = object
    callPos: int
    target: Cursor

  BasicBlock = object
    wasMovedLocs: seq[WasMovedLoc]
    kind: BasicBlockKind
    hasReturn, hasBreak: bool
    label: SymId
    parent: ptr BasicBlock
    symToDel: seq[Cursor]

  Con = object
    somethingTodo: bool
    inFinally: int
    toDelete: IntSet
    jmpTargets: IntSet
    source: ptr TokenBuf

const
  NoLabel = SymId(0)

proc getAttachedOp(symId: SymId; attachedOp: var AttachedOp): bool =
  var name = pool.syms[symId]
  extractBasename(name)
  # A specialized hook is minted by `lifter.generateHookName` as
  # `=<hookName>_<typeKey>`, so cut the type key off. `hookName` also spells
  # the op in lower case, hence the second spelling of `wasmoved`/`sinkh`
  # below. Without both, every specialized hook was invisible here.
  var i = 0
  while i < name.len and name[i] != '_': inc i
  if i < name.len: name.setLen i

  attachedOp = case name
    of "=destroy": attachedDestroy
    of "=wasMoved", "=wasmoved": attachedWasMoved
    of "=trace": attachedTrace
    of "=copy": attachedCopy
    of "=sink", "=sinkh": attachedSink
    of "=dup": attachedDup
    else: return false
  result = true

proc peelComparableArg(n: Cursor): Cursor =
  result = n
  while result.isTagLit:
    case result.exprKind
    of ExprX:
      var it = result
      it = sub(it)  # throwaway copy; bounds the walk under vpr
      while it.hasMore:
        if isLastSon(it):
          result = it
          break
        skip it
    of ConvKinds:
      inc result
      skip result # type argument
    of HaddrX, AddrX:
      inc result
    else:
      break

proc sameLocation(a, b: Cursor): bool {.inline.} =
  var a = peelComparableArg(a)
  var b = peelComparableArg(b)
  result = sameTrees(a, b)

proc nestedBlock(parent: var BasicBlock; kind = BbOther): BasicBlock =
  BasicBlock(
    wasMovedLocs: @[],
    kind: kind,
    hasReturn: false,
    hasBreak: false,
    label: NoLabel,
    parent: addr(parent),
    symToDel: @[]
  )

proc breakStmt(b: var BasicBlock; n: Cursor) =
  var it = addr(b)
  while it != nil:
    it.wasMovedLocs.setLen 0
    it.hasBreak = true

    if n.isSymbol:
      if it.label == n.symId:
        break
    else:
      if it.kind in {BbWhileStmt, BbBlockStmt}:
        break

    it = it.parent

proc returnStmt(b: var BasicBlock) =
  b.hasReturn = true
  var it = addr(b)
  while it != nil:
    it.wasMovedLocs.setLen 0
    it = it.parent

proc joinStmt(b: var BasicBlock) =
  ## A label that some `jmp` targets is a join: the other predecessors need
  ## not have run the pending `=wasMoved` calls, so nothing survives it.
  var it = addr(b)
  while it != nil:
    it.wasMovedLocs.setLen 0
    it = it.parent

proc labelOf(n: Cursor): SymId =
  ## The label of a `(lab :L)` or the target of a `(jmp L)`.
  result = NoLabel
  if n.isTagLit:
    var it = n
    it = sub(it)  # throwaway copy; bounds the peek under vpr
    if it.kind in {Symbol, SymbolDef}:
      result = it.symId

proc collectJmpTargets(targets: var IntSet; n: var Cursor) =
  case n.kind
  of TagLit:
    if n.cursorTagId == TagId(leng_tags.JmpS):
      let lab = labelOf(n)
      if lab != NoLabel: targets.incl int(lab)
      skip n
    else:
      n.into:
        while n.hasMore and n.kind != EofToken:
          collectJmpTargets(targets, n)
  else:
    inc n

proc invalidateWasMoved(c: var BasicBlock; x: Cursor) =
  var i = 0
  while i < c.wasMovedLocs.len:
    if sameLocation(c.wasMovedLocs[i].target, x):
      c.wasMovedLocs.del i
    else:
      inc i

proc mergeBasicBlockInfo(parent: var BasicBlock; this: BasicBlock) {.inline.} =
  if this.hasReturn:
    parent.wasMovedLocs.setLen 0
    parent.hasReturn = true
  elif this.symToDel.len > 0:
    parent.symToDel = this.symToDel
    for i in this.symToDel:
      invalidateWasMoved(parent, i)

proc wasMovedTarget(matches: var IntSet; branch: seq[WasMovedLoc];
                    moveTarget: Cursor): bool =
  result = false
  for i in 0..<branch.len:
    if sameLocation(branch[i].target, moveTarget):
      result = true
      matches.incl i

proc intersect(summary: var seq[WasMovedLoc]; branch: seq[WasMovedLoc]) =
  var i = 0
  var matches = initIntSet()
  while i < summary.len:
    if wasMovedTarget(matches, branch, summary[i].target):
      inc i
    else:
      summary.del i
  for m in matches:
    summary.add branch[m]

proc isExhaustive(n: Cursor; exhaustiveKind: SubstructureKind): bool =
  var it = n
  if not it.isTagLit:
    return false
  it = sub(it)  # throwaway copy; bounds the walk under vpr
  var lastKind = NoSub
  while it.hasMore:
    lastKind = it.substructureKind
    skip it
  result = lastKind == exhaustiveKind

proc getCallInfo(n: Cursor; attachedOp: var AttachedOp; arg: var Cursor): bool =
  if not n.isTagLit:
    return false
  if n.exprKind notin CallKinds and n.stmtKind notin {CallS, CmdS, HcallS, CallstrlitS, InfixS, PrefixS}:
    return false
  var it = n
  it = sub(it)  # throwaway copy; bounds the peek under vpr
  if it.kind != Symbol:
    return false
  if not getAttachedOp(it.symId, attachedOp):
    return false
  inc it
  if not it.hasMore:
    return false
  arg = it
  result = true

proc wasMovedDestroyPair(c: var Con; b: var BasicBlock; d, darg: Cursor) =
  var i = 0
  while i < b.wasMovedLocs.len:
    if sameLocation(b.wasMovedLocs[i].target, darg):
      c.toDelete.incl b.wasMovedLocs[i].callPos
      c.toDelete.incl cursorToPosition(c.source[], d)
      c.somethingTodo = true
      b.wasMovedLocs.del i
    else:
      inc i

proc analyse(c: var Con; b: var BasicBlock; n: var Cursor)

proc analyseChildren(c: var Con; b: var BasicBlock; n: var Cursor) =
  assert n.isTagLit
  n.into:
    while n.hasMore and n.kind != EofToken:
      let before = cursorToPosition(c.source[], n)
      analyse(c, b, n)
      if n.hasMore and n.kind != EofToken and cursorToPosition(c.source[], n) == before:
        skip n

proc analyse(c: var Con; b: var BasicBlock; n: var Cursor) =
  case n.kind
  of Symbol:
    b.invalidateWasMoved n
    b.symToDel.add n
    inc n
  of TagLit:
    if n.stmtKind in {ProcS, FuncS, ConverterS, MethodS, IteratorS, MacroS, TemplateS} or
        isDeclarative(n):
      skip n
      return

    var op = attachedDestroy
    var callArg = default(Cursor)
    var reverse = false
    if getCallInfo(n, op, callArg):
      case op
      of attachedWasMoved:
        b.wasMovedLocs.add WasMovedLoc(
          callPos: cursorToPosition(c.source[], n),
          target: peelComparableArg(callArg)
        )
        # Do not descend into the children of `=wasMoved`/`=destroy`. Their
        # argument *is* the location being tracked, and the `Symbol` case
        # counts every occurrence as a use and calls `invalidateWasMoved` —
        # so descending deleted the entry that was just added, one line above,
        # and no pair could ever be found.
        skip n
        return
      of attachedDestroy:
        if c.inFinally > 0 and (b.hasReturn or b.hasBreak):
          discard
        else:
          c.wasMovedDestroyPair b, n, callArg
        skip n
        return
      of attachedSink:
        reverse = true
      of attachedDup, attachedCopy, attachedTrace:
        discard

    # `jmp`/`lab` carry the control flow that `try`/`return`/`break` had before
    # Leng lowering. They are Leng-only tags, so the `stmtKind` dispatch below
    # sees `NoStmt` and would walk them as plain statements — which lets a
    # `=wasMoved` pair with a `=destroy` that a jump can reach without it.
    let tag = n.cursorTagId
    if tag == TagId(leng_tags.JmpS):
      returnStmt(b)
      skip n
      return
    if tag == TagId(leng_tags.LabS):
      if c.jmpTargets.contains(int(labelOf(n))):
        joinStmt(b)
      skip n
      return

    case n.stmtKind
    of AsgnS:
      n.into:
        var le = n
        skip n
        if n.hasMore:
          # analyse the rhs (advancing `n` so the scope is consumed) before
          # the lhs, mirroring evaluation order for wasMoved tracking.
          analyse(c, b, n)
          analyse(c, b, le)
    of IfS:
      let exhaustive = isExhaustive(n, ElseU)
      var wasMovedSet: seq[WasMovedLoc] = @[]
      n.into:
        var i = 0
        while n.hasMore:
          var branch = nestedBlock(b)
          analyse(c, branch, n)
          mergeBasicBlockInfo(b, branch)
          if exhaustive:
            if i == 0:
              wasMovedSet = move(branch.wasMovedLocs)
            else:
              wasMovedSet.intersect(branch.wasMovedLocs)
          inc i
      for x in wasMovedSet:
        b.wasMovedLocs.add x
    of CaseS:
      let exhaustive = isExhaustive(n, ElseU)
      var wasMovedSet: seq[WasMovedLoc] = @[]
      n.into:
        if n.hasMore:
          analyse(c, b, n) # selector
        var i = 0
        while n.hasMore:
          var branch = nestedBlock(b)
          analyse(c, branch, n)
          mergeBasicBlockInfo(b, branch)
          if exhaustive:
            if i == 0:
              wasMovedSet = move(branch.wasMovedLocs)
            else:
              wasMovedSet.intersect(branch.wasMovedLocs)
          inc i
      for x in wasMovedSet:
        b.wasMovedLocs.add x
    of TryS:
      n.into:
        while n.hasMore:
          var body = nestedBlock(b)
          analyse(c, body, n)
          mergeBasicBlockInfo(b, body)
    of WhileS:
      n.into:
        if n.hasMore:
          analyse(c, b, n) # cond
        if n.hasMore:
          var loopBody = nestedBlock(b, BbWhileStmt)
          analyse(c, loopBody, n)
          mergeBasicBlockInfo(b, loopBody)
    of BlockS:
      var blockBody = nestedBlock(b, BbBlockStmt)
      n.into:
        if n.kind == SymbolDef:
          blockBody.label = n.symId
        skip n
        if n.hasMore:
          analyse(c, blockBody, n)
          mergeBasicBlockInfo(b, blockBody)
    of BreakS:
      let label = n.childCursor
      breakStmt(b, label)
      skip n
    of RetS, RaiseS:
      analyseChildren(c, b, n)
      returnStmt(b)
    else:
      if n.substructureKind == FinU:
        inc c.inFinally
        analyseChildren(c, b, n)
        dec c.inFinally
      elif reverse:
        var sons: seq[Cursor] = @[]
        n.into:
          while n.hasMore:
            sons.add n
            skip n
        for i in countdown(sons.high, 0):
          var son = sons[i]
          analyse(c, b, son)
      else:
        analyseChildren(c, b, n)
  else: # atoms and suffix kinds; classic: also a physical ParRi
    inc n

proc opt(c: Con; n: var Cursor; dest: var TokenBuf) =
  case n.kind
  of TagLit:
    let pos = cursorToPosition(c.source[], n)
    if c.toDelete.contains(pos):
      dest.addDotToken(n.info)
      skip n
    else:
      takeInto dest, n:
        while n.hasMore:
          opt(c, n, dest)
  else:
    dest.takeTree n

proc optimizeArc*(pass: var Pass) =
  var c = Con(
    somethingTodo: false,
    inFinally: 0,
    toDelete: initIntSet(),
    jmpTargets: initIntSet(),
    source: addr pass.buf
  )
  var targetScan = pass.n
  collectJmpTargets(c.jmpTargets, targetScan)
  var b = BasicBlock(
    wasMovedLocs: @[],
    kind: BbOther,
    hasReturn: false,
    hasBreak: false,
    label: NoLabel,
    parent: nil,
    symToDel: @[]
  )

  var analysis = pass.n
  analyse(c, b, analysis)

  var n = pass.n
  opt(c, n, pass.dest)

proc runArcoptBody(buf: var TokenBuf; moduleSuffix = ""; bits = 0) =
  var input = createTokenBuf(buf.len)
  swap(input, buf)
  var pass = initPass(ensureMove(input), moduleSuffix, "arcopt", bits)
  optimizeArc(pass)
  pass.finishPass()
  buf = ensureMove(pass.dest)

proc runArcoptTree(dest: var TokenBuf; n: var Cursor; moduleSuffix: string; bits: int) =
  case n.kind
  of TagLit:
    let tag = n.cursorTagId
    let info = n.info
    if n.stmtKind == ProcS:
      let d = takeProcDecl(n)
      dest.addParLe(tag, info)
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      dest.addSubtree d.pragmas
      if d.body.isTagLit:
        var body = createTokenBuf(64)
        var b = d.body
        body.addSubtree b
        runArcoptBody(body, moduleSuffix, bits)
        var rb = beginRead(body)
        dest.addSubtree rb
      else:
        dest.addSubtree d.body
      dest.addParRi()
    else:
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          runArcoptTree(dest, n, moduleSuffix, bits)
      dest.addParRi()
  else:
    dest.takeTree n

proc runArcopt*(buf: var TokenBuf; moduleSuffix = ""; bits = 0) =
  ## Direct entry point for already generated NIFC buffers.
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  runArcoptTree(dest, n, moduleSuffix, bits)
  buf = ensureMove(dest)
