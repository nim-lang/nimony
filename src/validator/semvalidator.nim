#
#
#           Nimony Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Cursor/buffer discipline checks over a *semchecked* module.
##
## Same obligations as the untyped engine in `validator.nim`, but every
## judgement it used to make on a spelling is made on a symbol here (see
## `semfacts.nim`), and one judgement is new: whether a node is code the module
## actually contains.
##
## What an operation *does* is not decided here either. `skip` advancing the
## cursor and `takeTree` emitting what it moved over is the API's own
## knowledge, declared with the `nifroles` pragmas on the declarations and read
## back off them — for a proc through the callee symbol, for a template through
## the symbol its expansion's provenance names.
##
## That last one matters because sem expands templates. `n.loopInto: body` is
## gone by the time we see the tree; what is left is the loop `loopInto` is
## made of, including its own `inc n` — an advance the author never wrote and
## must not be blamed for. Every walk here therefore carries `inLib`, set as
## soon as it enters an expansion and cleared again the moment the tokens point
## back at the module's own source, which is exactly where the block argument
## the author *did* write begins. Library-expanded operations are still
## classified, but as the single idiom the author invoked.

import std / [tables, strutils, assertions, terminal, syncio]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / models / [tags, nimony_tags]
import ".." / nimony / [nimony_model, decls, programs]
import semfacts

type
  Violation* = object
    line*: int
    col*: int
    file*: string
    tag*: string
    msg*: string
    isWarning*: bool

  SemCheckContext* = object
    m*: SemModule
    violations*: seq[Violation]
    strict*: bool
    noColors*: bool

# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------

proc addViolationAt(ctx: var SemCheckContext; info: NifLineInfo; tag, msg: string;
                    isWarning: bool) =
  ## Diagnostics are always attributed to the module's own source: a violation
  ## found inside an expansion is the author's to fix, not `nifcore`'s, so the
  ## forged provenance filename never reaches the user.
  let f = realFile(fileOf(info))
  ctx.violations.add Violation(line: info.line.int, col: info.col.int + 1,
                               file: (if f.len > 0: f else: ctx.m.file),
                               tag: tag, msg: msg, isWarning: isWarning)

proc addViolation(ctx: var SemCheckContext; info: NifLineInfo; tag, msg: string) =
  addViolationAt(ctx, info, tag, msg, false)

proc addWarning(ctx: var SemCheckContext; info: NifLineInfo; tag, msg: string) =
  addViolationAt(ctx, info, tag, msg, true)

proc writeViolation*(ctx: SemCheckContext; v: Violation) =
  let loc = v.file & "(" & $v.line & ", " & $v.col & ")"
  let tagInfo = if v.tag.len > 0: " [" & v.tag & "]" else: ""
  if ctx.noColors:
    if v.isWarning:
      stdout.writeLine loc, " Warning: ", v.msg, tagInfo
    else:
      stdout.writeLine loc, " Error: ", v.msg, tagInfo
  else:
    if v.isWarning:
      stdout.styledWriteLine fgCyan, loc, " ", resetStyle,
        fgYellow, styleBright, "Warning: ", resetStyle, v.msg, tagInfo
    else:
      stdout.styledWriteLine fgCyan, loc, " ", resetStyle,
        fgRed, styleBright, "Error: ", resetStyle, v.msg, tagInfo

# ---------------------------------------------------------------------------
# Operation classification
# ---------------------------------------------------------------------------

const
  DelegateFallback = ["trExpr", "trStmt", "trLocal", "trProcDecl", "tr"]
    ## The one place a name is still matched. A pass's own traversal entry
    ## points hand the cursor on, and only the pass author can say so — with
    ## `{.nifDelegates.}` on the declaration. Until the passes carry it, these
    ## five names keep the compiler-pass checks working; nothing in the plugin
    ## API is matched by name any more.

  SkipIntentNames = ["SkipTag", "SkipParRi", "SkipName", "SkipExport",
                     "SkipPragmas", "SkipType", "SkipValue", "SkipGenParams",
                     "SkipEffects",
                     "SkipCond", "SkipBody", "SkipExpr", "SkipResult", "SkipFull",
                     "Anything", "AnyExpr", "AnyStmt", "AnyType"]
    ## Closed set: the classic `SkipIntent` roles plus the structural
    ## `TagClass` categories.

proc roleOf(n: Cursor; origin: NodeOrigin; idiom: string): OpRole =
  ## What a node does, whether it survived as a call or a template expanded it
  ## into the tree. Either way the answer comes from a declaration.
  if origin == noIdiom:
    result = roleOfMangled(idiom)
  elif n.isTagLit and n.exprKind in CallKinds:
    let callee = calleeSym(n)
    result = roleOfSym(callee)
    if result == roleNone and baseName(callee) in DelegateFallback:
      result = roleDelegates
  else:
    result = roleNone

proc opDisplayName(n: Cursor; origin: NodeOrigin; idiom: string): string =
  ## How to name the operation in a diagnostic.
  if origin == noIdiom:
    result = idiom
    extractBasename result
  elif n.isTagLit and n.exprKind in CallKinds:
    result = baseName(calleeSym(n))
  else:
    result = ""

proc hasIntentArg(n: Cursor): bool =
  ## True when a `skip`/`inc` call carries a `SkipIntent`/`TagClass`/tag-enum
  ## argument justifying it. A resolved enum value is a symbol, so the check is
  ## an identity test rather than the untyped engine's "any identifier will do".
  for a in callArgs(n):
    if a.kind == StrLit: return true
    if a.kind == Symbol:
      let b = baseName(a.symId)
      if b in SkipIntentNames: return true
      # a per-language tag enum value (`IfS`, `ProcS`, …): hundreds of them,
      # so accept any enum-shaped symbol that is not one of our locals.
      if b.len > 1 and b[0].isUpperAscii: return true
  false

# ---------------------------------------------------------------------------
# Walking user code
# ---------------------------------------------------------------------------

template eachChild(n: Cursor; body: untyped) =
  ## Iterate the children of a tag node; `child` is bound in `body`.
  if n.isTagLit:
    var child {.inject.} = childCursor(n)
    while child.hasMore:
      body
      skip child

proc classifyCall(p: ProcFacts; n: Cursor; origin: NodeOrigin; idiom: string): int =
  ## The balance contribution of one operation: positive when the cursor moved
  ## without anything being emitted, negative when output was produced without
  ## the cursor moving, zero when the two are tied.
  let isCall = n.isTagLit and n.exprKind in CallKinds
  case roleOf(n, origin, idiom)
  of roleAdvance:
    if isCall and hasTrackedArg(p, n, tkCursor):
      if hasIntentArg(n): return 0
      return 1
  of roleReads, roleBalanced, roleWrap, roleDelegates:
    return 0
  of roleEmits:
    if isCall and hasTrackedArg(p, n, tkTokenBuf): return -1
  of roleNone:
    if isCall:
      let hasCur = hasTrackedArg(p, n, tkCursor)
      let hasBuf = hasTrackedArg(p, n, tkTokenBuf)
      if hasCur and hasBuf: return 0    # delegated
      elif hasCur: return 1
      elif hasBuf: return -1
  return 0

proc blockBalance(ctx: var SemCheckContext; p: ProcFacts; n: Cursor;
                  inLib: bool): int =
  result = 0
  if not n.isTagLit: return
  let (origin, idiom) = originOf(n.info, ctx.m.file)
  let nowInLib = (if origin == noUser: false else: true)

  if origin == noIdiom:
    # An expanded idiom is a leaf here, exactly as the un-expanded call was for
    # the untyped engine: what the author wrote is one operation.
    #
    # The block argument inside it is therefore not balanced either — and that
    # is the one place where the untyped engine's blind spot is worth keeping
    # for now. Descending would be new coverage, not a port: a `linearScan`
    # body legitimately advances the cursor it was handed, so the debt rule
    # needs its own calibration before the walk is allowed in there.
    if not inLib:
      return classifyCall(p, n, origin, idiom)
    return 0
  if nowInLib:
    # Library glue between expansions: nothing here is the author's, but a
    # block argument nested inside it is, so keep descending.
    eachChild(n):
      result += blockBalance(ctx, p, child, true)
    return

  if n.exprKind in CallKinds:
    return classifyCall(p, n, origin, idiom)

  case n.stmtKind
  of StmtsS:
    eachChild(n):
      result += blockBalance(ctx, p, child, false)
  of IfS, CaseS:
    eachChild(n):
      if child.isTagLit:
        let branch = child.substructureKind
        if branch in {ElifU, ElseU, OfU}:
          var inner = childCursor(child)
          if branch in {ElifU, OfU} and inner.hasMore:
            skip inner    # condition / ranges
          var b = 0
          while inner.hasMore:
            b += blockBalance(ctx, p, inner, false)
            skip inner
          if b > 0:
            ctx.addWarning child.info, p.name,
              "branch advances cursor " & $b &
              " more time(s) than it emits (possible dropped input)"
    # each branch is self-contained, so the whole `if`/`case` contributes 0
    result = 0
  of WhileS, ForS, BlockS, TryS:
    eachChild(n):
      discard blockBalance(ctx, p, child, false)
    result = 0
  else:
    eachChild(n):
      result += blockBalance(ctx, p, child, false)

proc scanCursorBufferBalance(ctx: var SemCheckContext) =
  for p in ctx.m.procs:
    if not p.hasCursor or not p.hasBuffer: continue
    discard blockBalance(ctx, p, p.body, false)

# ---------------------------------------------------------------------------
# Obligations: a `var Cursor` parameter must reach some call
# ---------------------------------------------------------------------------

proc countCursorArgs(p: ProcFacts; n: Cursor; counts: var seq[int]) =
  ## Expansions count too: `n.into: …` consumes `n` through `enterScope(n)`,
  ## which is as good a consumer as a call the author spelled out.
  if not n.isTagLit: return
  if n.exprKind in CallKinds:
    for a in callArgs(n):
      let lv = unwrapAddr(a)
      if lv.kind == Symbol:
        for i, cp in p.cursorParams:
          if lv.symId == cp:
            counts[i] += 1
  eachChild(n):
    countCursorArgs(p, child, counts)

proc scanObligations(ctx: var SemCheckContext) =
  ## A `var Cursor` parameter that is never handed to any call is a parameter
  ## whose subtree nobody consumes.
  for p in ctx.m.procs:
    if p.cursorParams.len == 0: continue
    var counts = newSeq[int](p.cursorParams.len)
    countCursorArgs(p, p.body, counts)
    for i, cp in p.cursorParams:
      if counts[i] == 0:
        let v = p.vars[cp]
        ctx.addWarning p.info, p.name,
          "parameter `" & v.name & ": var Cursor` is never passed to any call"

# ---------------------------------------------------------------------------
# Unsafe cursor ops: a bare `skip`/`inc` on a cursor parameter
# ---------------------------------------------------------------------------

proc scanUnsafeCursorOpsIn(ctx: var SemCheckContext; p: ProcFacts; n: Cursor;
                           delegated, inLib: bool) =
  if not n.isTagLit: return
  let (origin, idiom) = originOf(n.info, ctx.m.file)
  var nowInLib = (if origin == noUser: false else: true)
  var delegatedHere = delegated

  if not inLib and n.exprKind in CallKinds:
    let op = roleOf(n, origin, idiom)
    if op == roleAdvance and not hasIntentArg(n):
      block flagFirst:
        for a in callArgs(n):
          let lv = unwrapAddr(a)
          if lv.kind == Symbol:
            for cp in p.cursorParams:
              if lv.symId == cp:
                ctx.addWarning n.info, p.name,
                  "`" & opDisplayName(n, origin, idiom) & " " & p.vars[cp].name &
                  "` needs a SkipIntent argument for justification"
                break flagFirst
    elif op notin {roleAdvance, roleBalanced, roleWrap}:
      if hasTrackedArg(p, n, tkCursor):
        delegatedHere = true

  eachChild(n):
    scanUnsafeCursorOpsIn(ctx, p, child, delegatedHere, nowInLib)

proc scanUnsafeCursorOps(ctx: var SemCheckContext) =
  for p in ctx.m.procs:
    if not p.hasCursor or not p.hasBuffer: continue
    scanUnsafeCursorOpsIn(ctx, p, p.body, false, false)

# ---------------------------------------------------------------------------
# Exhaustive `case` over a tag-kind discriminator
# ---------------------------------------------------------------------------

const ExhaustiveDiscriminators = [
  "stmtKind", "exprKind", "typeKind", "substructureKind", "symKind"]

proc scanForNonExhaustiveCases(ctx: var SemCheckContext) =
  ## `case n.stmtKind` must enumerate its values so that adding a tag breaks
  ## every pass that has to be reviewed. Post-sem the discriminator is a
  ## resolved call, so the accessor is named by symbol rather than by the last
  ## field of a dot expression.
  for p in ctx.m.procs:
    var n = p.body
    n.linearScan:
      if n.stmtKind == CaseS:
        let (origin, _) = originOf(n.info, ctx.m.file)
        if origin == noUser:
          var peek = childCursor(n)
          var discr = ""
          if peek.isTagLit and peek.exprKind in CallKinds:
            discr = baseName(calleeSym(peek))
          if discr in ExhaustiveDiscriminators:
            if peek.hasMore: skip peek
            while peek.hasMore:
              if peek.isTagLit and peek.substructureKind == ElseU:
                ctx.addViolation n.info, "case " & discr,
                  "`else` branch not allowed; enumerate all values for exhaustive checking"
                break
              skip peek

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

proc dumpFacts*(ctx: SemCheckContext) =
  ## `--dump`: what the front end made of the module. The first thing to look
  ## at when a check fires where it should not, or stays quiet where it should.
  echo "module ", ctx.m.file, " [", ctx.m.suffix, "] ", ctx.m.procs.len, " proc(s)"
  for p in ctx.m.procs:
    echo "  proc ", p.name, "  cursor=", p.hasCursor, " buffer=", p.hasBuffer
    for cp in p.cursorParams:
      echo "    var Cursor param: ", p.vars[cp].name
    for _, v in p.vars:
      if v.tracked != tkUnknown and v.tracked != tkOther:
        echo "    ", (if v.isParam: "param " else: "local "), v.name, ": ",
             (if v.isMut: "var " else: ""), v.tracked

proc validateSemModule*(nifFile, sourceFile: string; strict, noColors: bool;
                        dump = false): int =
  ## Runs every check over one semchecked module. Returns the number of errors
  ## (warnings do not affect the exit code).
  var owningBuf = default(TokenBuf)
  var ctx = SemCheckContext(strict: strict, noColors: noColors)
  ctx.m = openSemModule(nifFile, sourceFile, owningBuf)
  if dump: dumpFacts ctx

  scanObligations ctx
  scanCursorBufferBalance ctx
  if strict:
    scanUnsafeCursorOps ctx
    scanForNonExhaustiveCases ctx

  result = 0
  for v in ctx.violations:
    if not v.isWarning: inc result
    ctx.writeViolation v
