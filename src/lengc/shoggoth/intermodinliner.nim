#
#
#           NIFC Inter-Module Inliner
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Inter-module inliner for generated NIFC.
##
## Hexer's `intramodinliner` annotates each `.inline` proc's pragma with a
## threshold followed by per-parameter weights (see `intramodinliner`'s
## `computeInlineInfo`). This pass consumes those annotations and splices
## qualifying calls at NIFC level — both same-module and cross-module:
## cross-module bodies live in the matching `.x.nif` files, picked up via
## `intramodinliner`'s lazy foreign-module loader (`xnifDir` plus a one-level-up
## search, so the main module's `.x.nif` inside `<nimcache>/<backend>/` and
## non-main modules' `.x.nif` directly in `<nimcache>/` are both found).
##
## In addition to the full splice, this pass detects a *partial-inline
## prologue* — the guarded early-return shape
##
##   (stmts (if (elif COND (stmts (ret EXPR)))) … rest …)
##
## — and rewrites the call site so the cheap guard runs inline; if the
## guard does not fire the original call is made instead. That captures
## the common-case speedup without paying to inline the cold tail.

import std / [tables, assertions, os]
include "../../lib" / nifprelude
import nifpools
import ".." / leng_model
import ".." / ".." / hexer / intramodinliner
import ".." / ".." / lib / symparser

# ---- partial-inline detection --------------------------------------------

type
  GuardShape = object
    cond: Cursor       ## the guard expression (`elif`'s condition slot)
    retVal: Cursor     ## the `(ret X)`'s value cursor — `DotToken` for void
    voidReturn: bool

proc tryDetectGuardPrologue(body: Cursor; shape: var GuardShape): bool =
  ## Recognise the partial-inline shape at the *head* of a proc body:
  ##
  ##   (stmts (if (elif COND (stmts (ret X)))) …)
  ##
  ## The elif's body must be a single `(ret …)` (optionally wrapped in a
  ## length-1 `(stmts …)`); there must be exactly one `elif` (no further
  ## branches, no `else`). Returns true on match and fills `shape`.
  ##
  ## Implementation note: walks via `childCursor` + `into` + `hasMore`
  ## so the bounded child loop terminates correctly.
  if not body.isTagLit or body.stmtKind != StmtsS: return false
  let firstStmt = childCursor(body)
  if not firstStmt.isTagLit or firstStmt.stmtKind != IfS: return false

  # The if must contain exactly one branch, and it must be an elif.
  var theElif = default(Cursor)
  var branchCount = 0
  var rejected = false
  var ifn = firstStmt
  ifn.into:
    while ifn.hasMore:
      inc branchCount
      if branchCount == 1 and ifn.isTagLit and
         ifn.substructureKind == ElifU:
        theElif = ifn
      else:
        rejected = true
      skip ifn
  if rejected or branchCount != 1: return false

  # Inside (elif COND BODY): grab COND and BODY.
  let condCur = childCursor(theElif)
  shape.cond = condCur
  var elifBody = condCur
  skip elifBody                                # past condition, on body

  # Optionally unwrap a single-statement `(stmts (ret …))`.
  var retCur = elifBody
  if retCur.isTagLit and retCur.stmtKind == StmtsS:
    var saved = default(Cursor)
    var stmtCount = 0
    var inner = retCur
    inner.into:
      while inner.hasMore:
        inc stmtCount
        if stmtCount == 1: saved = inner
        skip inner
    if stmtCount != 1: return false
    retCur = saved

  if not retCur.isTagLit or retCur.stmtKind != RetS: return false
  let retVal = childCursor(retCur)             # `.` for void, else the value
  shape.retVal = retVal
  shape.voidReturn = retVal.kind == DotToken
  result = true

# ---- public entry --------------------------------------------------------

proc runInterModuleInliner*(buf: var TokenBuf; suffix: string;
                            xnifDir: string): bool =
  ## Returns true when `buf` was changed.
  ##
  ## Reuses `intramodinliner.trIntra`, which already calls `trySplice` /
  ## `trySpliceVarInit` at every statement-position call and bound-form
  ## `(var :t … (call …))`. The cross-module body fetch is automatic once
  ## `xnifDir` is set — `lookupBody` resolves the callee's module via the
  ## symbol name (`extractModule`) and lazy-loads the foreign `.x.nif`.
  var infos = initTable[string, ModuleAnalysis]()
  # Record the current module's OWN inline info so same-module `.inline` callees
  # are recognised — `lookupInlineInfo` only consults `infos`, and the foreign
  # path never populates the suffix we're processing.
  infos[suffix] = analyzeModule(buf)
  var ctx = initInlinerCtx(suffix, addr buf, addr infos,
                           xnifDir = xnifDir,
                           maxDepth = 4,
                           counterPrefix = "x")
  collectProcBodies(ctx)

  let originalLen = buf.len
  var dest = createTokenBuf(buf.len + buf.len div 8)
  var n = beginRead(buf)
  trIntra(ctx, dest, n)
  result = dest.len != originalLen
  buf = ensureMove(dest)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    result = parseFromBuffer(src, "M")

  for s in ["x.0.M", "cond.0.M"]:
    discard pool.syms.getOrIncl(s)

  block matches_guard_prologue:
    var buf = parse(
      "(stmts (if (elif cond.0.M (stmts (ret 42)))) (asgn x.0.M 1))")
    var shape = GuardShape()
    let body = beginRead(buf)
    doAssert tryDetectGuardPrologue(body, shape)
    doAssert not shape.voidReturn
    doAssert shape.retVal.kind == IntLit

  block rejects_no_if:
    var buf = parse("(stmts (asgn x.0.M 1))")
    var shape = GuardShape()
    let body = beginRead(buf)
    doAssert not tryDetectGuardPrologue(body, shape)

  block rejects_if_with_else:
    var buf = parse(
      "(stmts (if (elif cond.0.M (stmts (ret 1))) (else (stmts (ret 2)))))")
    var shape = GuardShape()
    let body = beginRead(buf)
    doAssert not tryDetectGuardPrologue(body, shape)

  block rejects_elif_with_extra_stmts:
    var buf = parse(
      "(stmts (if (elif cond.0.M (stmts (asgn x.0.M 1) (ret 2)))))")
    var shape = GuardShape()
    let body = beginRead(buf)
    doAssert not tryDetectGuardPrologue(body, shape)

  block matches_void_guard:
    var buf = parse(
      "(stmts (if (elif cond.0.M (stmts (ret .)))) (asgn x.0.M 1))")
    var shape = GuardShape()
    let body = beginRead(buf)
    doAssert tryDetectGuardPrologue(body, shape)
    doAssert shape.voidReturn

  echo "intermodinliner.nim: all self-tests passed"
