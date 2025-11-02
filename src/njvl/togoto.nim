#
#
#           Nimony Goto Insertion Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Undoes the effects of NJVL by turning `(jtrue X)` statements into goto statements, if possible.

## **THIS IS CURRENTLY UNUSED AND UNTESTED!**

## Overview
##
## This pass tries to “collapse” NJVL control-flow variables (cfvars) back into
## explicit jumps. In NJVL, structured control flow is expressed by:
## - Declaring a `cfvar` (starts as `false`, can only become `true`).
## - Setting it via `(jtrue cf)` inside conditions/branches.
## - Guarding execution with `if cf: ...` (or, more generally, `ite`/`loop`).
##
## The guiding rule (from the spec) is: converting `jtrue x` into a jump is only
## valid if no interim statements occur between the place that sets the cfvar and
## the point where control must transfer. Otherwise the cfvar must be
## materialized as data (`store true into cf`) and the control remains
## structured.
##
## - It traces `jtrue` sites and records whether they can be turned into `jmp`.
## - It tracks a simple per-symbol state `state[sym] = (value, activeCount)`:
##   - `value` reflects our current knowledge about the cfvar (starts `false`,
##     becomes `true` after a seen `jtrue`).
##   - `activeCount` is incremented/decremented while traversing `ite`
##     conditions to model “this branch is protected by cfvar X == true”. While
##     protection is active, side effects within the protected region do not
##     invalidate the potential jump for that cfvar.
## - If a side effect is encountered at a time when a cfvar is not “actively
##   guarding” the current code (`activeCount == 0`), we conservatively mark the
##   cfvar as `mustMaterialize` (cannot be rewritten to a jump).
## - In the emission phase, `(jtrue x, y, ...)` becomes either materialized
##   stores (keep cfvars as data) or a single `jmp <label>` if the last listed
##   cfvar can be a jump target (mirrors return/break lowering patterns).

import std / [tables, sets, assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav]
import ".." / hexer / [mover]

import njvl_model

type
  JtrueInstr = object
    pos: int
    isJump: bool

  Cfvar = (SymId, int) # name and version

  Context* = object
    typeCache: TypeCache
    state: Table[Cfvar, (bool, int)] # (value, active)
    mustMaterialize: HashSet[Cfvar]
    jtrueInstrs: Table[Cfvar, JtrueInstr]
    nextLabel: int

  CfvarState = object
    s: Cfvar
    value: bool # we always know the value of the cfvar
                # (they start out as false and can only become
                # true and then stay true)

  CfvarMask = seq[CfvarState]

proc getCfvar(n: var Cursor): Cfvar =
  inc n
  let s = n.symId
  inc n
  let v = pool.integers[n.intId]
  inc n
  skipParRi n
  result = (s, int(v))

proc computeCfvarMask(c: var Context; n: var Cursor; mask: var CfvarMask) =
  case n.kind
  of ParLe:
    if n.njvlKind == VV:
      inc n
      let cfvar = getCfvar(n)
      if c.state.hasKey(cfvar):
        mask.add CfvarState(s: cfvar, value: c.state[cfvar][0])
    else:
      case n.exprKind
      of NotX:
        inc n
        let oldLen = mask.len
        computeCfvarMask(c, n, mask)
        # change polarity for new entries:
        for i in oldLen..<mask.len:
          mask[i].value = not mask[i].value
        skipParRi n
      of AndX, OrX:
        inc n
        computeCfvarMask(c, n, mask)
        computeCfvarMask(c, n, mask)
        skipParRi n
      else:
        skip n
  else:
    skip n

proc aJoin(c: var Context; n: var Cursor) =
  inc n
  let sym = n.symId
  inc n
  let fresh = pool.integers[n.intId]
  inc n
  let old1 = pool.integers[n.intId]
  inc n
  let old2 = pool.integers[n.intId]
  inc n
  skipParRi n
  let freshX = (sym, int(fresh))
  let old1X = (sym, int(old1))
  let old2X = (sym, int(old2))
  # we know the joined value is `X or Y`:

  if c.state.hasKey(old1X) and c.state.hasKey(old2X):
    c.state[freshX] = (c.state[old1X][0] or c.state[old2X][0], 0)

proc aStmt(c: var Context; n: var Cursor) =
  case n.njvlKind
  of JtrueV:
    # we now know these symbols are true:
    inc n
    while n.kind != ParRi:
      let s = getCfvar(n)
      c.state[s][0] = true
    inc n
  of IteV, ItecV:
    inc n
    # we mask out cfvars here. Thus the statements that are
    # under their guard cause no trouble!
    var mask: CfvarMask = @[]
    computeCfvarMask(c, n, mask)
    # then branch, we know all the entries in the mask that are `true`
    # are protecting this branch:
    for m in mask.mitems:
      if m.value:
        inc c.state[m.s][1]
    aStmt c, n
    # in the else branch we know the other cfvars are active:
    for m in mask.mitems:
      if m.value:
        dec c.state[m.s][1]
      else:
        inc c.state[m.s][1]
    aStmt c, n
    aJoin c, n
    skipParRi n
  of CfvarV:
    inc n
    let s = getCfvar(n)
    # we know they start as false and inactive:
    c.state[s] = (false, 0)
    skipParRi n
  else:
    case n.stmtKind
    of AsgnS, CallKindsS:
      # we encountered an effect, so all cfvars not currently
      # disabled cannot be turned to jumps:
      for s, state in c.state:
        if state[1] == 0:
          c.mustMaterialize.incl s
    else:
      discard
    # ordinary recursion
    inc n
    while n.kind != ParRi:
      aStmt c, n
    inc n

proc labelFromCfvar(s: Cfvar): SymId =
  let name = pool.syms[s[0]] & "_" & $s[1]
  result = pool.syms.getOrIncl(name)

type
  Branch = enum
    ThenBranch,
    ElseBranch,
    UnknownBranch

proc pickBranch(c: Context; n: var Cursor): (Branch, SymId) =
  var n = n
  inc n
  if n.exprKind == NotX:
    inc n
    let (branch, label) = pickBranch(c, n)
    case branch
    of ThenBranch:
      result = (ElseBranch, label)
    of ElseBranch:
      result = (ThenBranch, label)
    of UnknownBranch:
      result = (UnknownBranch, label)
  elif n.njvlKind == VV:
    let s = getCfvar(n)
    if not c.mustMaterialize.contains(s):
      if c.state[s][0]:
        result = (ThenBranch, labelFromCfvar(s))
      else:
        result = (ElseBranch, labelFromCfvar(s))
    else:
      result = (UnknownBranch, NoSymId)
  else:
    result = (UnknownBranch, NoSymId)

proc emitJump(c: var Context; dest: var TokenBuf; label: SymId; info: PackedLineInfo) =
  dest.add tagToken("jmp", info)
  dest.addSymUse label, info
  dest.addParRi()

proc emitLabel(c: var Context; dest: var TokenBuf; label: SymId; info: PackedLineInfo) =
  dest.add tagToken("lab", info)
  dest.addSymDef label, info
  dest.addParRi()

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.njvlKind
  of JtrueV:
    let info = n.info
    # we now know these symbols are true:
    inc n
    var label = (NoSymId, 0)
    while n.kind != ParRi:
      let cfvar = n
      let s = getCfvar(n)
      if c.mustMaterialize.contains(s):
        dest.copyIntoKind StoreV, info:
          dest.addParPair TrueX, info
          dest.copyTree cfvar
      else:
        # see `ret` construction, etc. the last label counts
        label = s
      inc n
    inc n
    if label[0] != NoSymId:
      emitJump(c, dest, labelFromCfvar(label), info)
  of IteV, ItecV:
    var cond = n.firstSon
    let (branch, targetLabel) = pickBranch(c, cond)
    case branch
    of ThenBranch:
      inc n
      skip n
      # We need to ensure here that there is no "falltrough" to this section!
      let skipLabel = pool.syms.getOrIncl("`skip." & $c.nextLabel)
      inc c.nextLabel
      emitJump(c, dest, skipLabel, n.info)
      emitLabel(c, dest, targetLabel, n.info)
      trStmt c, dest, n # follow the then branch
      skip n # remove the else branch
      emitLabel(c, dest, skipLabel, n.info)
      dest.takeTree n # keep the join information
      skipParRi n
    of ElseBranch:
      inc n
      skip n
      # We need to ensure here that there is no "falltrough" to this section!
      let skipLabel = pool.syms.getOrIncl("`skip." & $c.nextLabel)
      inc c.nextLabel
      emitJump(c, dest, skipLabel, n.info)
      emitLabel(c, dest, targetLabel, n.info)
      skip n # remove the then branch
      trStmt c, dest, n # follow the else branch
      emitLabel(c, dest, skipLabel, n.info)
      dest.takeTree n # keep the join information
      skipParRi n
    of UnknownBranch:
      dest.takeToken n
      dest.takeTree n # condition is an expression so we don't have to traverse it
      trStmt c, dest, n
      trStmt c, dest, n
      dest.takeTree n # keep the join information
      dest.takeParRi n
  of CfvarV:
    inc n
    let s = getCfvar(n)
    # we know they start as false and inactive:
    c.state[s] = (false, 0)
    skipParRi n
  else:
    case n.kind
    of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StringLit, DotToken, EofToken, UnknownToken, Ident:
      dest.takeToken n
    of ParLe:
      dest.takeToken n
      while n.kind != ParRi:
        trStmt c, dest, n
      dest.takeToken n
    of ParRi:
      bug "Unmatched ParRi"


proc toGoto*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(typeCache: createTypeCache())
  var n = n
  c.typeCache.openScope()
  result = createTokenBuf(300)
  assert n.stmtKind == StmtsS, $n.kind
  result.add n

  aStmt c, n

  inc n
  while n.kind != ParRi:
    trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()
