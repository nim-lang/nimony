#
#
#           Nimony Goto Insertion Pass
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Undoes the effects of NJVL by turning `(jtrue X)` statements into goto statements, if possible.

## **THIS IS CURRENTLY UNUSED AND UNTESTED AND UNFINISHED!**

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
## What this prototype does
## ------------------------
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
##
## Why this does not work yet (without cfvar versioning and kill)
## --------------------------------------------------------------
## The above model ignores versioning for cfvars and does not end their
## lifetimes explicitly. NJVL’s data side (VL) relies on versioned locations and
## `kill` to capture precise lifetimes and joins. Cfvars are also subject to the
## same scoping/merging issues:
##
## 1) Missing cfvar versioning at joins
##    - At `ite` joins (and loop headers), values come from multiple control
##      predecessors. For ordinary variables, NJVL uses `join`/`either` and
##      versions `(v x +k)`. This prototype keeps a single global `state` entry
##      per cfvar symbol, so truth assignments leak across branches or get
##      observed in regions where they no longer dominate.
##    - Consequence: we may incorrectly decide a `jtrue` can jump (or must
##      materialize) because we do not know which version of the cfvar guards the
##      current region.
##
## 2) Missing cfvar `kill` (end-of-lifetime) markers
##    - The `activeCount` heuristic approximates guard scopes, but there is no
##      explicit lifetime end for a cfvar version. After leaving a guarded
##      region, the “true” fact for a cfvar version should die; otherwise later
##      effects may be treated as if still guarded or, conversely, earlier facts
##      may prevent turning later `jtrue` occurrences into jumps.
##    - With explicit `kill` (on versioned cfvars), we could end the guarding
##      version precisely at scope exit, after last use, or at joins. This also
##      enables earlier freeing of the logical guard, mirroring how `kill` helps
##      register-pressure on data values.
##
## 3) Loops and back-edges
##    - Loop headers require `either` or at least explicit `join` semantics for
##      cfvars: a cfvar visible at the header is either the initial version or a
##      back-edge version. Without versioning, the prototype cannot distinguish
##      these, so guardedness can bleed from a previous iteration into the next
##      or into the `after` section.
##
## 4) Monotonic truth without scoping
##    - Cfvars are monotonic (`false` -> `true`), which is sound only when
##      combined with versioning and scoped lifetimes: each region/branch works
##      with its own version; once that version is `true` and used, it should be
##      killed when leaving the region. Without that, a single mutable truth flag
##      misrepresents dominance and makes the jump-conversion predicate unstable.
##
## 5) Effects and dominance
##    - The spec’s caveat “no interim statements” is about dominance: the jump
##      introduced for `(jtrue cf)` must be immediately applicable. Without
##      cfvar versions and `kill`, we cannot robustly answer “is this `jtrue`
##      still the controlling event for the current point?” across nested `ite`s
##      and loops.
##
## Path to correctness
## -------------------
## - Treat cfvars like versioned locations in VL: create `(v cf +k)` at each
##   assignment (`jtrue`) or merge (`join`/`either`).
## - Insert `kill (v cf +k)` when the guard’s lifetime ends (end of the guarded
##   region, join point, or loop exit). The `activeCount` can then be derived
##   from structured traversal of these versioned lifetimes instead of being a
##   global counter.
## - Decide jump vs materialization per cfvar version, not per symbol. A version
##   can be turned into a jump only if there are no intervening statements
##   between its creation point and the target transfer, within that version’s
##   lifetime.
##
## Until versioning and `kill` for cfvars are implemented, this pass remains a
## prototype that can illustrate the idea but cannot be sound across joins,
## loops, or complex nesting.

import std / [tables, sets, assertions]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav]
import ".." / hexer / [mover]

import njvl_model

type
  JtrueInstr = object
    pos: int
    isJump: bool
  Context* = object
    typeCache: TypeCache
    state: Table[SymId, (bool, int)] # (value, active)
    mustMaterialize: HashSet[SymId]
    jtrueInstrs: Table[SymId, JtrueInstr]

  CfvarState = object
    s: SymId
    value: bool # we always know the value of the cfvar
                # (they start out as false and can only become
                # true and then stay true)

  CfvarMask = seq[CfvarState]

proc computeCfvarMask(c: var Context; n: var Cursor; mask: var CfvarMask) =
  case n.kind
  of Symbol:
    let s = n.symId
    if c.state.hasKey(s):
      mask.add CfvarState(s: s, value: c.state[s][0])
    inc n
  of ParLe:
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

proc finishTrace(c: var Context; s: SymId) =
  if c.jtrueInstrs.hasKey(s):
    c.jtrueInstrs[s].isJump = not c.mustMaterialize.contains(s)

proc aStmt(c: var Context; n: var Cursor) =
  case n.njvlKind
  of JtrueV:
    # we now know these symbols are true:
    inc n
    while n.kind != ParRi:
      assert n.kind == Symbol
      let s = n.symId
      finishTrace(c, s)
      c.state[s][0] = true
      inc n
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
    # skip join information
    skip n
    skipParRi n
  of CfvarV:
    inc n
    assert n.kind == SymbolDef
    # we know they start as false and inactive:
    c.state[n.symId] = (false, 0)
    inc n
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

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.njvlKind
  of JtrueV:
    let info = n.info
    # we now know these symbols are true:
    inc n
    var label = NoSymId
    while n.kind != ParRi:
      assert n.kind == Symbol
      let s = n.symId
      if c.mustMaterialize.contains(s):
        dest.copyIntoKind StoreV, info:
          dest.addParPair TrueX, info
          dest.addSymUse s, info
      else:
        # see `ret` construction, etc. the last label counts
        label = s
      inc n
    inc n
    if label != NoSymId:
      dest.add tagToken("jmp", info)
      dest.addSymUse label, info
      dest.addParRi()
  of CfvarV:
    inc n
    assert n.kind == SymbolDef
    # we know they start as false and inactive:
    c.state[n.symId] = (false, 0)
    inc n
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
