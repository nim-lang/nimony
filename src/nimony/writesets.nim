#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## WriteSets: Compact TokenBuf-based storage for per-ite write-set implications.
## Replaces ``seq[Implication]`` in NjvlContext, avoiding per-ite seq allocations.
##
## Each implication record is encoded in a ``TokenBuf`` history as:
##   ``(stmts guardOrDot (stmts thenSym1 ...) (stmts elseSym1 ...) )``
## where ``guardOrDot`` is a Symbol for the guard cfvar, or '.' if none.
##
## Records are appended sequentially; nested procs are isolated via pushScope/popScope.

import std / [sets, assertions]
include ".." / lib / nifprelude
import nimony_model

type
  WriteSets* = object
    history: TokenBuf
    startIdx: int  ## scan queries start here; set by pushScope to isolate procs

proc createWriteSets*(): WriteSets =
  WriteSets(history: createTokenBuf(50), startIdx: 0)

type WriteSetsScopeIdx* = int

proc pushScope*(ws: var WriteSets): WriteSetsScopeIdx =
  ## Save current scope boundary. Call at proc entry so the inner proc's records
  ## are isolated from the outer proc's.
  result = ws.startIdx
  ws.startIdx = ws.history.len

proc popScope*(ws: var WriteSets; saved: WriteSetsScopeIdx) =
  ## Discard all records added since pushScope and restore the outer scope.
  ws.history.shrink(ws.startIdx)
  ws.startIdx = saved

proc openRecord*(ws: var WriteSets; guard: SymId) =
  ## Begin an implication record. Call after both branches have been traversed
  ## and before emitting then-items.
  ## Emits: ``(stmts guardOrDot (stmts``
  ws.history.addParLe StmtsS, NoLineInfo
  if guard == NoSymId:
    ws.history.addDotToken()
  else:
    ws.history.addSymUse guard, NoLineInfo
  ws.history.addParLe StmtsS, NoLineInfo  # open then-section

proc emitThen*(ws: var WriteSets; sym: SymId) {.inline.} =
  ## Record a symbol written in the then-branch.
  ws.history.addSymUse sym, NoLineInfo

proc separateRecord*(ws: var WriteSets) =
  ## Close the then-section and open the else-section.
  ## Emits: ``) (stmts``
  ws.history.addParRi()  # close then-section
  ws.history.addParLe StmtsS, NoLineInfo  # open else-section

proc emitElse*(ws: var WriteSets; sym: SymId) {.inline.} =
  ## Record a symbol written in the else-branch.
  ws.history.addSymUse sym, NoLineInfo

proc closeRecord*(ws: var WriteSets) =
  ## Finalize the implication record.
  ## Emits: ``))``
  ws.history.addParRi()  # close else-section
  ws.history.addParRi()  # close outer record

proc impliedWhenFalse*(ws: WriteSets; cf: SymId; knownCfVars: HashSet[SymId]): HashSet[SymId] =
  ## Returns the set of syms known initialized when cfvar ``cf`` is false.
  ## Logic per record:
  ##   - ``guard == cf`` or ``cf`` in else-set  →  then-set vars are safe
  ##   - ``cf`` in then-set                     →  else-set vars are safe
  result = initHashSet[SymId]()
  var i = ws.startIdx
  while i < ws.history.len:
    assert ws.history[i].kind == ParLe  # outer record open
    inc i
    # Read guard
    var guard = NoSymId
    if ws.history[i].kind == Symbol:
      guard = ws.history[i].symId
    inc i  # skip guard or dot
    # Scan then-section: locate boundaries and check membership
    assert ws.history[i].kind == ParLe  # then-section open
    inc i
    let thenStart = i
    var cfInThen = false
    while ws.history[i].kind == Symbol:
      if ws.history[i].symId == cf: cfInThen = true
      inc i
    let thenEnd = i
    assert ws.history[i].kind == ParRi  # then-section close
    inc i
    # Scan else-section: locate boundaries and check membership
    assert ws.history[i].kind == ParLe  # else-section open
    inc i
    let elseStart = i
    var cfInElse = false
    while ws.history[i].kind == Symbol:
      if ws.history[i].symId == cf: cfInElse = true
      inc i
    let elseEnd = i
    assert ws.history[i].kind == ParRi; inc i  # else-section close
    assert ws.history[i].kind == ParRi; inc i  # outer record close
    # Apply implication logic
    if guard == cf or cfInElse:
      for j in thenStart ..< thenEnd:
        let s = ws.history[j].symId
        if s notin knownCfVars: result.incl s
    if cfInThen:
      for j in elseStart ..< elseEnd:
        let s = ws.history[j].symId
        if s notin knownCfVars: result.incl s

proc impliedWhenTrue*(ws: WriteSets; cf: SymId; knownCfVars: HashSet[SymId]): HashSet[SymId] =
  ## Returns the set of syms known initialized when cfvar ``cf`` is true.
  ## Logic per record:
  ##   - ``cf`` in then-set → other then-set vars were written in the same branch
  ##   - ``cf`` in else-set → other else-set vars were written in the same branch
  ##   - ``guard == cf``    → condition is ``(not cf)``, cf true means else-branch ran
  result = initHashSet[SymId]()
  var i = ws.startIdx
  while i < ws.history.len:
    assert ws.history[i].kind == ParLe
    inc i
    var guard = NoSymId
    if ws.history[i].kind == Symbol:
      guard = ws.history[i].symId
    inc i
    assert ws.history[i].kind == ParLe
    inc i
    let thenStart = i
    var cfInThen = false
    while ws.history[i].kind == Symbol:
      if ws.history[i].symId == cf: cfInThen = true
      inc i
    let thenEnd = i
    assert ws.history[i].kind == ParRi; inc i
    assert ws.history[i].kind == ParLe; inc i
    let elseStart = i
    var cfInElse = false
    while ws.history[i].kind == Symbol:
      if ws.history[i].symId == cf: cfInElse = true
      inc i
    let elseEnd = i
    assert ws.history[i].kind == ParRi; inc i
    assert ws.history[i].kind == ParRi; inc i
    # When cf is true: cf was jtrue'd, so the branch containing cf ran.
    # All non-cfvar syms in that same branch were also written.
    if cfInThen:
      for j in thenStart ..< thenEnd:
        let s = ws.history[j].symId
        if s notin knownCfVars: result.incl s
    if cfInElse or guard == cf:
      for j in elseStart ..< elseEnd:
        let s = ws.history[j].symId
        if s notin knownCfVars: result.incl s
