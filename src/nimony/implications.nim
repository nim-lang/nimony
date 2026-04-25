#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Flow-sensitive init-implication tracking for contract analysis.
##
## A proc's body produces a list of :ref:`Implication` values that tell us,
## at the current program point, which symbols are definitely written and
## under which "cond" hypotheses.  Three shapes cover everything we need:
##
## - ``Always s``          — `s` is written on every path that reaches here.
## - ``IfTrue  cond s``    — `s` is written whenever `cond` evaluates true.
## - ``IfFalse cond s``    — `s` is written whenever `cond` evaluates false.
##
## A "cond" is identified by a ``SymId``.  Two flavours share this namespace:
## *cfvars* (mflags declared by the NJVL pass and set via ``jtrue``) and
## *synthetic cond syms* — fresh symbols the caller mints for complex ite
## conditions that aren't a cfvar (`(a.len == b.len)`, `(etupat …)` …).  A
## jtrue of a cfvar is just an ``Always cfvar`` fact contributed by the
## branch, which lets :proc:`combine` derive further implications via the
## asymmetric-jtrue rule for cfvars.
##
## Since both flavours live in the same namespace, transitive reasoning
## over conditions — ``IfTrue cond1 cond2`` + ``IfTrue cond2 s`` implies
## ``IfTrue cond1 s`` — flows across cfvars and synthetics uniformly.

import std / [sets, assertions]
include ".." / lib / nifprelude
import nimony_model

type
  ImplKind* = enum
    Always      ## `s` is written on every reaching path
    IfTrue      ## `s` is written whenever `cond` is true
    IfFalse     ## `s` is written whenever `cond` is false

  Implication* = object
    kind*: ImplKind
    cond*: SymId            ## `NoSymId` for ``Always``; otherwise either a
                            ## cfvar or a synthetic cond sym minted by the
                            ## caller for a complex ite condition.
    sym*: SymId

  PolarizedSym* = object
    ## Represents an ite's condition as a polarized cond sym.  The caller
    ## is expected to supply a non-``NoSymId`` `sym` for every real ite
    ## (minting a synthetic when the condition isn't a cfvar); passing
    ## ``NoSymId`` means "no recognizable cond" and disables cond-polarity
    ## lifting in :proc:`combine`.
    sym*: SymId
    negated*: bool            ## the ite's condition is ``(not sym)`` —
                              ## the lexical then-branch runs when `sym`
                              ## is false.

  Implications* = object
    items: seq[Implication]
    startIdx: int           ## queries and fold ignore items before this

  ImplScopeIdx* = int

proc createImplications*(): Implications =
  Implications(items: @[], startIdx: 0)

proc pushScope*(impls: var Implications): ImplScopeIdx =
  ## Start a new isolation scope (for e.g. nested proc bodies or loop
  ## bodies whose implications must not leak into the surrounding scope).
  result = impls.startIdx
  impls.startIdx = impls.items.len

proc popScope*(impls: var Implications; saved: ImplScopeIdx) =
  ## Discard everything added since the matching `pushScope`.
  impls.items.setLen(impls.startIdx)
  impls.startIdx = saved

proc checkpoint*(impls: Implications): int {.inline.} =
  impls.items.len

proc rollback*(impls: var Implications; cp: int) {.inline.} =
  impls.items.setLen(cp)

proc take*(impls: var Implications; cp: int): seq[Implication] =
  ## Extract and remove the items added since `cp`.  Intended for capturing
  ## per-branch implications before combining them at an ite join.
  result = @[]
  var i = cp
  while i < impls.items.len:
    result.add impls.items[i]
    inc i
  impls.items.setLen(cp)

proc add*(impls: var Implications; imp: Implication) =
  ## Append `imp`, de-duplicating and folding complementary conditionals
  ## (``IfTrue cf s`` ∧ ``IfFalse cf s`` become ``Always s``).
  for i in impls.startIdx ..< impls.items.len:
    if impls.items[i] == imp: return
  if imp.kind in {IfTrue, IfFalse}:
    let compKind = if imp.kind == IfTrue: IfFalse else: IfTrue
    for i in impls.startIdx ..< impls.items.len:
      let other = impls.items[i]
      if other.kind == compKind and other.cond == imp.cond and other.sym == imp.sym:
        impls.items[i] = Implication(kind: Always, cond: NoSymId, sym: imp.sym)
        return
  impls.items.add imp

proc always*(sym: SymId): Implication {.inline.} =
  Implication(kind: Always, cond: NoSymId, sym: sym)

proc ifTrue*(cond, sym: SymId): Implication {.inline.} =
  Implication(kind: IfTrue, cond: cond, sym: sym)

proc ifFalse*(cond, sym: SymId): Implication {.inline.} =
  Implication(kind: IfFalse, cond: cond, sym: sym)

# ---- queries --------------------------------------------------------------

iterator allItems*(impls: Implications): lent Implication =
  ## Iterate over all implications in the current scope (for debug / dumping).
  for i in impls.startIdx ..< impls.items.len:
    yield impls.items[i]

proc isAlwaysInit*(impls: Implications; sym: SymId): bool =
  result = false
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.kind == Always and imp.sym == sym: return true

proc isInitIfCondTrueImpl(impls: Implications; sym: SymId; cond: SymId;
                          visited: var HashSet[SymId]): bool =
  ## sym is init when `cond` is true — directly (``Always`` / ``IfTrue cond
  ## sym``) or via transitive chains (``IfTrue cond x`` + ``IfTrue x sym``).
  if cond in visited: return false
  visited.incl cond
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.sym == sym:
      if imp.kind == Always: return true
      if imp.kind == IfTrue and imp.cond == cond: return true
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.kind == IfTrue and imp.cond == cond:
      if isInitIfCondTrueImpl(impls, sym, imp.sym, visited): return true
  return false

proc isInitIfCondFalseImpl(impls: Implications; sym: SymId; cond: SymId;
                           visited: var HashSet[SymId]): bool =
  ## sym is init when `cond` is false — directly or via transitive
  ## ``IfFalse cond x`` + ``IfFalse x sym`` chains, or any ``Always`` fact.
  if cond in visited: return false
  visited.incl cond
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.sym == sym:
      if imp.kind == Always: return true
      if imp.kind == IfFalse and imp.cond == cond: return true
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.kind == IfFalse and imp.cond == cond:
      if isInitIfCondFalseImpl(impls, sym, imp.sym, visited): return true
  return false

proc isInitIfCondTrue*(impls: Implications; sym, cond: SymId): bool =
  var visited = initHashSet[SymId]()
  isInitIfCondTrueImpl(impls, sym, cond, visited)

proc isInitIfCondFalse*(impls: Implications; sym, cond: SymId): bool =
  var visited = initHashSet[SymId]()
  isInitIfCondFalseImpl(impls, sym, cond, visited)

# ---- combine --------------------------------------------------------------

proc alwaysSyms(impls: openArray[Implication];
                knownFalse: HashSet[SymId] = initHashSet[SymId]()): HashSet[SymId] =
  ## Syms the branch's impls unconditionally initialize, taking ambient
  ## `knownFalse` cfvars into account: an ``IfFalse cf s`` fact is effectively
  ## ``Always s`` in a scope where `cf` is known false.
  result = initHashSet[SymId]()
  for imp in impls:
    case imp.kind
    of Always: result.incl imp.sym
    of IfFalse:
      if imp.cond in knownFalse: result.incl imp.sym
    of IfTrue: discard

proc liftAsymmetric(impls: var Implications;
                    cf: SymId;
                    sameAlways, otherAlways: HashSet[SymId];
                    sameImpls, otherImpls: openArray[Implication]) =
  ## `cf` was jtrue'd Always in one branch ("same") and not touched in the
  ## other ("other"). After the ite, `cf=true` ⟺ same ran, `cf=false` ⟺
  ## other ran, so each branch's Always facts survive under the matching
  ## cfvar condition. Conditional facts that agree with this polarity
  ## survive unchanged; complementary ones are vacuous and dropped.
  for s in sameAlways:
    if s != cf and s notin otherAlways:
      impls.add Implication(kind: IfTrue, cond: cf, sym: s)
  for s in otherAlways:
    if s != cf and s notin sameAlways:
      impls.add Implication(kind: IfFalse, cond: cf, sym: s)
  for imp in sameImpls:
    if imp.kind == IfTrue and imp.cond == cf: impls.add imp
  for imp in otherImpls:
    if imp.kind == IfFalse and imp.cond == cf: impls.add imp

proc combine*(impls: var Implications;
              nowKnownFalse, nowKnownTrue: var seq[SymId];
              thenImpls, elseImpls: openArray[Implication];
              cond: PolarizedSym;
              knownCfVars: HashSet[SymId];
              knownFalseConds: HashSet[SymId] = initHashSet[SymId]()) =
  ## Merge the implications produced by an ite's two branches into `impls`.
  ##
  ## The condition is expected to be a concrete cond sym (cfvar or synthetic
  ## minted by the caller); a ``NoSymId`` `cond.sym` simply disables
  ## cond-polarity lifting. `knownCfVars` lists the syms that actually
  ## represent cfvars (the subset of conds that can be ``jtrue``'d);
  ## synthetic cond syms participate only through cond-polarity lifting.
  let thenAlways = alwaysSyms(thenImpls, knownFalseConds)
  let elseAlways = alwaysSyms(elseImpls, knownFalseConds)

  # (1) Always on both paths survives unconditionally.
  for s in thenAlways:
    if s in elseAlways: impls.add always(s)

  # (2) Lift branch-exclusive Always facts by the condition's polarity.
  if cond.sym != NoSymId:
    let (thenKind, elseKind) =
      if cond.negated: (IfFalse, IfTrue)
      else:            (IfTrue,  IfFalse)
    for s in thenAlways:
      if s notin elseAlways:
        impls.add Implication(kind: thenKind, cond: cond.sym, sym: s)
    for s in elseAlways:
      if s notin thenAlways:
        impls.add Implication(kind: elseKind, cond: cond.sym, sym: s)

  # (3) Asymmetric-jtrue reasoning: a cfvar Always-set in exactly one branch
  #     tells us after the ite which branch ran based on that cfvar's value.
  #     We also note whether either branch leaves via any cfvar (step 5).
  var thenLeaves = false
  var elseLeaves = false
  for s in thenAlways:
    if s in knownCfVars:
      thenLeaves = true
      if s notin elseAlways:
        liftAsymmetric(impls, s, thenAlways, elseAlways, thenImpls, elseImpls)
  for s in elseAlways:
    if s in knownCfVars:
      elseLeaves = true
      if s notin thenAlways:
        liftAsymmetric(impls, s, elseAlways, thenAlways, elseImpls, thenImpls)

  # (4) Propagate conditional facts that appear in **both** branches. A
  #     conditional that only one branch produces is not valid in the outer
  #     scope (we can't encode "… AND that branch ran" without conjunctions),
  #     so we drop it. Intersection is sound.
  for imp in thenImpls:
    if imp.kind == Always: continue
    for other in elseImpls:
      if imp == other:
        impls.add imp
        break

  # (5) Leaving-path promotion: if a branch unconditionally `jtrue`s a
  #     cfvar, sequential code past the ite is only reachable via the other
  #     branch, so *that* branch's Always facts also hold as Always outside
  #     the ite. We also register the leaving cfvars and the ite's cond
  #     itself (with its correct polarity) as now-known-false / -true so
  #     the caller can extend its ambient cfvar state.
  if thenLeaves and not elseLeaves:
    for s in elseAlways:
      if s notin thenAlways: impls.add always(s)
    for s in thenAlways:
      if s in knownCfVars: nowKnownFalse.add s
    # Sequential ran else-branch ⟹ actual cond was false.
    if cond.sym != NoSymId:
      if cond.negated: nowKnownTrue.add cond.sym
      else:            nowKnownFalse.add cond.sym
  elif elseLeaves and not thenLeaves:
    for s in thenAlways:
      if s notin elseAlways: impls.add always(s)
    for s in elseAlways:
      if s in knownCfVars: nowKnownFalse.add s
    # Sequential ran then-branch ⟹ actual cond was true.
    if cond.sym != NoSymId:
      if cond.negated: nowKnownFalse.add cond.sym
      else:            nowKnownTrue.add cond.sym
  elif thenLeaves and elseLeaves:
    # Both branches leave. Every leaving path `jtrue`'s a cfvar that guards
    # all post-ite sequential code (`(ite (not cf) …)` wraps). So *any*
    # code that reads a sym here is inside a guard proving one of these
    # cfvars false — but all of them are true on every branch that got us
    # here, so no such code is reachable. Claims therefore hold vacuously;
    # we keep each branch's Always facts for consistency.
    for s in thenAlways:
      if s notin elseAlways: impls.add always(s)
    for s in elseAlways:
      if s notin thenAlways: impls.add always(s)
