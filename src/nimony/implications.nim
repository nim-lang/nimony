#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Flow-sensitive init-implication tracking for contract analysis.
##
## A proc's body produces a list of :ref:`Implication` values that tell us,
## at the current program point, which symbols are definitely written and
## under which cfvar hypotheses.  Three shapes cover everything we need:
##
## - ``Always s``          — `s` is written on every path that reaches here.
## - ``IfTrue  cf s``      — `s` is written whenever cfvar `cf` is true.
## - ``IfFalse cf s``      — `s` is written whenever cfvar `cf` is false.
##
## Conditional implications are produced by :proc:`combine` at each ite join
## from the per-branch implications; the caller does not need to distinguish
## regular variable writes from cfvars activated via ``jtrue`` — a jtrue is
## just an ``Always cfvar`` contribution to the relevant branch.

import std / [sets, assertions]
include ".." / lib / nifprelude
import nimony_model

type
  ImplKind* = enum
    Always      ## `s` is written on every reaching path
    IfTrue      ## `s` is written whenever `cfvar` is true
    IfFalse     ## `s` is written whenever `cfvar` is false

  Implication* = object
    kind*: ImplKind
    cfvar*: SymId           ## `NoSymId` for ``Always``
    sym*: SymId

  PolarizedSym* = object
    ## Classifies the shape of an ite's condition.
    ## ``sym == NoSymId`` — the condition is not a cfvar expression.
    ## ``negated == true`` — the condition has the shape ``(not cf)``, so
    ## the ite's lexical then-branch runs when ``cf`` is false.
    sym*: SymId
    negated*: bool

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
  result = impls.items[cp ..< impls.items.len]
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
      if other.kind == compKind and other.cfvar == imp.cfvar and other.sym == imp.sym:
        impls.items[i] = Implication(kind: Always, cfvar: NoSymId, sym: imp.sym)
        return
  impls.items.add imp

proc always*(sym: SymId): Implication {.inline.} =
  Implication(kind: Always, cfvar: NoSymId, sym: sym)

proc ifTrue*(cfvar, sym: SymId): Implication {.inline.} =
  Implication(kind: IfTrue, cfvar: cfvar, sym: sym)

proc ifFalse*(cfvar, sym: SymId): Implication {.inline.} =
  Implication(kind: IfFalse, cfvar: cfvar, sym: sym)

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

proc isInitIfCfTrueImpl(impls: Implications; sym: SymId; cf: SymId;
                       visited: var HashSet[SymId]): bool =
  ## sym is init when cf is true — directly or via transitive `IfTrue`
  ## implications between cfvars (e.g. ``IfTrue cf x`` + ``IfTrue x sym``).
  if cf in visited: return false
  visited.incl cf
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.sym == sym:
      if imp.kind == Always: return true
      if imp.kind == IfTrue and imp.cfvar == cf: return true
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.kind == IfTrue and imp.cfvar == cf:
      if isInitIfCfTrueImpl(impls, sym, imp.sym, visited): return true
  return false

proc isInitIfCfFalseImpl(impls: Implications; sym: SymId; cf: SymId;
                        visited: var HashSet[SymId]): bool =
  ## sym is init when cf is false — directly or via transitive implications
  ## (``IfFalse cf x`` + ``IfFalse x sym``, or any `Always` fact).
  if cf in visited: return false
  visited.incl cf
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.sym == sym:
      if imp.kind == Always: return true
      if imp.kind == IfFalse and imp.cfvar == cf: return true
  for i in impls.startIdx ..< impls.items.len:
    let imp = impls.items[i]
    if imp.kind == IfFalse and imp.cfvar == cf:
      if isInitIfCfFalseImpl(impls, sym, imp.sym, visited): return true
  return false

proc isInitIfCfTrue*(impls: Implications; sym: SymId; cf: SymId): bool =
  var visited = initHashSet[SymId]()
  isInitIfCfTrueImpl(impls, sym, cf, visited)

proc isInitIfCfFalse*(impls: Implications; sym: SymId; cf: SymId): bool =
  var visited = initHashSet[SymId]()
  isInitIfCfFalseImpl(impls, sym, cf, visited)

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
      if imp.cfvar in knownFalse: result.incl imp.sym
    of IfTrue: discard

proc liftAsymmetric(impls: var Implications;
                    cf: SymId;
                    sameAlways, otherAlways, inBoth: HashSet[SymId];
                    sameImpls, otherImpls: openArray[Implication]) =
  ## `cf` was jtrue'd Always in one branch ("same") and not touched in the
  ## other ("other"). After the ite, `cf=true` ⟺ same ran, `cf=false` ⟺
  ## other ran, so each branch's Always facts survive under the matching
  ## cfvar condition. Conditional facts that agree with this polarity
  ## survive unchanged; complementary ones are vacuous and dropped.
  for s in sameAlways - inBoth:
    if s != cf:
      impls.add Implication(kind: IfTrue, cfvar: cf, sym: s)
  for s in otherAlways - inBoth:
    if s != cf:
      impls.add Implication(kind: IfFalse, cfvar: cf, sym: s)
  for imp in sameImpls:
    if imp.kind == IfTrue and imp.cfvar == cf: impls.add imp
  for imp in otherImpls:
    if imp.kind == IfFalse and imp.cfvar == cf: impls.add imp

proc combine*(impls: var Implications;
              nowKnownFalse: var seq[SymId];
              thenImpls, elseImpls: openArray[Implication];
              cond: PolarizedSym;
              knownCfVars: HashSet[SymId];
              knownFalseCfVars: HashSet[SymId] = initHashSet[SymId]()) =
  ## Merge the implications produced by an ite's two branches into `impls`,
  ## using the condition's polarity to lift branch-local ``Always`` facts
  ## into conditionals, and any cfvars jtrue'd on only one side to derive
  ## further conditionals via monotonicity.
  let thenAlways = alwaysSyms(thenImpls, knownFalseCfVars)
  let elseAlways = alwaysSyms(elseImpls, knownFalseCfVars)
  let inBoth = thenAlways * elseAlways

  # Always on both paths survives unconditionally.
  for s in inBoth: impls.add always(s)

  # Lift branch-exclusive Always facts by the condition's polarity.
  if cond.sym != NoSymId:
    let (thenKind, elseKind) =
      if cond.negated: (IfFalse, IfTrue)
      else:            (IfTrue,  IfFalse)
    for s in thenAlways - inBoth:
      impls.add Implication(kind: thenKind, cfvar: cond.sym, sym: s)
    for s in elseAlways - inBoth:
      impls.add Implication(kind: elseKind, cfvar: cond.sym, sym: s)

  # Asymmetric-jtrue reasoning: a cfvar Always-set in exactly one branch
  # tells us after the ite which branch ran based on the cfvar's value.
  let thenCfs = thenAlways * knownCfVars
  let elseCfs = elseAlways * knownCfVars
  for cf in thenCfs - elseCfs:
    liftAsymmetric(impls, cf, thenAlways, elseAlways, inBoth, thenImpls, elseImpls)
  for cf in elseCfs - thenCfs:
    liftAsymmetric(impls, cf, elseAlways, thenAlways, inBoth, elseImpls, thenImpls)

  # Propagate branch-local conditional facts through the combine. Strictly
  # these are only valid under the additional premise that the originating
  # branch ran — a premise our single-cfvar implications cannot encode.
  # Passing them through is pragmatic: queries like `isInitializedAtProcEnd`
  # combine them with the ambient cfvar state to conclude initialization
  # (mirrors the old writesets "impliedByIte" leniency).
  for imp in thenImpls:
    if imp.kind != Always: impls.add imp
  for imp in elseImpls:
    if imp.kind != Always: impls.add imp

  # When the condition isn't a cfvar, branch-exclusive Always facts
  # ordinarily drop out of the outer scope. However, if a branch Always
  # jtrue's a cfvar (leaving-path behaviour), sequential code after the
  # ite is only reachable via the other branch, so *that* branch's
  # Always facts hold. We also register the leaving cfvar(s) as
  # "now-known-false" so the caller can push them to its falseCfvars
  # set for the rest of the enclosing scope.
  if cond.sym == NoSymId:
    let thenCfs = thenAlways * knownCfVars
    let elseCfs = elseAlways * knownCfVars
    let thenLeaves = thenCfs.len > 0
    let elseLeaves = elseCfs.len > 0
    if thenLeaves and not elseLeaves:
      for s in elseAlways - inBoth: impls.add always(s)
      for cf in thenCfs: nowKnownFalse.add cf
    elif elseLeaves and not thenLeaves:
      for s in thenAlways - inBoth: impls.add always(s)
      for cf in elseCfs: nowKnownFalse.add cf
    elif thenLeaves and elseLeaves:
      # Both branches leave — no sequential successor. Still, to avoid
      # spuriously losing writes the compiler relies on, keep each
      # branch's Always facts as-is (unsound in general; sound when any
      # post-ite code is anyway guarded by the cfvars they jtrue'd).
      for s in thenAlways - inBoth: impls.add always(s)
      for s in elseAlways - inBoth: impls.add always(s)
