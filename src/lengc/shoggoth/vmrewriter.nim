#
#
#           NIF VM-based Rewrite Engine (NFA/DFA)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A `lexim`-style rewrite engine over NIF tokens. Rules are parsed at runtime
## (no macros) into pattern trees, then each root tag's rules are merged into a
## **DFA** (deterministic pushdown tree automaton, per `vmrewriter_design.md`)
## and run by a VM over a `nifcore` cursor.
##
## - The DFA is built per *scope* (a node's child sequence). Every transition
##   consumes exactly one child; `Open(tag)` descends into a recursively-built
##   child scope DFA (the pushdown). Subtree boundaries come from `cursorJump`/
##   `subtreeWidth`, so there is no close token to match.
## - Captures use TDFA-style per-(rule,slot) **registers**: each transition
##   writes the registers of the items that advanced; an accept reads the
##   winning rule's registers.
## - The dynamic predicates `(pure X)` and `(same X)` are runtime **guards** on
##   transition refinements (`isPureSubtree` / `subtreeEqual`).
## - Accept states carry an **ordered** list of rules; `WHEN` predicates run at
##   accept time and a failing rule falls through to the next candidate.
##
## Deferred: an `Open(tag)` that competes with a `Wild`/`Pure`/`Same` at the same
## position (descend-vs-skip) needs the full pushdown return-continuation; that
## is detected and raised as an error for now (it does not occur in the current
## rule set). Kleene/variadic patterns are future work.

import std / [assertions, tables, algorithm]
when defined(nimony):
  import std / syncio   # `quit` lives here under Nimony; host Nim has it in `system`
  {.feature: "lenientnils".}   # `Engine` is a `ref` the driver passes as nil, and
                               # an unregistered `PredProc` is a nil closure
import ".." / ".." / "lib" / [nifcoreparse, nifcdecl]  # nifcoreparse re-exports nifcore
import ".." / [nifmodules, typenav]
import countedloops
# For `getOrQuit`: Nimony's `Table.[]` raises `KeyError`, and every lookup
# below is already guarded by an `in` test or an `assert`. `getOrQuit` is the
# non-raising spelling of "the key is there"; the shim provides it under Nim.
include ".." / ".." / "lib" / compat2

proc fail(msg: string) {.noreturn.} =
  ## Every rejection below is a defect in the *rule file* or a pattern shape the
  ## engine does not implement — decided while compiling a `staticRead` rule set,
  ## i.e. the same for every run of the binary. Nothing has ever caught them: the
  ## `ValueError` these used to raise walked straight out of `main`, so the
  ## observable behaviour was already "print and die with a non-zero status".
  ## Spelling that as `quit` says so, and keeps the engine out of Nimony's
  ## `{.raises.}` model, which would otherwise go viral from `newEngine` through
  ## every caller for a diagnostic that never returns.
  quit "vmrewriter: " & msg

type
  RuleTok = enum
    ## The rule-DSL vocabulary. Registered contiguously in the tag pool so a
    ## cursor's tag maps to this enum by a single subtraction + cast (the
    ## nifcore fast-tag idiom). `rtOther` = any tag outside the vocabulary
    ## (i.e. a concrete pattern tag like `add`).
    rtRules, rtRule, rtIf, rtDo, rtWhen,
    rtAny, rtInt, rtSym, rtLit, rtPure, rtSame,
    rtOther

const
  RuleTokNames = ["rules", "rule", "IF", "DO", "WHEN",
                  "any", "int", "sym", "lit", "pure", "same"]
  MetaToks = {rtAny, rtInt, rtSym, rtLit, rtPure, rtSame}
  NullaryConstTags = ["true", "false", "nil", "inf", "neginf", "nan"]

proc tagNameOf(c: Cursor): string {.inline.} =
  c.tags.tagName(c.cursorTagId)

# ---- runtime helpers ------------------------------------------------------

proc isPureSubtree*(c: Cursor): bool =
  case c.kind
  of IntLit, UIntLit, FloatLit, CharLit, StrLit, Symbol, DotToken:
    true
  of TagLit:
    tagNameOf(c) in NullaryConstTags
  else:
    false

proc subtreeEqual*(a, b: Cursor): bool =
  ## Structural equality of two subtrees in the same pool/tag space.
  if a.kind != b.kind: return false
  if a.kind == TagLit:
    if cursorTagId(a) != cursorTagId(b): return false
    if cursorJump(a) != cursorJump(b): return false
    var ca = a
    var cb = b
    inc ca
    inc cb
    var rem = int cursorJump(a)
    while rem > 0:
      if not subtreeEqual(ca, cb): return false
      let w = subtreeWidth(ca)
      skip ca
      skip cb
      rem -= w
    return true
  else:
    return a.load == b.load

# ---- pattern representation -----------------------------------------------
#
# Compiled LHS patterns are stored as a `nifcore` tree (one `TokenBuf` per
# rule) rather than a parallel Nim AST — `nifcore`'s tagging scheme already is
# a flexible tagged tree, so we reuse it:
#
#   Open(tag)          -> the real `TagLit tag` with the compiled kids
#   (any X)/bare ident -> `(@wild   <reg>)`
#   (int X)            -> `(@anyint <reg>)`
#   (sym X)            -> `(@anysym <reg>)`
#   (lit X)            -> `(@anylit <reg>)`
#   (pure X)           -> `(@pure   <reg>)`
#   (same X) / repeat  -> `(@same   <reg>)`   (reg = the referenced capture)
#   literal / . / sym  -> the raw `IntLit` / `DotToken` / `Symbol` token
#
# The `<reg>` is a single `IntLit` child = the global capture register. A
# `PatKind` is *derived* from a cursor (see `patKindOf`); there is no parallel
# node type.

type
  PatKind = enum
    pkOpen, pkInt, pkDot, pkSymLit, pkAnyInt, pkAnySym, pkAnyLit, pkWild, pkPure, pkSame

  SideCond = object
    pred: TagId                  # the predicate tag, e.g. `isPow2`
    slots: seq[int]              # global registers

  Rule = object
    rootTag: TagId
    patBuf: TokenBuf             # compiled LHS, root = (rootTag <kids…>)
    rootKids: seq[Cursor]        # cursors into patBuf, one per root-scope child
    rhs: Cursor                  # template root (points into the rule buffer)
    capIndex: Table[SymId, int]  # capture name (interned) -> global register
    sideConds: seq[SideCond]

# ---- DFA ------------------------------------------------------------------

type
  DiscKind = enum
    diIntEq, diIntOther, diSymEq, diSymOther, diDot, diOtherLit, diTagEq, diTagOther
  Disc = object
    case kind: DiscKind
    of diIntEq: ival: int64
    of diSymEq: str: string
    of diTagEq: tag: TagId
    else: discard

  GuardKind = enum gkNone, gkPure, gkSame
  Guard = object
    kind: GuardKind
    reg: int                     # gkSame

  Refine = object
    guards: seq[Guard]           # all must pass; tried most-specific (largest) first
    writes: seq[int]             # registers to write with the current child
    descend: bool
    childDFA: int
    dest: int

  Bucket = object
    disc: Disc
    refines: seq[Refine]

  DState = object
    accepts: seq[int]            # rule ids, priority (ascending) order
    buckets: seq[Bucket]         # specific discs first, "other" discs last

  Dfa = object
    states: seq[DState]

  ScopeEntry = object
    ruleId: int
    siblings: seq[Cursor]        # cursors into a rule's patBuf, one per sibling

  PredProc* = proc (args: openArray[Cursor]): bool {.closure.}

  Engine* = ref object
    pool: Pool
    tags: TagPool
    ruleBuf: TokenBuf
    rules: seq[Rule]
    dfas: seq[Dfa]
    numRegs: int
    dispatch: Table[TagId, int]  # root tag -> dfa index
    preds: Table[TagId, PredProc]
    dslIds: array[rtRules..rtSame, TagId]  # DSL vocabulary tag ids (see ruleTok)
    stmtsTag, scopeTag: TagId    # statement lists: a member rewritten to `.` is dropped
    # meta-tag vocabulary for compiled patterns (in `tags`)
    metaWild, metaAnyInt, metaAnySym, metaAnyLit, metaPure, metaSame: TagId
    tagVar, tagParam: TagId      # NIFC local-var / parameter decl tags

proc ruleTok(e: Engine; c: Cursor): RuleTok =
  ## Map a cursor's tag to the DSL enum. An 11-entry scan instead of the old
  ## contiguous-id cast: with a shared tag pool (the production pipeline hands
  ## the module's Leng pool in, see `newEngine`) a DSL name that already exists
  ## as a Leng tag — `pure` is one — gets its pre-existing id, so the DSL ids
  ## are not contiguous. Only consulted on rule-file cursors (parse + DO
  ## templates), never in the hot matching path.
  if c.kind != TagLit: return rtOther
  let t = cursorTagId(c)
  for rt in rtRules..rtSame:
    if e.dslIds[rt] == t: return rt
  rtOther

# ---- compiling LHS patterns ----------------------------------------------

proc allocReg(e: Engine; rule: var Rule; name: string): int =
  let s = e.pool.symId(name)
  if s in rule.capIndex: return getOrQuit(rule.capIndex, s)
  result = e.numRegs
  inc e.numRegs
  rule.capIndex[s] = result

proc metaChildName(c: Cursor): string =
  var m = c
  m.into:
    assert m.kind == Ident, "meta expects one identifier"
    result = strVal(m)
    m.inc

proc emitMeta(pat: var TokenBuf; tag: TagId; reg: int) =
  pat.openTag tag
  pat.addIntLit reg
  pat.closeTag()

proc compilePat(e: Engine; rule: var Rule; c: var Cursor; pat: var TokenBuf) =
  ## Transcribe the LHS pattern at `c` into the nifcore buffer `pat`.
  case c.kind
  of TagLit:
    case ruleTok(e, c)
    of rtAny:  emitMeta(pat, e.metaWild,   allocReg(e, rule, metaChildName(c))); skip c
    of rtInt:  emitMeta(pat, e.metaAnyInt, allocReg(e, rule, metaChildName(c))); skip c
    of rtSym:  emitMeta(pat, e.metaAnySym, allocReg(e, rule, metaChildName(c))); skip c
    of rtLit:  emitMeta(pat, e.metaAnyLit, allocReg(e, rule, metaChildName(c))); skip c
    of rtPure: emitMeta(pat, e.metaPure,   allocReg(e, rule, metaChildName(c))); skip c
    of rtSame:
      let childName = metaChildName(c)
      let s = e.pool.symId(childName)
      assert s in rule.capIndex, "(same " & childName & "): not bound"
      emitMeta(pat, e.metaSame, getOrQuit(rule.capIndex, s))
      skip c
    else:                                  # concrete tag -> Open
      pat.openTag cursorTagId(c)
      c.into:
        while c.hasMore:
          compilePat(e, rule, c, pat)
      pat.closeTag()
  of Ident:
    let nm = strVal(c)
    let s = e.pool.symId(nm)
    if s in rule.capIndex: emitMeta(pat, e.metaSame, getOrQuit(rule.capIndex, s))
    else:                  emitMeta(pat, e.metaWild, allocReg(e, rule, nm))
    inc c
  of IntLit:
    pat.addIntLit intVal(c); inc c
  of DotToken:
    pat.addDotToken(); inc c
  of Symbol:
    pat.addSymUse symName(c); inc c
  else:
    fail "unsupported pattern token: " & $c.kind

# ---- reading a compiled pattern node off a cursor -------------------------

proc patKindOf(e: Engine; c: Cursor): PatKind =
  case c.kind
  of IntLit:   pkInt
  of DotToken: pkDot
  of Symbol:   pkSymLit
  of TagLit:
    let t = cursorTagId(c)
    if   t == e.metaWild:   pkWild
    elif t == e.metaAnyInt: pkAnyInt
    elif t == e.metaAnySym: pkAnySym
    elif t == e.metaAnyLit: pkAnyLit
    elif t == e.metaPure:   pkPure
    elif t == e.metaSame:   pkSame
    else:                   pkOpen
  else:
    fail "bad pattern node: " & $c.kind

proc patReg(c: Cursor): int =
  ## The register stored as the `IntLit` child of a meta node.
  var cc = c
  cc.into:
    result = int(intVal(cc))
    cc.inc

proc patCapReg(e: Engine; c: Cursor): int =
  ## Register written by a capturing node, or -1.
  case patKindOf(e, c)
  of pkWild, pkAnyInt, pkAnySym, pkAnyLit, pkPure: patReg(c)
  else: -1

proc childCursors(node: Cursor): seq[Cursor] =
  ## Cursors to each child of a (compiled) tag node, in order.
  result = @[]
  var c = node
  c.into:
    while c.hasMore:
      result.add c
      skip c

# ---- subset construction --------------------------------------------------

type Item = tuple[entry, idx: int]

proc cmpItem(x, y: Item): int =
  ## The canonical order on an item set. `buildScopeDFA` keys its state table on
  ## the *sorted* item seq, so this is what makes two orderings of the same set
  ## one state. Written out because Nimony's `algorithm.sort` has no
  ## `system.cmp` default overload — the comparator is always explicit there.
  ## `entry`/`idx` are small array indices, so the subtraction cannot overflow.
  result = x.entry - y.entry
  if result == 0: result = x.idx - y.idx

proc cmpRuleId(x, y: int): int = x - y
  ## Likewise for the accept list, whose elements are rule ids.

proc patMatchesDisc(e: Engine; p: Cursor; d: Disc):
    tuple[m, descend: bool; g: GuardKind; greg: int] =
  template no: untyped = (false, false, gkNone, 0)
  case patKindOf(e, p)
  of pkInt:
    if d.kind == diIntEq and d.ival == intVal(p): (true, false, gkNone, 0) else: no
  of pkAnyInt:
    if d.kind in {diIntEq, diIntOther}: (true, false, gkNone, 0) else: no
  of pkAnyLit:
    if d.kind in {diIntEq, diIntOther, diOtherLit}: (true, false, gkNone, 0) else: no
  of pkSymLit:
    if d.kind == diSymEq and d.str == symName(p): (true, false, gkNone, 0) else: no
  of pkAnySym:
    if d.kind in {diSymEq, diSymOther}: (true, false, gkNone, 0) else: no
  of pkDot:
    if d.kind == diDot: (true, false, gkNone, 0) else: no
  of pkOpen:
    if d.kind == diTagEq and d.tag == cursorTagId(p): (true, true, gkNone, 0) else: no
  of pkWild:
    (true, false, gkNone, 0)            # matches every discriminator
  of pkPure:
    case d.kind
    of diIntEq, diIntOther, diSymEq, diSymOther, diDot, diOtherLit:
      (true, false, gkNone, 0)          # atoms are statically pure
    of diTagEq:
      if e.tags.tagName(d.tag) in NullaryConstTags: (true, false, gkNone, 0) else: no
    of diTagOther:
      (true, false, gkPure, 0)          # runtime isPureSubtree
  of pkSame:
    (true, false, gkSame, patReg(p))    # runtime subtreeEqual

type ScopeBuild = object
  ## The three pieces of subset-construction state, in one object so `getState`
  ## can be a plain proc. It used to be a nested proc closing over three locals;
  ## Nimony wants such a capture spelled `{.closure.}`, and an explicit `var`
  ## parameter is both cheaper and clearer about what it mutates.
  dfa: Dfa
  ids: Table[seq[Item], int]     ## canonical (sorted) item set -> state id
  worklist: seq[seq[Item]]

proc getState(b: var ScopeBuild; items0: seq[Item]): int =
  ## The state for this item set, minting one on first sight. Sorting is what
  ## makes the set canonical, so two orderings map to the same state.
  var items = items0
  sort(items, cmpItem)
  result = b.ids.getOrDefault(items, -1)
  if result >= 0: return
  result = b.dfa.states.len
  b.ids[items] = result
  b.dfa.states.add DState()
  b.worklist.add items

proc buildScopeDFA(e: Engine; entries: seq[ScopeEntry]): int =
  var b = ScopeBuild(dfa: Dfa(), ids: initTable[seq[Item], int](), worklist: @[])

  var startItems: seq[Item] = @[]
  for i in 0 ..< entries.len: startItems.add (entry: i, idx: 0)
  discard getState(b, startItems)

  var wi = 0
  while wi < b.worklist.len:
    let items = b.worklist[wi]
    let sid = b.ids.getOrDefault(items, -1)
    assert sid >= 0, "worklist item without a state"
    var st = DState()

    # accepts (items past their last sibling) + collect live (non-end) items
    var live: seq[Item] = @[]
    for it in items:
      if it.idx == entries[it.entry].siblings.len:
        st.accepts.add entries[it.entry].ruleId
      else:
        live.add it
    sort(st.accepts, cmpRuleId)

    # gather discriminators present among live items' next patterns
    var intVals: seq[int64] = @[]
    var symStrs: seq[string] = @[]
    var openTags: seq[TagId] = @[]
    var hasAnyInt, hasAnySym, hasAnyLit, hasDot, hasWild, hasPure, hasSame = false
    for it in live:
      let p = entries[it.entry].siblings[it.idx]
      case patKindOf(e, p)
      of pkInt:    (let v = intVal(p); (if v notin intVals: intVals.add v))
      of pkSymLit: (let s = symName(p); (if s notin symStrs: symStrs.add s))
      of pkOpen:   (let t = cursorTagId(p); (if t notin openTags: openTags.add t))
      of pkAnyInt: hasAnyInt = true
      of pkAnySym: hasAnySym = true
      of pkAnyLit: hasAnyLit = true
      of pkDot:    hasDot = true
      of pkWild:   hasWild = true
      of pkPure:   hasPure = true
      of pkSame:   hasSame = true

    let general = hasWild or hasPure or hasSame
    var discs: seq[Disc] = @[]
    for v in intVals: discs.add Disc(kind: diIntEq, ival: v)
    for s in symStrs: discs.add Disc(kind: diSymEq, str: s)
    for t in openTags: discs.add Disc(kind: diTagEq, tag: t)
    if hasAnyInt or hasAnyLit or general: discs.add Disc(kind: diIntOther)
    if hasAnySym or general:              discs.add Disc(kind: diSymOther)
    if hasDot or general:                 discs.add Disc(kind: diDot)
    if hasAnyLit or general:              discs.add Disc(kind: diOtherLit)
    if general:                           discs.add Disc(kind: diTagOther)

    for d in discs:
      # matched live items under this discriminator
      var statics: seq[Item] = @[]           # guard gkNone, not descend
      var dyns: seq[tuple[it: Item; g: Guard]] = @[]
      var opens: seq[Item] = @[]
      for it in live:
        let p = entries[it.entry].siblings[it.idx]
        let r = patMatchesDisc(e, p, d)
        if not r.m: continue
        if r.descend: opens.add it
        elif r.g == gkNone: statics.add it
        else: dyns.add (it, Guard(kind: r.g, reg: r.greg))

      if opens.len > 0 and (statics.len > 0 or dyns.len > 0):
        fail "merge not supported: Open competes with Wild/Pure/Same at one position"

      var bucket = Bucket(disc: d)
      if opens.len > 0:
        if opens.len > 1:
          fail "merge not supported: multiple distinct Open patterns at one position"
        let it = opens[0]
        let p = entries[it.entry].siblings[it.idx]
        let childDFA = buildScopeDFA(e, @[ScopeEntry(ruleId: entries[it.entry].ruleId,
                                                     siblings: childCursors(p))])
        let dest = getState(b, @[(entry: it.entry, idx: it.idx + 1)])
        bucket.refines.add Refine(guards: @[], writes: @[], descend: true,
                                  childDFA: childDFA, dest: dest)
      else:
        # refines = subsets of dyns, largest first; empty subset last (default)
        let n = dyns.len
        for mask in countdown((1 shl n) - 1, 0):
          var adv = statics
          var guards: seq[Guard] = @[]
          for k in 0 ..< n:
            if (mask and (1 shl k)) != 0:
              adv.add dyns[k].it
              guards.add dyns[k].g
          var writes: seq[int] = @[]
          var destItems: seq[Item] = @[]
          for it in adv:
            let p = entries[it.entry].siblings[it.idx]
            let r = patCapReg(e, p)
            if r >= 0: writes.add r
            destItems.add (entry: it.entry, idx: it.idx + 1)
          let dest = getState(b, destItems)
          bucket.refines.add Refine(guards: guards, writes: writes,
                                    descend: false, childDFA: -1, dest: dest)
      st.buckets.add bucket

    b.dfa.states[sid] = st
    inc wi

  e.dfas.add ensureMove(b.dfa)
  result = e.dfas.len - 1

# ---- rule parsing ---------------------------------------------------------

proc parseRule(e: Engine; c: Cursor) =
  var rule = Rule(capIndex: initTable[SymId, int](),
                  patBuf: createTokenBuf(16, e.pool, e.tags))
  var compiled = false
  var haveRhs = false
  var rc = c
  rc.into:
    while rc.hasMore:
      assert rc.kind == TagLit, "rule body: expected (IF …)/(DO …)/(WHEN …)"
      case ruleTok(e, rc)
      of rtIf:
        var l = rc
        l.into:
          assert l.kind == TagLit and ruleTok(e, l) == rtOther,
            "IF root must be a concrete tag"
          rule.rootTag = cursorTagId(l)
          # `compilePat(e, rule, l, rule.patBuf)` would hand the same object in
          # twice as a mutable argument — legal in Nim, rejected by Nimony's
          # alias check, and genuinely ambiguous. Compile into a fresh buffer
          # and install it: `compilePat` only ever appends to the destination it
          # is handed, so nothing is lost by not starting from `rule.patBuf`.
          var pat = createTokenBuf(16, e.pool, e.tags)
          compilePat(e, rule, l, pat)           # emits (rootTag <kids…>)
          rule.patBuf = ensureMove(pat)
        compiled = true
        skip rc
      of rtDo:
        var r = rc
        r.into:
          rule.rhs = r
          skip r
        haveRhs = true
        skip rc
      of rtWhen:
        assert compiled, "(WHEN …) must follow (IF …)"
        var wc = rc
        wc.into:
          while wc.hasMore:
            var call = wc
            let predTag = cursorTagId(call)
            var slots: seq[int] = @[]
            call.into:
              while call.hasMore:
                assert call.kind == Ident, "(WHEN …) args must be captured names"
                let argName = strVal(call)
                let s = e.pool.symId(argName)
                assert s in rule.capIndex,
                  "(WHEN …) references unbound name: " & argName
                slots.add getOrQuit(rule.capIndex, s)
                call.inc
            rule.sideConds.add SideCond(pred: predTag, slots: slots)
            skip wc
        skip rc
      else:
        fail "unknown rule section: " & tagNameOf(rc)
  assert compiled, "rule missing (IF …)"
  assert haveRhs, "rule missing (DO …)"
  e.rules.add ensureMove(rule)

proc parseRules(e: Engine) =
  var c = e.ruleBuf.beginRead()
  assert c.kind == TagLit
  case ruleTok(e, c)
  of rtRules:
    c.into:
      while c.hasMore:
        if ruleTok(e, c) == rtRule: parseRule(e, c)
        skip c
  of rtRule:
    parseRule(e, c)
  else:
    fail "rule file: expected (rules …) or (rule …)"

proc finalizeRules(e: Engine) =
  ## Take cursors into each rule's (now-immutable) patBuf. Done after all rules
  ## are parsed so no later append can invalidate the cursors.
  for i in 0 ..< e.rules.len:
    var root = e.rules[i].patBuf.beginRead()
    e.rules[i].rootKids = childCursors(root)

proc buildDispatch(e: Engine) =
  ## Group rules by root tag, then build one merged scope DFA per group.
  var groups = initOrderedTable[TagId, seq[int]]()
  for i in 0 ..< e.rules.len:
    groups.mgetOrPut(e.rules[i].rootTag, @[]).add i
  for tag, ruleIdxs in groups:
    var entries: seq[ScopeEntry] = @[]
    for ri in ruleIdxs:
      entries.add ScopeEntry(ruleId: ri, siblings: e.rules[ri].rootKids)
    e.dispatch[tag] = buildScopeDFA(e, entries)

proc isIntTPred(args: openArray[Cursor]): bool =
  ## The built-in `(WHEN (isIntT T))` guard: T is a SIGNED integer type `(i N)`.
  ## Used by the `x - x → 0` rule — a float `x - x` is not `0.0` under IEEE
  ## (NaN/Inf), and the integer `0` template would be ill-typed for `(f N)`/
  ## `(u N)` operands.
  let c = args[0]
  c.kind == TagLit and c.tags.tagName(c.cursorTagId) == "i"

type LitOrder = enum
  loUnknown, loLess, loEqual, loGreater

proc litOrder(a0, b0: Cursor): LitOrder =
  ## How two literal operands of a comparison order. The literal kind carries
  ## the comparison's signedness; a `(suf 14 "i64")` is its literal (both
  ## operands have the same type); `false < true`, `nil == nil`. Floats stay unknown — NaN is no literal,
  ## but there is nothing to gain from deciding them either.
  template peel(c: Cursor): Cursor =
    (if c.kind == TagLit and c.tags.tagName(c.cursorTagId) == "suf": c.childCursor else: c)
  template order(x, y: untyped): LitOrder =
    (if x < y: loLess elif x == y: loEqual else: loGreater)
  proc nullary(c: Cursor): int =
    ## `(nil)`, `(false)`, `(true)` as 0, 1, 2; anything else -1.
    if c.kind != TagLit or c.childCursor.hasMore: return -1
    case c.tags.tagName(c.cursorTagId)
    of "nil": 0
    of "false": 1
    of "true": 2
    else: -1
  let a = peel(a0)
  let b = peel(b0)
  if a.kind != b.kind: return loUnknown
  case a.kind
  of IntLit: order(intVal(a), intVal(b))
  of UIntLit: order(uintVal(a), uintVal(b))
  of CharLit: order(charLit(a), charLit(b))
  of TagLit:
    let x = nullary(a)
    let y = nullary(b)
    # nil is not ordered against a bool: such an operand pair is ill-typed
    if x < 0 or y < 0 or (x == 0) != (y == 0): loUnknown
    else: order(x, y)
  else: loUnknown

proc litLtPred(args: openArray[Cursor]): bool =
  ## `(WHEN (litLt X Y))`: X and Y are literals and X < Y.
  litOrder(args[0], args[1]) == loLess

proc litLePred(args: openArray[Cursor]): bool =
  litOrder(args[0], args[1]) in {loLess, loEqual}

proc litEqPred(args: openArray[Cursor]): bool =
  litOrder(args[0], args[1]) == loEqual

proc litNeqPred(args: openArray[Cursor]): bool =
  litOrder(args[0], args[1]) in {loLess, loGreater}

proc newEngine*(rulesSrc: string; pool: Pool = nil; tags: TagPool = nil): Engine =
  ## Build an engine from a rules source. Pass the module's `pool`/`tags` (e.g.
  ## the typenav context's, as `optdriver` does) so the compiled patterns' tag
  ## ids line up with the buffers the engine will rewrite; with no pool given
  ## (tests, fuzzing) the engine owns a private one and inputs go through
  ## `parseInput`.
  result = Engine(pool: (if pool == nil: newPool() else: pool),
                  tags: (if tags == nil: newTagPool() else: tags),
                  dispatch: initTable[TagId, int](),
                  preds: initTable[TagId, PredProc]())
  result.metaWild   = result.tags.registerTag("@wild")
  result.stmtsTag   = result.tags.registerTag("stmts")
  result.scopeTag   = result.tags.registerTag("scope")
  result.metaAnyInt = result.tags.registerTag("@anyint")
  result.metaAnySym = result.tags.registerTag("@anysym")
  result.metaAnyLit = result.tags.registerTag("@anylit")
  result.metaPure   = result.tags.registerTag("@pure")
  result.metaSame   = result.tags.registerTag("@same")
  for rt in rtRules..rtSame:
    result.dslIds[rt] = result.tags.registerTag(RuleTokNames[ord(rt)])
  result.tagVar   = result.tags.registerTag("var")
  result.tagParam = result.tags.registerTag("param")
  result.preds[result.tags.registerTag("isIntT")] = isIntTPred   # built-in guard
  result.preds[result.tags.registerTag("litLt")] = litLtPred
  result.preds[result.tags.registerTag("litLe")] = litLePred
  result.preds[result.tags.registerTag("litEq")] = litEqPred
  result.preds[result.tags.registerTag("litNeq")] = litNeqPred
  result.ruleBuf = parseFromBuffer(rulesSrc, "R", 256, result.pool, result.tags)
  parseRules(result)
  finalizeRules(result)
  buildDispatch(result)

proc registerPred*(e: Engine; name: string; p: PredProc) =
  e.preds[e.tags.registerTag(name)] = p

proc parseInput*(e: Engine; src: string; thisModule = "M"): TokenBuf =
  parseFromBuffer(src, thisModule, 100, e.pool, e.tags)

# ---- the matching VM ------------------------------------------------------

proc discMatchesChild(d: Disc; c: Cursor): bool =
  case d.kind
  of diIntEq:    c.kind == IntLit and intVal(c) == d.ival
  of diIntOther: c.kind == IntLit
  of diSymEq:    c.kind == Symbol and symName(c) == d.str
  of diSymOther: c.kind == Symbol
  of diDot:      c.kind == DotToken
  of diOtherLit: c.kind in {UIntLit, FloatLit, CharLit, StrLit}
  of diTagEq:    c.kind == TagLit and cursorTagId(c) == d.tag
  of diTagOther: c.kind == TagLit

proc guardsPass(guards: seq[Guard]; c: Cursor; caps: seq[Cursor]): bool =
  for g in guards:
    case g.kind
    of gkPure:
      if not isPureSubtree(c): return false
    of gkSame:
      if not subtreeEqual(caps[g.reg], c): return false
    of gkNone: discard
  true

proc runScope(e: Engine; dfaIdx: int; c0: Cursor; rem0: int;
              caps: var seq[Cursor]): seq[int] =
  ## Walk the scope DFA over the children bounded by `rem0` body tokens.
  ## Returns the accept rule ids (priority order) of the final state, or @[]
  ## if the scope died or ended at a non-accepting state.
  var c = c0
  var rem = rem0
  var state = 0
  while rem > 0:
    let st = e.dfas[dfaIdx].states[state]
    var bi = -1
    for i in 0 ..< st.buckets.len:
      if discMatchesChild(st.buckets[i].disc, c): bi = i; break
    if bi < 0: return @[]
    let bucket = st.buckets[bi]
    var rf = -1
    for i in 0 ..< bucket.refines.len:
      if guardsPass(bucket.refines[i].guards, c, caps): rf = i; break
    if rf < 0: return @[]
    let refine = bucket.refines[rf]
    for reg in refine.writes: caps[reg] = c
    let w = subtreeWidth(c)
    if refine.descend:
      var cc = c
      let childRem = int cursorJump(c)
      inc cc
      if runScope(e, refine.childDFA, cc, childRem, caps).len == 0:
        return @[]
    rem -= w
    skip c
    state = refine.dest
  result = e.dfas[dfaIdx].states[state].accepts

# ---- DO template emission ------------------------------------------------

proc emitRhs(e: Engine; t: var Cursor; dest: var TokenBuf; caps: seq[Cursor];
             capIndex: Table[SymId, int]; info: NifLineInfo) =
  ## Emit one template value, advancing `t` past it. Synthesized tokens are
  ## stamped with `info`; spliced captures keep their own info via `addSubtree`.
  case t.kind
  of Ident:
    let s = t.pool.symId(strVal(t))
    assert s in capIndex, "DO references unbound name: " & strVal(t)
    dest.addSubtree caps[getOrQuit(capIndex, s)]
    inc t
  of IntLit:   dest.addIntLit intVal(t);   dest.appendLineInfo info; inc t
  of UIntLit:  dest.addUIntLit uintVal(t); dest.appendLineInfo info; inc t
  of FloatLit: dest.addFloatLit floatVal(t); dest.appendLineInfo info; inc t
  of CharLit:  dest.addCharLit charLit(t); dest.appendLineInfo info; inc t
  of StrLit:   dest.addStrLit strVal(t);   dest.appendLineInfo info; inc t
  of Symbol:   dest.addSymUse symName(t);  dest.appendLineInfo info; inc t
  of DotToken: dest.addDotToken();         dest.appendLineInfo info; inc t
  of TagLit:
    if ruleTok(e, t) in MetaToks:
      let childName = metaChildName(t)
      let s = t.pool.symId(childName)
      assert s in capIndex, "DO meta references unbound name: " & childName
      dest.addSubtree caps[getOrQuit(capIndex, s)]
      skip t
    else:
      dest.openTag cursorTagId(t)
      dest.appendLineInfo info
      t.into:
        while t.hasMore:
          emitRhs(e, t, dest, caps, capIndex, info)
      dest.closeTag()
  else:
    fail "unsupported DO token: " & $t.kind

# ---- driver ---------------------------------------------------------------

proc tryRulesAt(e: Engine; node: Cursor; dest: var TokenBuf): bool =
  if node.kind != TagLit: return false
  let dfaIdx = e.dispatch.getOrDefault(cursorTagId(node), -1)
  if dfaIdx < 0: return false
  var caps = newSeq[Cursor](e.numRegs)
  var cc = node
  let rem = int cursorJump(node)
  inc cc
  let accepts = runScope(e, dfaIdx, cc, rem, caps)
  for r in accepts:                        # priority (ascending rule id) order
    template rule: untyped = e.rules[r]     # no copy (Rule holds a TokenBuf)
    var ok = true
    for i in 0 ..< rule.sideConds.len:
      let sc = rule.sideConds[i]
      var args: seq[Cursor] = @[]
      for s in sc.slots: args.add caps[s]
      let p = e.preds.getOrDefault(sc.pred)
      if p == nil or not p(args):
        ok = false
        break
    if ok:
      var rhs = rule.rhs
      emitRhs(e, rhs, dest, caps, rule.capIndex, rawLineInfo(node))
      return true
  return false

# ---- constant aggregate read folding --------------------------------------

proc isConstScalar(node: Cursor; allowSuf = true): bool =
  ## True for scalar literals (and suffixed forms); excludes aggregates, strings, `nil`.
  if node.kind in {IntLit, UIntLit, FloatLit, CharLit}: return true
  if node.exprKind in {TrueC, FalseC, InfC, NeginfC, NanC}: return not node.childCursor.hasMore
  result = false
  if allowSuf and node.exprKind == SufC:
    var child = node.childCursor
    if child.hasMore and isConstScalar(child, false):
      skip child
      if child.hasMore and child.kind == StrLit:
        skip child
        result = not child.hasMore

proc emitConstScalar(dest: var TokenBuf; value: Cursor; info: NifLineInfo) =
  ## Emit a scalar constant literal into `dest`, stamped with `info`.
  case value.kind
  of IntLit: dest.addIntLit intVal(value)
  of UIntLit: dest.addUIntLit uintVal(value)
  of FloatLit: dest.addFloatLit floatVal(value)
  of CharLit: dest.addCharLit charLit(value)
  of StrLit: dest.addStrLit strVal(value)
  of TagLit:
    dest.openTag cursorTagId(value)
    dest.appendLineInfo info
    var child = value.childCursor
    while child.hasMore:
      emitConstScalar(dest, child, NoNifLineInfo)
      skip child
    dest.closeTag()
    return
  else: return
  dest.appendLineInfo info

proc lookupObjField(oconstr: Cursor; field: SymId; wantDepth: int64;
                    value: var Cursor): bool =
  ## Find `(kv field val [depth])` in `oconstr`; bail on nested base `oconstr`.
  result = false
  var cur = oconstr.childCursor
  if cur.hasMore: skip cur
  while cur.hasMore and not result:
    if cur.exprKind == OconstrC: return false
    if cur.substructureKind == KvU:
      var kv = cur.childCursor
      if kv.hasMore and kv.kind == Symbol and symId(kv) == field:
        skip kv
        value = kv
        if kv.hasMore:
          skip kv
          var kvDepth = 0'i64
          result = (not kv.hasMore or constInt(kv, kvDepth)) and kvDepth == wantDepth
    skip cur

proc evalConstAgg(node: Cursor; module: var MainModule; depth: int;
                  value: var Cursor): bool =
  ## Resolve a compile-time constant aggregate or element referenced by `node`.
  result = false
  if depth > 16: return
  if node.kind == Symbol:
    if not canLoadForeign(module, symId(node)): return
    let decl = getDeclOrNil(module, symId(node))
    if decl == nil or decl.kind != ConstY or decl.isImport or decl.pos.stmtKind != ConstS: return
    var pos = decl.pos
    value = takeVarDecl(pos).value
    result = not cursorIsNil(value) and value.kind != DotToken
  elif node.kind == TagLit:
    var cur = node.childCursor
    if not cur.hasMore: return
    if node.exprKind == ParC: return evalConstAgg(cur, module, depth + 1, value)
    let base = cur
    skip cur
    var baseVal = default(Cursor)
    if node.exprKind == AtC:
      var index = 0'i64
      if cur.hasMore and constInt(cur, index) and index >= 0 and
         evalConstAgg(base, module, depth + 1, baseVal) and baseVal.exprKind == AconstrC:
        var elem = baseVal.childCursor
        if elem.hasMore: skip elem
        while elem.hasMore and index > 0:
          dec index
          skip elem
        value = elem
        result = elem.hasMore
    elif node.exprKind == DotC and cur.hasMore and cur.kind == Symbol:
      let field = symId(cur)
      skip cur
      var wantDepth = 0'i64
      if (not cur.hasMore or (constInt(cur, wantDepth) and wantDepth >= 0)) and
         evalConstAgg(base, module, depth + 1, baseVal) and baseVal.exprKind == OconstrC:
        result = lookupObjField(baseVal, field, wantDepth, value)
  if result and value.kind == Symbol:
    let nextNode = value
    result = evalConstAgg(nextNode, module, depth + 1, value)

proc memOpLocCount(module: ptr MainModule; node: Cursor): int =
  ## Leading lvalue operand count for memory-op `(instr ...)` nodes.
  result = 0
  if module != nil and node.stmtKind == InstrS:
    let callee = node.childCursor
    if callee.kind == Symbol and canLoadForeign(module[], callee.symId):
      var bits = 0
      let op = intrinsicOfCallee(module[], callee, bits)
      if op.isMemOp: result = memLocOperands(op)

proc childTakesAddr(node: Cursor; childIdx: int; inAddr: bool; instrLocs: int): bool =
  ## True when child `childIdx` of `node` is evaluated as an lvalue.
  if node.exprKind in AddrKinds or node.stmtKind == AsgnS: return childIdx == 0
  if node.stmtKind == KeepovfS: return childIdx == 1
  if instrLocs > 0: return childIdx >= 1 and childIdx <= instrLocs
  if not inAddr: return false
  case node.exprKind
  of DotC, AtC, ParC: childIdx == 0
  of ConvC, CastC: childIdx == 1
  of BaseobjC: childIdx == 2
  else: false

proc rewriteNode(e: Engine; src: var Cursor; dest: var TokenBuf;
                 fired: var bool; inList = false;
                 module: ptr MainModule = nil; inAddr = false) =
  ## `inList`: `src` is a member of a `(stmts …)`/`(scope …)`. A statement a rule
  ## rewrites to `.` — `(ite (false) A .)` — is then dropped instead of left as
  ## a hole: Leng lists carry none.
  if src.kind != TagLit:
    dest.addSubtree src
    skip src
    return
  let tagId = cursorTagId(src)
  let isList = tagId == e.stmtsTag or tagId == e.scopeTag
  let parentNode = src
  let instrLocs = memOpLocCount(module, parentNode)
  # The children are rewritten straight into `dest`, and the rules are tried on
  # the node right where it landed. Building every node in a buffer of its own
  # and copying it into its parent's copied each node once per ancestor, and
  # cost two buffers per node, rules or not.
  let start = dest.len
  dest.openTag tagId
  dest.appendLineInfo rawLineInfo(src)
  src.into:
    var childIdx = 0
    while src.hasMore:
      let childInAddr = module != nil and childTakesAddr(parentNode, childIdx, inAddr, instrLocs)
      rewriteNode(e, src, dest, fired, isList, module, childInAddr)
      inc childIdx
  dest.closeTag()
  if module != nil and not inAddr:
    var node = cursorAt(dest, start)
    var folded = default(Cursor)
    let hitConst = node.exprKind in {DotC, AtC} and
                   evalConstAgg(node, module[], 0, folded) and isConstScalar(folded)
    let info = rawLineInfo(node)
    endRead node
    if hitConst:
      fired = true
      dest.shrink start
      emitConstScalar(dest, folded, info)
      return
  if e.dispatch.getOrDefault(tagId, -1) < 0: return   # no rule has this head
  var node = cursorAt(dest, start)
  var res = createTokenBuf(16, e.pool, e.tags)
  let hit = tryRulesAt(e, node, res)
  endRead node            # before `dest` changes again (no copy-on-write)
  if hit:
    fired = true
    dest.shrink start
    var rc = res.beginRead()
    if not (inList and rc.kind == DotToken):
      dest.addSubtree rc
    endRead rc

proc runRewrites*(e: Engine; buf: var TokenBuf; module: ptr MainModule = nil): bool =
  var fired = false
  var dest = createTokenBuf(buf.len + 16, e.pool, e.tags)
  var c = buf.beginRead()
  while c.hasMore:
    rewriteNode(e, c, dest, fired, module = module)
  endRead c
  if fired:
    buf = ensureMove(dest)
  result = fired

proc runRewritesFix*(e: Engine; buf: var TokenBuf; module: ptr MainModule = nil;
                     maxIter = 32): int {.discardable.} =
  result = 0
  for i in 0 ..< maxIter:
    if not runRewrites(e, buf, module): return
    inc result

# ---- canonicalization -----------------------------------------------------
#
# Alpha-rename parameters (`(param :s …)`) and local variables (`(var :s …)`)
# to canonical positional names (`_canon.0`, `_canon.1`, …) in declaration
# (pre-order) order, rewriting all their uses. Proc names, `gvar`/`tvar`/
# `const`, types and any other globals are left untouched. nimony already
# gives every distinct binding a unique symbol, so a flat identity map handles
# shadowing without scope tracking. This makes two procs that differ only in
# local/parameter names compare equal — the cheap "structural exactness"
# the rewrite patterns rely on.

proc collectLocals(e: Engine; c: Cursor;
                   map: var Table[SymId, string]; counter: var int) =
  if c.kind != TagLit: return
  let t = cursorTagId(c)
  if t == e.tagVar or t == e.tagParam:
    var nm = c
    inc nm                                   # first child = the name
    if nm.kind == SymbolDef:
      let s = e.pool.symId(symName(nm))
      if s notin map:
        map[s] = "_canon." & $counter
        inc counter
  var cc = c
  cc.into:
    while cc.hasMore:
      collectLocals(e, cc, map, counter)
      skip cc

proc rebuildCanon(e: Engine; src: var Cursor; dest: var TokenBuf;
                  map: Table[SymId, string]) =
  case src.kind
  of TagLit:
    dest.openTag cursorTagId(src)
    dest.appendLineInfo rawLineInfo(src)
    src.into:
      while src.hasMore: rebuildCanon(e, src, dest, map)
    dest.closeTag()
  of Symbol:
    let s = e.pool.symId(symName(src))
    if s in map:
      dest.addSymUse getOrQuit(map, s); dest.appendLineInfo rawLineInfo(src); inc src
    else:
      dest.addSubtree src; skip src
  of SymbolDef:
    let s = e.pool.symId(symName(src))
    if s in map:
      dest.addSymDef getOrQuit(map, s); dest.appendLineInfo rawLineInfo(src); inc src
    else:
      dest.addSubtree src; skip src
  else:
    dest.addSubtree src; skip src

proc canon*(e: Engine; buf: var TokenBuf) =
  ## Canonicalize parameter and local-variable names in place.
  var map = initTable[SymId, string]()
  var counter = 0
  var c0 = buf.beginRead()
  while c0.hasMore:
    collectLocals(e, c0, map, counter)
    skip c0
  endRead c0
  if map.len == 0: return
  var dest = createTokenBuf(buf.len + 8, e.pool, e.tags)
  var c = buf.beginRead()
  while c.hasMore: rebuildCanon(e, c, dest, map)
  endRead c
  buf = ensureMove(dest)

# ---- self-tests -----------------------------------------------------------

when isMainModule:
  from std / os import getTempDir, `/`, fileExists, removeFile
  import std / syncio
  const rulesSrc = staticRead("rules/arith.rewrite.nif")

  var e = newEngine(rulesSrc)

  proc sameTokens(a, b: var TokenBuf): bool =
    if a.len != b.len: return false
    for i in 0 ..< a.len:
      if not (a[i] == b[i]): return false
    true

  template check(buf: var TokenBuf; expected: string) =
    var exp = e.parseInput(expected)
    if not sameTokens(buf, exp):
      echo "FAILED. got:\n", toString(buf), "\nexpected:\n", toString(exp)
      quit 1

  block dead_ite_leaves_no_hole:
    # `(ite (false) A .)` rewrites to its missing else-arm, `.`; as a statement
    # list member that is a hole, so it goes instead.
    var buf = e.parseInput("(stmts (asgn x.0.M 1) (ite (false) (stmts (asgn x.0.M 2)) .) (asgn y.0.M 3))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M 1) (asgn y.0.M 3))"

  block dead_ite_in_fixed_slot_keeps_dot:
    # The same rewrite as an `ite` ARM is a slot, not a list member: keep `.`.
    var buf = e.parseInput("(stmts (ite c.0.M (ite (false) (stmts (asgn x.0.M 2)) .) .))")
    discard runRewrites(e, buf)
    check buf, "(stmts (ite c.0.M . .))"

  block add_zero:
    var buf = e.parseInput("(stmts (asgn x.0.M (add (i 32) x.0.M 0)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M x.0.M))"

  block zero_add:
    var buf = e.parseInput("(stmts (asgn x.0.M (add (i 32) 0 y.0.M)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block sub_zero:
    var buf = e.parseInput("(stmts (asgn x.0.M (sub (i 32) y.0.M 0)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block mul_one:
    var buf = e.parseInput("(stmts (asgn x.0.M (mul (i 32) y.0.M 1)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block mul_one_left:
    var buf = e.parseInput("(stmts (asgn x.0.M (mul (i 32) 1 y.0.M)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block mul_zero_pure:
    var buf = e.parseInput("(stmts (asgn x.0.M (mul (i 32) y.0.M 0)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M 0))"

  block mul_zero_impure_blocked:
    var buf = e.parseInput("(stmts (asgn x.0.M (mul (i 32) (call side.0.M) 0)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M (mul (i 32) (call side.0.M) 0)))"

  block and_true:
    var buf = e.parseInput("(stmts (asgn x.0.M (and (true) y.0.M)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block or_false:
    var buf = e.parseInput("(stmts (asgn x.0.M (or (false) y.0.M)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block not_not:
    var buf = e.parseInput("(stmts (asgn x.0.M (not (not y.0.M))))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block deref_addr:
    var buf = e.parseInput("(stmts (asgn x.0.M (deref (addr y.0.M))))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M y.0.M))"

  block sub_same:
    var buf = e.parseInput("(stmts (asgn x.0.M (sub (i 32) y.0.M y.0.M)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M 0))"

  block sub_same_float_blocked:
    # x - x is not 0.0 under IEEE (NaN/Inf), and the `0` template would be
    # ill-typed for a float sub — the rule is gated to `(i W)`.
    var buf = e.parseInput("(stmts (asgn x.0.M (sub (f 64) y.0.M y.0.M)))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M (sub (f 64) y.0.M y.0.M)))"

  block sub_same_impure_blocked:
    # Eliding `f() - f()` would drop two calls — the rule requires a pure operand.
    var buf = e.parseInput("(stmts (asgn x.0.M (sub (i 32) (call side.0.M) (call side.0.M))))")
    discard runRewrites(e, buf)
    check buf, "(stmts (asgn x.0.M (sub (i 32) (call side.0.M) (call side.0.M))))"

  block fixpoint_chains:
    var buf = e.parseInput("(stmts (asgn x.0.M (and (true) (not (not y.0.M)))))")
    let passes = runRewritesFix(e, buf)
    doAssert passes >= 1
    check buf, "(stmts (asgn x.0.M y.0.M))"

  # (The old `mul by pow2 → shl` rule is gone: its DO template shifted by the
  # MULTIPLIER (`y*4 → y shl 4`), not log2 of it — a miscompile the DSL cannot
  # express correctly until it grows computed captures.)

  block deref_addr_lvalue:
    # The fold must also fire in lvalue position — this is the inliner residue
    # of `inc(addr i)` after addr-argument substitution.
    var buf = e.parseInput(
      "(stmts (asgn (deref (addr i.0.M)) (add (i 64) (deref (addr i.0.M)) 1)))")
    let passes = runRewritesFix(e, buf)
    doAssert passes >= 1
    check buf, "(stmts (asgn i.0.M (add (i 64) i.0.M 1)))"

  block shared_pool_engine:
    # The production pipeline hands the module's pool/tags in; tag ids of rules
    # and input then coincide (including the `pure` DSL/Leng name collision).
    var pool = newPool()
    var tags = newTagPool()
    var e2 = newEngine(rulesSrc, pool, tags)
    var buf = parseFromBuffer("(stmts (asgn x.0.M (deref (addr y.0.M))))", "M",
                              100, pool, tags)
    discard runRewrites(e2, buf)
    var exp = parseFromBuffer("(stmts (asgn x.0.M y.0.M))", "M", 100, pool, tags)
    if not sameTokens(buf, exp):
      echo "FAILED shared_pool_engine. got:\n", toString(buf)
      quit 1

  proc testLineInfo() =
    let f = e.pool.filenames.getOrIncl("t.nim")
    proc li(f: FileId; line, col: int): NifLineInfo =
      NifLineInfo(file: f, line: line.int32, col: col.int32)
    template tg(name: string): TagId = e.tags.registerTag(name)

    var buf = createTokenBuf(32, e.pool, e.tags)
    buf.openTag tg("stmts");  buf.appendLineInfo li(f, 1, 1)
    buf.openTag tg("asgn");   buf.appendLineInfo li(f, 2, 1)
    buf.addSymUse "x.0.M";    buf.appendLineInfo li(f, 2, 3)
    buf.openTag tg("add");    buf.appendLineInfo li(f, 3, 1)
    buf.openTag tg("i");      buf.appendLineInfo li(f, 3, 5)
    buf.addIntLit 32;         buf.appendLineInfo li(f, 3, 7)
    buf.closeTag()
    buf.addSymUse "x.0.M";    buf.appendLineInfo li(f, 4, 2)
    buf.addIntLit 0;          buf.appendLineInfo li(f, 4, 4)
    buf.closeTag()            # add
    buf.closeTag()            # asgn
    buf.closeTag()            # stmts

    discard runRewrites(e, buf)

    template wantInfo(c: Cursor; ln, cl: int) =
      let g = rawLineInfo(c)
      doAssert g.file.uint32 == f.uint32 and g.line == ln.int32 and g.col == cl.int32,
        "line info: got " & $g.line & "," & $g.col & " want " & $ln & "," & $cl

    var c = buf.beginRead()
    wantInfo c, 1, 1
    c.into:
      wantInfo c, 2, 1
      c.into:
        wantInfo c, 2, 3
        skip c
        wantInfo c, 4, 2
        skip c

    # A synthesized template token (the `x - x → 0` rule's `0`) is stamped with
    # the matched node's info.
    var b2 = createTokenBuf(16, e.pool, e.tags)
    b2.openTag tg("sub");  b2.appendLineInfo li(f, 7, 3)
    b2.openTag tg("i");    b2.appendLineInfo li(f, 7, 5)
    b2.addIntLit 32
    b2.closeTag()
    b2.addSymUse "y.0.M";  b2.appendLineInfo li(f, 7, 9)
    b2.addSymUse "y.0.M";  b2.appendLineInfo li(f, 7, 11)
    b2.closeTag()

    discard runRewrites(e, b2)
    var d = b2.beginRead()
    doAssert d.kind == IntLit and intVal(d) == 0
    wantInfo d, 7, 3
  testLineInfo()

  block canon_alpha_equiv:
    # Two procs differing only in parameter/local names canon to the same tokens.
    var a = e.parseInput(
      "(proc :foo.0.M (params (param :x.1.M (i 32)) (param :y.2.M (i 32))) (i 32) " &
      "(stmts (var :t.3.M (i 32) (add (i 32) x.1.M y.2.M)) (ret t.3.M)))")
    var b = e.parseInput(
      "(proc :foo.0.M (params (param :aa.7.M (i 32)) (param :bb.8.M (i 32))) (i 32) " &
      "(stmts (var :zz.9.M (i 32) (add (i 32) aa.7.M bb.8.M)) (ret zz.9.M)))")
    e.canon(a)
    e.canon(b)
    doAssert sameTokens(a, b), "canon: alpha-equivalent procs should match\n" &
      toString(a) & "\n---\n" & toString(b)

  block canon_keeps_globals:
    # Proc name, gvar, type and the constant are preserved; only x/local renamed.
    var a = e.parseInput(
      "(proc :bar.0.M (params (param :x.1.M (i 32))) (i 32) " &
      "(stmts (asgn g.5.M (add (i 32) x.1.M (i 32))) (ret x.1.M)))")
    e.canon(a)
    # g.5.M (a global, not a (var)/(param) decl here) and bar.0.M survive;
    # x.1.M becomes _canon.0.
    var exp = e.parseInput(
      "(proc :bar.0.M (params (param :_canon.0 (i 32))) (i 32) " &
      "(stmts (asgn g.5.M (add (i 32) _canon.0 (i 32))) (ret _canon.0)))")
    doAssert sameTokens(a, exp), "canon globals:\n" & toString(a)

  block canon_distinct_after_rename:
    # Differently-structured procs must not collapse: different proc names stay.
    var a = e.parseInput("(proc :p.0.M (params (param :x.1.M (i 32))) (i 32) (stmts (ret x.1.M)))")
    var b = e.parseInput("(proc :q.0.M (params (param :x.1.M (i 32))) (i 32) (stmts (ret x.1.M)))")
    e.canon(a); e.canon(b)
    doAssert not sameTokens(a, b), "canon must keep distinct proc names distinct"

  proc testConstAggFold() =
    const ModSrc = """(.nif24)
(stmts
 (const :arr.0.M . (array (i 64) 3) (aconstr (array (i 64) 3) (suf 10 "i64") (suf 20 "i64") (suf 30 "i64")))
 (const :things.0.M . (array Thing.0.M 2)
  (aconstr (array Thing.0.M 2)
   (oconstr Thing.0.M (kv kind.0 0) (kv flag.0 (true)) (kv val.0 -0.25)
    (kv center.0 (oconstr Vec.0.M (kv x.0 1.5) (kv y.0 (suf -2.0 "f32")))))
   (oconstr Thing.0.M (kv kind.0 (suf 1 "i64")) (kv flag.0 (false)) (kv val.0 2.5)
    (kv center.0 (oconstr Vec.0.M (kv x.0 -3.0) (kv y.0 (suf 4.0 "f32")))))))
 (const :subObj.0.M . Sub.0.M
  (oconstr Sub.0.M (addr vt.0.M) (kv baseF.0 11 1) (kv subF.0 22)))
 (const :aliasObj.0.M . Sub.0.M subObj.0.M)
 (const :extConst.0.M (pragmas (importc "ext")) (array (i 64) 2) (aconstr (array (i 64) 2) 1 2))
 (const :emptyConst.0.M . (array (i 64) 2) .)
 (const :strArr.0.M . (array (aptr (c 8)) 1) (aconstr (array (aptr (c 8)) 1) "hi"))
 (gvar :mutArr.0.M . (array (i 64) 2) (aconstr (array (i 64) 2) 7 8))
 (proc :copyMem.0.M (params (param :d.0 . (i 64)) (param :s.0 . (i 64)) (param :n.0 . (i 64))) .
  (pragmas (intrinsic "copyMemIns")) .))
"""
    const ForeignSrc = """(stmts
 (const :fArr.0.F . (array (i 64) 2) (aconstr (array (i 64) 2) (suf 77 "i64") (suf 88 "i64")))
 (const :fObj.0.F . Pt.0.F (oconstr Pt.0.F (kv x.0 3.25) (kv y.0 -1.5))))
"""
    let dir = getTempDir()
    let mainPath = dir / "M.c.nif"
    let foreignPath = dir / "F.c.nif"
    writeFile(mainPath, ModSrc)
    var fBuf = parseFromBuffer(ForeignSrc, "F", 64, sharedTags = createLengTagPool())
    writeFile(foreignPath, toModuleString(fBuf, ".F"))
    defer:
      if fileExists(mainPath): removeFile(mainPath)
      if fileExists(foreignPath): removeFile(foreignPath)
    var pool = newPool()
    var tags = createLengTagPool()
    var module = load(mainPath, pool, tags)
    var eng = newEngine(rulesSrc, pool, tags)

    proc runCase(src, expected: string) =
      var buf = parseFromBuffer(src, "M", 64, pool, tags)
      discard runRewritesFix(eng, buf, addr module)
      var want = parseFromBuffer(expected, "M", 64, pool, tags)
      doAssert sameTokens(buf, want),
        "const fold mismatch:\n  got:  " & toString(buf) & "\n  want: " & toString(want)

    # Scalar array, object field, nested object field, float/bool/suf, deref+haddr, branch fold
    runCase "(stmts (asgn r.0 (at arr.0.M 1)))",
            "(stmts (asgn r.0 (suf 20 \"i64\")))"
    runCase "(stmts (asgn r.0 (dot (at things.0.M 0) kind.0 0)))",
            "(stmts (asgn r.0 0))"
    runCase "(stmts (asgn r.0 (dot (at things.0.M 1) flag.0 0)))",
            "(stmts (asgn r.0 (false)))"
    runCase "(stmts (asgn r.0 (dot (dot (at things.0.M 0) center.0 0) y.0 0)))",
            "(stmts (asgn r.0 (suf -2.0 \"f32\")))"
    runCase "(stmts (ite (eq (dot (deref (haddr (at things.0.M 1))) kind.0 0) 1) (asgn r.0 1) (asgn r.0 2)))",
            "(stmts (asgn r.0 1))"
    # Cross-module constant array and object (loaded via .c.nif embedded index)
    runCase "(stmts (asgn r.0 (at fArr.0.F 1)))",
            "(stmts (asgn r.0 (suf 88 \"i64\")))"
    runCase "(stmts (asgn r.0 (dot fObj.0.F x.0 0)))",
            "(stmts (asgn r.0 3.25))"
    # Inheritance depth + const alias
    runCase "(stmts (asgn r.0 (dot aliasObj.0.M baseF.0 1)))",
            "(stmts (asgn r.0 11))"
    runCase "(stmts (asgn r.0 (dot subObj.0.M baseF.0 0)))",
            "(stmts (asgn r.0 (dot subObj.0.M baseF.0 0)))"
    # Non-literal, OOB, negative index, aggregate result, string, gvar, importc, missing init stay untouched
    runCase "(stmts (asgn r.0 (at arr.0.M i.0)))",
            "(stmts (asgn r.0 (at arr.0.M i.0)))"
    runCase "(stmts (asgn r.0 (at arr.0.M 5)))",
            "(stmts (asgn r.0 (at arr.0.M 5)))"
    runCase "(stmts (asgn r.0 (at arr.0.M -1)))",
            "(stmts (asgn r.0 (at arr.0.M -1)))"
    runCase "(stmts (asgn r.0 (dot (at things.0.M 0) center.0 0)))",
            "(stmts (asgn r.0 (dot (at things.0.M 0) center.0 0)))"
    runCase "(stmts (asgn r.0 (at strArr.0.M 0)))",
            "(stmts (asgn r.0 (at strArr.0.M 0)))"
    runCase "(stmts (asgn r.0 (at mutArr.0.M 0)))",
            "(stmts (asgn r.0 (at mutArr.0.M 0)))"
    runCase "(stmts (asgn r.0 (at extConst.0.M 0)))",
            "(stmts (asgn r.0 (at extConst.0.M 0)))"
    runCase "(stmts (asgn r.0 (at emptyConst.0.M 0)))",
            "(stmts (asgn r.0 (at emptyConst.0.M 0)))"
    # Lvalue positions must NOT fold, while index subexpr inside addr CAN fold
    runCase "(stmts (asgn p.0 (addr (at arr.0.M 0))))",
            "(stmts (asgn p.0 (addr (at arr.0.M 0))))"
    runCase "(stmts (asgn p.0 (haddr (dot (at things.0.M 0) kind.0 0))))",
            "(stmts (asgn p.0 (haddr (dot (at things.0.M 0) kind.0 0))))"
    runCase "(stmts (instr copyMem.0.M d.0 (at arr.0.M 0) 8))",
            "(stmts (instr copyMem.0.M d.0 (at arr.0.M 0) 8))"
    runCase "(stmts (asgn p.0 (addr (at d.0 (dot (at things.0.M 1) kind.0 0)))))",
            "(stmts (asgn p.0 (addr (at d.0 (suf 1 \"i64\")))))"
  testConstAggFold()

  echo "vmrewriter.nim: all self-tests passed"
