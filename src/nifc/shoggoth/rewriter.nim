#
#
#           NIFC Peephole Rewrite Engine (Codegen)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Macro-based NIF-pattern rewrite engine.
##
## Each rule's LHS and RHS are translated **at compile time** to Nim
## code. There is no runtime interpreter — the matcher for every rule
## becomes a directly-inlined sequence of cursor checks, and the
## dispatch becomes a `case` on the root tag's `TagId`.
##
## ## Rule syntax (NIF)
##
## ```nif
## (rules
##   (rule (lhs (add T X 0))               (rhs X))
##   (rule (lhs (mul T (pure X) 0))        (rhs 0))
##   (rule (lhs (mul T X (int K)) (when (isPow2 K)))
##         (rhs (shl T X (int K))))
## )
## ```
##
## - Bare identifier — capture-any-subtree, named for use in the RHS.
##   Because NIF identifiers never contain a dot and user-level symbols
##   always do (`foo.0.M`), pattern variables can't clash with input
##   names.
## - `(any X)` / `(int X)` / `(sym X)` / `(lit X)` / `(pure X)` — typed
##   captures. `(pure X)` rejects subtrees with side effects; required
##   when the RHS discards the capture.
## - `(same X)` — backreference; structurally equal to an earlier `X`.
## - `(when (predName cap1 cap2 …))` — sibling of `lhs`/`rhs` in a rule;
##   each child of `when` is emitted as a Nim call to a regular proc
##   defined in the surrounding scope. The call's arguments are the
##   captured `Cursor` values. The rule fires iff every `when` call
##   returns `true`.
##
## ## Macro
##
## ```nim
## genRewriter staticRead("rules/arith.rewrite.nif")
## # now `runRewrites*(buf: var TokenBuf)` and
## # `runRewritesFix*(buf: var TokenBuf; maxIter = 32): int` are in scope.
## ```
##
## ## Runtime support (this module)
##
## - `subtreeEqual` for `(same X)`.
## - `isPureSubtree` for `(pure X)`.
## - Re-exports of `Patchset`, `addSubst`, `apply` from `patchsets`.

import std / [macros, strutils, tables, assertions]
include "../../lib" / nifprelude
import nifstreams, nifcursors
import ".." / nifc_model
import ".." / ".." / models / tags  # `TagEnum` and the `*TagId` enumerators
import patchsets_legacy
export patchsets_legacy

# ---- Runtime helpers ------------------------------------------------------

proc subtreeEqual*(a, b: Cursor): bool =
  ## Structural equality of two subtrees. virtualParRi-safe: it bounds each
  ## subtree with `skip` (which uses the jump fields) instead of counting
  ## explicit `ParRi` tokens to balance depth. Under `-d:virtualParRi` a
  ## sealed scope's closing paren is elided, so the old depth-balanced walk
  ## never returned to depth 0 and ran off the end of the cursor's window
  ## (`c.rem` underflow). Comparing the two subtrees' real-token spans
  ## position-by-position avoids that: identical token sequences ⇔
  ## identical structure (ParLe carry their tag; jump fields match for
  ## equal structure; atoms carry their interned payload in `uoperand`).
  var aEnd = a
  skip aEnd
  var bEnd = b
  skip bEnd
  let n = cursorToPosition(a, aEnd)
  if n != cursorToPosition(b, bEnd): return false   # different token counts
  var x = a
  var y = b
  var i = 0
  while i < n:
    if x.kind != y.kind: return false
    if x.kind == ParLe:
      if x.tagId != y.tagId: return false
    else:
      if x.uoperand != y.uoperand: return false
    inc i
    if i < n:                       # never advance past the last token
      inc x
      inc y
  return true

const NullaryConstTags* = ["true", "false", "nil", "inf", "neginf", "nan"]

proc isPureSubtree*(c: Cursor): bool =
  ## Conservative purity test for `(pure X)` captures. True for atoms
  ## and the well-known nullary constant tags.
  case c.kind
  of IntLit, UIntLit, FloatLit, CharLit, StringLit, Symbol, DotToken:
    true
  of ParLe:
    pool.tags[c.tagId] in NullaryConstTags
  else:
    false

# ---- Compile-time NIF parser ----------------------------------------------
#
# Minimal subset: ParLe with tag name, ParRi, bare identifiers, integer
# literals, dot. No escapes, no line info, no comments, no symbols, no
# string literals. Enough for rule files.

type
  PNodeKind = enum
    pnTag, pnIdent, pnIntLit, pnDotToken
  PNode = ref object
    case kind: PNodeKind
    of pnTag:
      tag: string
      kids: seq[PNode]
    of pnIdent:
      name: string
    of pnIntLit:
      ival: int64
    of pnDotToken:
      discard

proc isIdentChar(ch: char): bool =
  ch in {'a'..'z', 'A'..'Z', '0'..'9', '_', '=', '!', '?'}

proc isIdentStart(ch: char): bool =
  ch in {'a'..'z', 'A'..'Z', '_', '=', '!', '?'}

proc parsePNode(src: string; i: var int): PNode =
  # skip whitespace and comments (we ignore # ... # blocks for safety)
  while i < src.len:
    let ch = src[i]
    if ch in {' ', '\t', '\n', '\r'}:
      inc i
    elif ch == '#':
      inc i
      while i < src.len and src[i] != '#': inc i
      if i < src.len: inc i  # skip closing #
    else:
      break
  if i >= src.len:
    error "unexpected end of rule source"
  let ch = src[i]
  if ch == '(':
    inc i
    var name = ""
    while i < src.len and isIdentChar(src[i]):
      name.add src[i]
      inc i
    if name.len == 0:
      # could be a directive like `(.nif26)` — skip whole thing
      if i < src.len and src[i] == '.':
        # eat tag name starting with '.'
        name.add '.'
        inc i
        while i < src.len and isIdentChar(src[i]):
          name.add src[i]
          inc i
      else:
        error "rule parse: expected tag name after '(' at " & $i
    result = PNode(kind: pnTag, tag: name, kids: @[])
    while true:
      # skip whitespace
      while i < src.len and src[i] in {' ', '\t', '\n', '\r'}:
        inc i
      if i >= src.len:
        error "rule parse: unmatched '(' for tag '" & name & "'"
      if src[i] == ')':
        inc i
        return
      result.kids.add parsePNode(src, i)
  elif ch == '.':
    inc i
    result = PNode(kind: pnDotToken)
  elif ch in {'0'..'9'} or (ch == '-' and i+1 < src.len and src[i+1] in {'0'..'9'}):
    var s = ""
    if ch == '-':
      s.add ch
      inc i
    while i < src.len and src[i] in {'0'..'9'}:
      s.add src[i]
      inc i
    result = PNode(kind: pnIntLit, ival: parseBiggestInt(s).int64)
  elif isIdentStart(ch):
    var s = ""
    while i < src.len and isIdentChar(src[i]):
      s.add src[i]
      inc i
    result = PNode(kind: pnIdent, name: s)
  else:
    error "rule parse: unexpected character '" & $ch & "' at offset " & $i

proc parseRuleFile(src: string): seq[PNode] =
  ## Returns the list of `(rule …)` nodes found in `src`. Skips
  ## directives and unwraps a top-level `(rules …)` if present.
  result = @[]
  var i = 0
  while true:
    while i < src.len and src[i] in {' ', '\t', '\n', '\r'}:
      inc i
    if i >= src.len: break
    let n = parsePNode(src, i)
    if n.kind == pnTag:
      if n.tag.len > 0 and n.tag[0] == '.':
        continue   # directive, skip
      if n.tag == "rules":
        for k in n.kids:
          if k.kind == pnTag and k.tag == "rule":
            result.add k
      elif n.tag == "rule":
        result.add n

# ---- Compile-time rule analysis -------------------------------------------

type
  Capture = object
    name: string
    kind: CaptureKind
  CaptureKind = enum
    ckAny, ckInt, ckSym, ckLit, ckPure

  Rule = object
    rootTag: string
    captures: OrderedTable[string, Capture]
    lhs: PNode
    rhs: PNode
    sideConds: seq[PNode]  # each is a (predName argName argName …) form

const MetaTags = ["any", "lit", "int", "sym", "pure", "same"]

proc captureKindOf(meta: string): CaptureKind =
  case meta
  of "any":  ckAny
  of "int":  ckInt
  of "sym":  ckSym
  of "lit":  ckLit
  of "pure": ckPure
  else:      ckAny

proc collectCaptures(lhs: PNode; caps: var OrderedTable[string, Capture]) =
  ## Walk the LHS and register each capture name in declaration order.
  case lhs.kind
  of pnIdent:
    if lhs.name notin caps:
      caps[lhs.name] = Capture(name: lhs.name, kind: ckAny)
  of pnTag:
    if lhs.tag in MetaTags:
      if lhs.kids.len != 1 or lhs.kids[0].kind != pnIdent:
        error "(" & lhs.tag & " …) expects exactly one identifier"
      let name = lhs.kids[0].name
      let k = captureKindOf(lhs.tag)
      if lhs.tag == "same":
        if name notin caps:
          error "(same " & name & "): " & name & " not previously bound"
      else:
        if name notin caps:
          caps[name] = Capture(name: name, kind: k)
    else:
      for kid in lhs.kids:
        collectCaptures(kid, caps)
  else: discard

proc analyzeRule(node: PNode): Rule =
  result = Rule()
  if node.kind != pnTag or node.tag != "rule":
    error "expected (rule …)"
  var lhsNode, rhsNode: PNode = nil
  for kid in node.kids:
    if kid.kind != pnTag:
      error "rule body: expected (lhs …) / (rhs …) / (when …)"
    case kid.tag
    of "lhs":
      if kid.kids.len != 1:
        error "(lhs …) takes exactly one child — the pattern"
      lhsNode = kid.kids[0]
    of "rhs":
      if kid.kids.len != 1:
        error "(rhs …) takes exactly one child — the template"
      rhsNode = kid.kids[0]
    of "when":
      for call in kid.kids:
        if call.kind != pnTag:
          error "(when …) children must be predicate calls"
        result.sideConds.add call
    else:
      error "unknown rule section: " & kid.tag
  if lhsNode == nil:
    error "rule missing (lhs …)"
  if rhsNode == nil:
    error "rule missing (rhs …)"
  if lhsNode.kind != pnTag:
    error "LHS root must be a tag"
  if lhsNode.tag in MetaTags or lhsNode.tag == "same":
    error "LHS root must be a concrete tag, got (" & lhsNode.tag & " …)"
  result.rootTag = lhsNode.tag
  result.lhs = lhsNode
  result.rhs = rhsNode
  collectCaptures(lhsNode, result.captures)

# ---- Codegen for a single LHS pattern -------------------------------------
#
# Produces a NimNode statement list that *attempts* to match against a
# cursor `c`. On success: leaves `c` advanced past the matched subtree
# and capture variables set. On failure: sets `ok` to false and consumes
# the remaining children to satisfy the `into:` template's invariant.

proc emitChildMatch(p: PNode; caps: OrderedTable[string, Capture];
                    cIdent, okIdent: NimNode): NimNode

proc emitTagMatch(p: PNode; caps: OrderedTable[string, Capture];
                  cIdent, okIdent: NimNode): NimNode =
  ## Match a concrete tag and its children. `cIdent` must be at the
  ## ParLe of this tag on entry. On exit (success): past the (real or
  ## virtual) ParRi.
  let tagName = p.tag
  if tagName == "true":
    # `(true)` — match nullary true tag and skip past.
    return quote do:
      if `cIdent`.kind != ParLe or pool.tags[`cIdent`.tagId] != "true":
        `okIdent` = false
      else:
        skip `cIdent`
  if tagName == "false":
    return quote do:
      if `cIdent`.kind != ParLe or pool.tags[`cIdent`.tagId] != "false":
        `okIdent` = false
      else:
        skip `cIdent`

  # Build statement: if c.kind != ParLe or tag mismatch → ok = false +
  # skip; else enter scope and check children in order.
  let tagLit = newLit(tagName)
  let childMatches = newStmtList()
  for kid in p.kids:
    childMatches.add emitChildMatch(kid, caps, cIdent, okIdent)
  # After all expected children: any remaining ⇒ failure. Either way,
  # consume the rest so `into:` is satisfied.
  let extrasCheck = quote do:
    if `okIdent` and `cIdent`.hasMore:
      `okIdent` = false
    while `cIdent`.hasMore: skip `cIdent`

  result = quote do:
    if `cIdent`.kind != ParLe or pool.tags[`cIdent`.tagId] != `tagLit`:
      `okIdent` = false
      if `cIdent`.hasMore: skip `cIdent`
    else:
      `cIdent`.into:
        `childMatches`
        `extrasCheck`

proc emitChildMatch(p: PNode; caps: OrderedTable[string, Capture];
                    cIdent, okIdent: NimNode): NimNode =
  ## Match one child position. Skips remaining work on prior failure.
  case p.kind
  of pnTag:
    if p.tag in MetaTags or p.tag == "same":
      # meta capture
      if p.kids.len != 1 or p.kids[0].kind != pnIdent:
        error "(" & p.tag & " …) expects one identifier"
      let varIdent = ident(p.kids[0].name)
      case p.tag
      of "any":
        result = quote do:
          if `okIdent`:
            if `cIdent`.hasMore:
              `varIdent` = `cIdent`; skip `cIdent`
            else:
              `okIdent` = false
      of "int":
        result = quote do:
          if `okIdent`:
            if `cIdent`.kind == IntLit:
              `varIdent` = `cIdent`; inc `cIdent`
            else:
              `okIdent` = false
              if `cIdent`.hasMore: skip `cIdent`
      of "sym":
        result = quote do:
          if `okIdent`:
            if `cIdent`.kind == Symbol:
              `varIdent` = `cIdent`; inc `cIdent`
            else:
              `okIdent` = false
              if `cIdent`.hasMore: skip `cIdent`
      of "lit":
        result = quote do:
          if `okIdent`:
            if `cIdent`.kind in {IntLit, UIntLit, FloatLit, CharLit, StringLit}:
              `varIdent` = `cIdent`; inc `cIdent`
            else:
              `okIdent` = false
              if `cIdent`.hasMore: skip `cIdent`
      of "pure":
        result = quote do:
          if `okIdent`:
            if `cIdent`.hasMore and isPureSubtree(`cIdent`):
              `varIdent` = `cIdent`; skip `cIdent`
            else:
              `okIdent` = false
              if `cIdent`.hasMore: skip `cIdent`
      of "same":
        result = quote do:
          if `okIdent`:
            if `cIdent`.hasMore and subtreeEqual(`varIdent`, `cIdent`):
              skip `cIdent`
            else:
              `okIdent` = false
              if `cIdent`.hasMore: skip `cIdent`
      else:
        error "unreachable meta " & p.tag
    else:
      # concrete tag — recursive match
      let inner = emitTagMatch(p, caps, cIdent, okIdent)
      result = quote do:
        if `okIdent`:
          if not `cIdent`.hasMore:
            `okIdent` = false
          else:
            `inner`
  of pnIdent:
    # bare identifier ≡ (any …)
    let varIdent = ident(p.name)
    result = quote do:
      if `okIdent`:
        if `cIdent`.hasMore:
          `varIdent` = `cIdent`; skip `cIdent`
        else:
          `okIdent` = false
  of pnIntLit:
    let v = newLit(p.ival)
    result = quote do:
      if `okIdent`:
        if `cIdent`.kind == IntLit and pool.integers[`cIdent`.intId] == `v`:
          inc `cIdent`
        else:
          `okIdent` = false
          if `cIdent`.hasMore: skip `cIdent`
  of pnDotToken:
    result = quote do:
      if `okIdent`:
        if `cIdent`.kind == DotToken:
          inc `cIdent`
        else:
          `okIdent` = false
          if `cIdent`.hasMore: skip `cIdent`

# ---- Codegen for a single RHS template ------------------------------------

proc emitRhs(p: PNode; caps: OrderedTable[string, Capture];
             destIdent, infoIdent: NimNode): NimNode =
  ## Produce statements that *emit* the RHS into a `TokenBuf` named by
  ## `destIdent`, using the line info `infoIdent`.
  case p.kind
  of pnTag:
    if p.tag == "true":
      return quote do:
        `destIdent`.addParLe(pool.tags.getOrIncl("true"), `infoIdent`)
        `destIdent`.addParRi(`infoIdent`)
    if p.tag == "false":
      return quote do:
        `destIdent`.addParLe(pool.tags.getOrIncl("false"), `infoIdent`)
        `destIdent`.addParRi(`infoIdent`)
    if p.tag in MetaTags or p.tag == "same":
      # `(any X)` etc. in RHS == splice X. `(pred …)` is rejected at
      # parse time (it lives in (when …), not in (rhs …)).
      if p.kids.len != 1 or p.kids[0].kind != pnIdent:
        error "(" & p.tag & " …) in RHS expects one identifier"
      let name = p.kids[0].name
      if name notin caps:
        error "RHS meta references unbound name: " & name
      let varIdent = ident(name)
      return quote do:
        `destIdent`.addSubtree(`varIdent`)
    # concrete tag
    let body = newStmtList()
    let tagLit = newLit(p.tag)
    body.add quote do:
      `destIdent`.addParLe(pool.tags.getOrIncl(`tagLit`), `infoIdent`)
    for k in p.kids:
      body.add emitRhs(k, caps, destIdent, infoIdent)
    body.add quote do:
      `destIdent`.addParRi(`infoIdent`)
    return body
  of pnIdent:
    if p.name notin caps:
      error "RHS references unbound name: " & p.name
    let varIdent = ident(p.name)
    return quote do:
      `destIdent`.addSubtree(`varIdent`)
  of pnIntLit:
    let v = newLit(p.ival)
    return quote do:
      `destIdent`.add intToken(pool.integers.getOrIncl(BiggestInt(`v`)), `infoIdent`)
  of pnDotToken:
    return quote do:
      `destIdent`.addDotToken()

# ---- Per-rule proc emission ----------------------------------------------

proc emitRuleProc(idx: int; rule: Rule;
                  walkerCtxType: NimNode): NimNode =
  ## Emits a `proc tryRule_<idx>(n: Cursor; ctx: var WalkCtx): bool`
  ## that returns true iff the rule matched and a substitution was
  ## recorded.
  let procName = ident("tryRule_" & $idx)
  let cIdent = ident("c")
  let okIdent = ident("ok")

  # Capture variable declarations.
  let captureDecls = newStmtList()
  for capName, _ in rule.captures:
    let v = ident(capName)
    captureDecls.add quote do:
      var `v` = default(Cursor)

  let lhsCode = emitTagMatch(rule.lhs, rule.captures, cIdent, okIdent)

  # Side conditions: each becomes a Nim proc call with capture Cursors
  # as arguments.
  let sideCondCheck = newStmtList()
  for sc in rule.sideConds:
    if sc.kind != pnTag or sc.kids.len == 0:
      error "(when …) entries must look like (predName cap1 cap2 …)"
    var call = newCall(ident(sc.tag))
    for arg in sc.kids:
      if arg.kind != pnIdent:
        error "(when …) call arguments must be captured names"
      if arg.name notin rule.captures:
        error "(when …) call references unbound name: " & arg.name
      call.add ident(arg.name)
    sideCondCheck.add quote do:
      if not `call`: return false

  let synthIdent = ident("synthBuf")
  let infoIdent = ident("info")
  let posIdent = ident("pos")
  let rhsCode = emitRhs(rule.rhs, rule.captures, synthIdent, infoIdent)
  let ctxIdent = ident("ctx")
  let nIdent = ident("n")

  # `-d:rewriterDebug` makes each rule announce when it fires, naming the
  # rule index and its LHS root tag plus the buffer position it rewrote.
  let ruleLabel = newLit("rule " & $idx & " (" & rule.rootTag & ")")
  let debugNode =
    when defined(rewriterDebug):
      quote do:
        debugEcho "[rewriter] fired ", `ruleLabel`, " @pos ", `posIdent`
    else:
      newStmtList()

  result = quote do:
    proc `procName`(`nIdent`: Cursor; `ctxIdent`: var `walkerCtxType`): bool =
      `captureDecls`
      var `okIdent` = true
      var `cIdent` = `nIdent`
      `lhsCode`
      if not `okIdent`: return false
      `sideCondCheck`
      let `infoIdent` = `nIdent`.info
      var `synthIdent` = createTokenBuf(8)
      `rhsCode`
      `ctxIdent`.synth.add `synthIdent`
      let `posIdent` = cursorToPosition(`ctxIdent`.orig[], `nIdent`)
      `ctxIdent`.patchset.addSubst(`posIdent`, cursorAt(`ctxIdent`.synth[^1], 0))
      `debugNode`
      `ctxIdent`.fired = true
      return true

# ---- Dispatch + walker emission -------------------------------------------

proc tagEnumIdent(tag: string): NimNode =
  ## Map a NIF tag name to its `TagEnum` identifier (e.g. `"add"` →
  ## `AddTagId`). Tags must be in the predefined NIFC tag set; an
  ## unknown name yields an undeclared-identifier error at the macro's
  ## call site.
  result = ident(capitalizeAscii(tag) & "TagId")

proc emitDispatch(rules: seq[Rule]; ctxType: NimNode): NimNode =
  ## Emits a `tryRulesAt` proc that switches directly on `n.tagEnum`
  ## with one `of <Tag>TagId:` per distinct LHS root tag. The compiler
  ## turns this into a jump table over the (small, dense) ordinal range
  ## of the matched tags. No runtime lookup table needed.
  let nIdent = ident("n")
  let ctxIdent = ident("ctx")

  var groups: OrderedTable[string, seq[int]] = initOrderedTable[string, seq[int]]()
  for i, r in rules:
    if r.rootTag notin groups:
      groups[r.rootTag] = @[]
    groups[r.rootTag].add i

  var caseStmt = nnkCaseStmt.newTree(newDotExpr(nIdent, ident("tagEnum")))
  for tag, idxs in groups:
    let branch = newStmtList()
    for i in idxs:
      let p = ident("tryRule_" & $i)
      branch.add quote do:
        if `p`(`nIdent`, `ctxIdent`): return true
    branch.add (quote do: return false)
    var ofBranch = nnkOfBranch.newTree(tagEnumIdent(tag))
    ofBranch.add branch
    caseStmt.add ofBranch
  var elseBranch = nnkElse.newTree()
  elseBranch.add (quote do: return false)
  caseStmt.add elseBranch

  let body = newStmtList()
  body.add quote do:
    if `nIdent`.kind != ParLe: return false
  body.add caseStmt

  result = newProc(
    name = ident("tryRulesAt"),
    params = @[
      ident("bool"),
      newIdentDefs(nIdent, ident("Cursor")),
      newIdentDefs(ctxIdent, nnkVarTy.newTree(ctxType))
    ],
    body = body)

# ---- Public macro ---------------------------------------------------------

macro genRewriter*(rulesSrc: static[string]): untyped =
  ## Translate the NIF rule source `rulesSrc` to Nim. Emits:
  ##
  ## - one private `tryRule_<n>` per rule,
  ## - a private `tryRulesAt` dispatch proc,
  ## - the public `runRewrites*(buf: var TokenBuf): bool` and
  ##   `runRewritesFix*(buf: var TokenBuf; maxIter = 32): int`.
  ##
  ## Predicates referenced from `(when (predName cap …))` must be in
  ## scope at the macro call site.
  let parsed = parseRuleFile(rulesSrc)
  if parsed.len == 0:
    error "no rules found in source"

  var rules: seq[Rule] = @[]
  for n in parsed:
    rules.add analyzeRule(n)

  result = newStmtList()

  # Local WalkCtx type used by the generated procs.
  let ctxType = ident("RewriterWalkCtx")
  result.add quote do:
    type `ctxType` = object
      orig: ptr TokenBuf
      patchset: Patchset
      synth: seq[TokenBuf]
      fired: bool

  for i, r in rules:
    result.add emitRuleProc(i, r, ctxType)

  result.add emitDispatch(rules, ctxType)

  let walkName = ident("rewriterWalk")
  let ctxIdent = ident("ctx")
  let nIdent = ident("n")
  result.add quote do:
    proc `walkName`(`nIdent`: var Cursor; `ctxIdent`: var `ctxType`) =
      case `nIdent`.kind
      of ParLe:
        let savedHere = `nIdent`
        `nIdent`.into:
          while `nIdent`.hasMore:
            `walkName`(`nIdent`, `ctxIdent`)
        discard tryRulesAt(savedHere, `ctxIdent`)
      else:
        inc `nIdent`

  result.add quote do:
    proc runRewrites*(buf: var TokenBuf): bool {.discardable.} =
      ## One bottom-up pass. Returns true iff at least one rule fired.
      var ctx = `ctxType`(
        orig: addr buf,
        patchset: initPatchset(addr buf),
        synth: @[],
        fired: false)
      var n = beginRead(buf)
      while n.hasMore:
        `walkName`(n, ctx)
      if ctx.fired and not ctx.patchset.isEmpty:
        var newBuf = ctx.patchset.apply()
        buf = ensureMove(newBuf)
      result = ctx.fired

    proc runRewritesFix*(buf: var TokenBuf; maxIter = 32): int {.discardable.} =
      ## Iterate `runRewrites` until stable or `maxIter` passes reached.
      result = 0
      for i in 0 ..< maxIter:
        if not runRewrites(buf): return
        inc result

# ---- Self-tests -----------------------------------------------------------

when isMainModule:
  import nifrender

  proc parse(src: string): TokenBuf =
    var stream = nifstreams.openFromBuffer(src, "M")
    result = fromStream(stream)

  # Seed input symbols.
  discard pool.syms.getOrIncl("x.0.M")
  discard pool.syms.getOrIncl("y.0.M")
  discard pool.syms.getOrIncl("z.0.M")
  discard pool.syms.getOrIncl("use.0.M")
  discard pool.syms.getOrIncl("side.0.M")

  proc isPow2(c: Cursor): bool =
    if c.kind != IntLit: return false
    let v = pool.integers[c.intId]
    v > 0 and (v and (v - 1)) == 0

  genRewriter staticRead("rules/arith.rewrite.nif")

  block add_zero:
    var buf = parse("(stmts (asgn x.0.M (add (i 32) x.0.M 0)))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M x.0.M))""")

  block zero_add:
    var buf = parse("(stmts (asgn x.0.M (add (i 32) 0 y.0.M)))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M y.0.M))""")

  block mul_one:
    var buf = parse("(stmts (asgn x.0.M (mul (i 32) y.0.M 1)))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M y.0.M))""")

  block mul_zero_pure:
    var buf = parse("(stmts (asgn x.0.M (mul (i 32) y.0.M 0)))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M 0))""")

  block mul_zero_impure_blocked:
    var buf = parse("(stmts (asgn x.0.M (mul (i 32) (call side.0.M) 0)))")
    let before = render(buf)
    runRewrites(buf)
    assertRender(buf, before)

  block and_true:
    var buf = parse("(stmts (asgn x.0.M (and (true) y.0.M)))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M y.0.M))""")

  block not_not:
    var buf = parse("(stmts (asgn x.0.M (not (not y.0.M))))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M y.0.M))""")

  block deref_addr:
    var buf = parse("(stmts (asgn x.0.M (deref (addr y.0.M))))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M y.0.M))""")

  block sub_same:
    var buf = parse("(stmts (asgn x.0.M (sub (i 32) y.0.M y.0.M)))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M 0))""")

  block fixpoint_chains:
    var buf = parse("(stmts (asgn x.0.M (and (true) (not (not y.0.M)))))")
    let passes = runRewritesFix(buf)
    doAssert passes >= 1
    assertRender(buf, """
(stmts
(asgn x.0.M y.0.M))""")

  block pow2_when_fires:
    var buf = parse("(stmts (asgn x.0.M (mul (i 32) y.0.M 4)))")
    runRewrites(buf)
    assertRender(buf, """
(stmts
(asgn x.0.M
(shl
(i 32)y.0.M 4)))""")

  block pow2_when_blocks:
    var buf = parse("(stmts (asgn x.0.M (mul (i 32) y.0.M 3)))")
    let before = render(buf)
    runRewrites(buf)
    assertRender(buf, before)

  echo "rewriter.nim: all self-tests passed"
