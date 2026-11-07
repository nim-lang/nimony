#
#
#           NIFC Copy Coalescing (nifcore)
#        (c) Copyprop's backward twin
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A temporary that is defined once and read once, by a copy into another
## local, is that local:
##
##   (var :t . T E) … (asgn d t)      →     (asgn d E) …
##
## Copy propagation runs the other way (it replaces later reads of `d` by `t`),
## and it cannot when `t` dies first: the inliner and xelim declare their temps
## in an inner scope and copy them out at its end —
##
##   (var :r . Vec3 .)
##   (scope (var :x`h . (f 64) (sub …)) … (asgn r.x x`h) …)
##
## — so `x`h` is gone before `r.x` is read and every such value costs a second
## register and a move. Here the store of `E` goes straight into `d`.
##
## Legal when, between `t`'s definition and the copy, in program order:
##  * nothing mentions `d` (no read, write or declaration of it) — so `d` holds
##    the same value at the copy whichever of the two places stores it, and its
##    declaration is visible at the definition;
##  * every statement is straight-line: no `jmp`, `lab`, `ret`, `break` or
##    `raise` anywhere in it, and the copy is reached only through `scope` and
##    `stmts` — never under an `if`, a `case` or a loop the definition is not
##    in. So the copy runs exactly when the definition did, once per time it did.
## And `t` has exactly one definition (its initializer, or one `asgn` with the
## declaration uninitialised) and one read (the copy); neither `t` nor `d` is
## address-taken; and both are declared with the same type.

import std / [assertions, tables, sets]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "models" / tags         # tag ids for synthesis

include ".." / ".." / "lib" / compat2         # getOrQuit (host Nim)

const MaxRounds = 8

type
  SymFacts = object
    declPos: int             ## its `var`, -1 when not a local of this body
    declInit: bool           ## declared with an initializer
    asgnDefs: seq[int]       ## positions of `(asgn s …)`
    reads: int               ## occurrences other than as an `asgn` target / decl
    addrTaken: bool
    declTwice: bool

  Pair = object
    t, d: SymId
    defPos: int              ## the `var` (with init) or the `asgn` defining `t`
    usePos: int              ## the `(asgn d t)`
    tDecl: int               ## `t`'s uninitialised decl when defPos is an asgn, else -1

  Context = object
    buf: ptr TokenBuf
    facts: Table[SymId, SymFacts]
    jumps: Table[SymId, int]  ## jumps per label in the whole body

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc pos(c: Context; n: Cursor): int {.inline.} = cursorToPosition(c.buf[], n)
proc at(c: Context; p: int): Cursor {.inline.} = cursorAt(c.buf[], p)

proc spanEnd(c: Context; p: int): int =
  var n = c.at(p)
  skip n
  result = c.pos(n)

proc fact(c: var Context; s: SymId): var SymFacts =
  if s notin c.facts:
    c.facts[s] = SymFacts(declPos: -1)
  result = c.facts.getOrQuit(s)

# ── facts ────────────────────────────────────────────────────────────────────

proc slotRoot(n: Cursor): SymId =
  var cur = n
  while true:
    case cur.kind
    of Symbol: return cur.symId
    of TagLit:
      if cur.exprKind in {DotC, AtC}: cur = child0(cur)
      else: return SymId(0)
    else: return SymId(0)

proc collect(c: var Context; n: Cursor) =
  case n.kind
  of Symbol:
    let reads = c.fact(n.symId).reads + 1   # `fact` may insert: not inside the store
    c.fact(n.symId).reads = reads
  of TagLit:
    case n.stmtKind
    of JmpS:
      let t = child0(n)
      if t.kind == Symbol: c.jumps.mgetOrPut(t.symId, 0) += 1
      return
    of LabS:
      return
    of VarS:
      var it = child0(n)
      if it.kind == SymbolDef:
        let s = it.symId
        if c.fact(s).declPos >= 0: c.fact(s).declTwice = true
        c.fact(s).declPos = c.pos(n)
        skip it                                  # name
        skip it                                  # pragmas
        skip it                                  # type
        c.fact(s).declInit = it.kind != DotToken
        if it.kind != DotToken: collect(c, it)
        return
    of AsgnS:
      let lhs = child0(n)
      var rhs = lhs
      skip rhs
      if lhs.kind == Symbol:
        c.fact(lhs.symId).asgnDefs.add c.pos(n)
      else:
        collect(c, lhs)
      collect(c, rhs)
      return
    else: discard
    if n.exprKind in {AddrC, HaddrC}:
      let r = slotRoot(child0(n))
      if r != SymId(0): c.fact(r).addrTaken = true
    var it = n
    it.loopInto:
      collect(c, it)
      skip it
  else: discard

# ── legality ─────────────────────────────────────────────────────────────────

proc declType(c: Context; declPos: int): Cursor =
  result = child0(c.at(declPos))
  skip result                                    # name
  skip result                                    # pragmas

proc scanStmt(n: Cursor; d: SymId; labels: var HashSet[SymId];
              jumps: var Table[SymId, int]): bool =
  case n.kind
  of Symbol, SymbolDef: result = n.symId != d
  of TagLit:
    case n.stmtKind
    of RetS, BreakS, RaiseS, JtrueS: return false
    of JmpS:
      let t = child0(n)
      if t.kind != Symbol: return false
      jumps.mgetOrPut(t.symId, 0) += 1
      return true
    of LabS:
      let l = child0(n)
      if l.kind != SymbolDef: return false
      labels.incl l.symId
      return true
    else: discard
    result = true
    var it = n
    it.loopInto:
      if not scanStmt(it, d, labels, jumps): return false
      skip it
  else: result = true

proc cleanStmt(c: Context; n: Cursor; d: SymId): bool =
  ## A complete statement between the definition and the copy: it does not
  ## mention `d`, and control flow stays inside it — every jump in it targets a
  ## label in it, and every jump to one of its labels comes from it.
  var labels = initHashSet[SymId]()
  var jumps = initTable[SymId, int]()
  if not scanStmt(n, d, labels, jumps): return false
  for l, k in jumps:
    if l notin labels: return false
  for l in labels:
    if c.jumps.getOrDefault(l, 0) != jumps.getOrDefault(l, 0): return false
  result = true

proc straightLine(c: Context; fromPos, toPos: int; d: SymId): bool =
  ## Tokens in `[fromPos, toPos)`: complete subtrees must be clean; a tag that
  ## is still open at `toPos` (an ancestor of the copy) must be a `scope`/`stmts`.
  var p = fromPos
  while p < toPos:
    let n = c.at(p)
    if n.kind == TagLit:
      let e = c.spanEnd(p)
      if e <= toPos:
        if not c.cleanStmt(n, d): return false
        p = e
      else:
        if n.stmtKind notin {ScopeS, StmtsS}: return false
        inc p
    else:
      if n.kind in {Symbol, SymbolDef} and n.symId == d: return false
      inc p
  result = true

proc findPairs(c: var Context; n: Cursor; pairs: var seq[Pair]; used: var HashSet[SymId]) =
  if n.kind != TagLit: return
  if n.stmtKind == AsgnS:
    let lhs = child0(n)
    var rhs = lhs
    skip rhs
    if lhs.kind == Symbol and rhs.kind == Symbol and lhs.symId != rhs.symId:
      let d = lhs.symId
      let t = rhs.symId
      if d notin used and t notin used and t in c.facts and d in c.facts:
        let ft = c.facts.getOrQuit(t)
        let fd = c.facts.getOrQuit(d)
        block check:
          if ft.declPos < 0 or fd.declPos < 0: break check
          if ft.declTwice or fd.declTwice or ft.addrTaken or fd.addrTaken: break check
          if ft.reads != 1: break check
          var defPos = -1
          var tDecl = -1
          if ft.declInit:
            if ft.asgnDefs.len != 0: break check
            defPos = ft.declPos
          else:
            if ft.asgnDefs.len != 1: break check
            defPos = ft.asgnDefs[0]
            tDecl = ft.declPos
          let usePos = c.pos(n)
          if defPos >= usePos or fd.declPos >= defPos: break check
          if not sameTree(c.declType(ft.declPos), c.declType(fd.declPos)): break check
          if not straightLine(c, c.spanEnd(defPos), usePos, d): break check
          # the definition itself must be plain (a value that reads `d` is fine:
          # `d` still holds the same value there)
          if not c.cleanStmt(c.at(defPos), SymId(0)): break check
          pairs.add Pair(t: t, d: d, defPos: defPos, usePos: usePos, tDecl: tDecl)
          used.incl t
          used.incl d
    return
  var it = n
  it.loopInto:
    findPairs(c, it, pairs, used)
    skip it

# ── rewriting ────────────────────────────────────────────────────────────────

type Edit = object
  kind: int                  ## 0 drop, 1 def → (asgn d VALUE)
  d: SymId

proc emit(c: Context; dest: var TokenBuf; n: Cursor; edits: Table[int, Edit]) =
  if n.kind != TagLit:
    dest.addSubtree n
    return
  let p = c.pos(n)
  if p in edits:
    let e = edits.getOrQuit(p)
    if e.kind == 0: return
    # the definition: `(var :t pragmas T VALUE)` or `(asgn t VALUE)`
    var v = child0(n)
    if n.stmtKind == VarS:
      skip v; skip v; skip v                     # name, pragmas, type
    else:
      skip v                                     # target
    let li = rawLineInfo(n)
    dest.openTag TagId(ord(AsgnTagId))
    if li.isValid: dest.appendLineInfo li
    dest.addSymUse e.d
    emit(c, dest, v, edits)
    dest.closeTag()
    return
  let li = rawLineInfo(n)
  dest.openTag n.cursorTagId
  if li.isValid: dest.appendLineInfo li
  var it = n
  it.loopInto:
    emit(c, dest, it, edits)
    skip it
  dest.closeTag()

proc runCoalesce*(buf: var TokenBuf): int {.discardable.} =
  ## Coalesce single-use copy temps into their destinations. Returns the number
  ## of temps removed.
  result = 0
  for round in 0 ..< MaxRounds:
    var c = Context(buf: addr buf, facts: initTable[SymId, SymFacts](),
                    jumps: initTable[SymId, int]())
    collect(c, beginRead(buf))
    var pairs: seq[Pair] = @[]
    var used = initHashSet[SymId]()
    findPairs(c, beginRead(buf), pairs, used)
    if pairs.len == 0: break
    var edits = initTable[int, Edit]()
    for p in pairs:
      edits[p.defPos] = Edit(kind: 1, d: p.d)
      edits[p.usePos] = Edit(kind: 0)
      if p.tDecl >= 0: edits[p.tDecl] = Edit(kind: 0)
    var dest = createTokenBuf(buf.len, buf.pool, buf.tags)
    emit(c, dest, beginRead(buf), edits)
    buf = ensureMove(dest)
    result += pairs.len

# ── self-tests ───────────────────────────────────────────────────────────────

when isMainModule:
  import std / strutils

  proc run(src: string): string =
    var b = parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())
    discard runCoalesce(b)
    result = toString(b).multiReplace(("\n", ""), (" ", ""))

  proc canon(src: string): string =
    var b = parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())
    result = toString(b).multiReplace(("\n", ""), (" ", ""))

  proc unchanged(src: string): bool = run(src) == canon(src)

  block inlined_result_chain:
    let got = run(
      "(stmts (var :r.0 . (f 64) .) (var :s.0 . (f 64) .) " &
      "(scope (var :x.0 . (f 64) (sub (f 64) a.0 b.0)) " &
      "(scope (stmts (asgn s.0 x.0) (jmp R.0)) (lab :R.0))) " &
      "(asgn r.0 s.0) (call use.0 r.0))")
    # `x` -> `s` in round one, `s` -> `r` in round two
    doAssert got == canon("(stmts (var :r.0 . (f 64) .) " &
      "(scope (asgn r.0 (sub (f 64) a.0 b.0)) " &
      "(scope (stmts (jmp R.0)) (lab :R.0))) (call use.0 r.0))"), got

  block d_read_between:
    doAssert unchanged("(stmts (var :d.0 . (f 64) 1.0) (var :t.0 . (f 64) a.0) " &
      "(call use.0 d.0) (asgn d.0 t.0) (call use.0 d.0))")

  block jump_between:
    doAssert unchanged("(stmts (var :d.0 . (f 64) 1.0) (var :t.0 . (f 64) a.0) " &
      "(if (elif c.0 (stmts (jmp L.0)))) (asgn d.0 t.0) (lab :L.0) (call use.0 d.0))")

  block copy_under_if:
    doAssert unchanged("(stmts (var :d.0 . (f 64) 1.0) (var :t.0 . (f 64) a.0) " &
      "(if (elif c.0 (stmts (asgn d.0 t.0)))) (call use.0 d.0))")

  block copy_in_loop:
    doAssert unchanged("(stmts (var :d.0 . (f 64) 1.0) (var :t.0 . (f 64) a.0) " &
      "(while (true) (stmts (asgn d.0 t.0) (call use.0 d.0) (asgn d.0 2.0))))")

  block t_read_twice:
    doAssert unchanged("(stmts (var :d.0 . (f 64) .) (var :t.0 . (f 64) a.0) " &
      "(asgn d.0 t.0) (call use.0 t.0 d.0))")

  block different_types:
    doAssert unchanged("(stmts (var :d.0 . (i 64) .) (var :t.0 . (i 32) a.0) " &
      "(asgn d.0 t.0) (call use.0 d.0))")

  block value_reads_d:
    let got = run("(stmts (var :d.0 . (f 64) 1.0) (var :t.0 . (f 64) (add (f 64) d.0 1.0)) " &
      "(asgn d.0 t.0) (call use.0 d.0))")
    doAssert got == canon("(stmts (var :d.0 . (f 64) 1.0) (asgn d.0 (add (f 64) d.0 1.0)) " &
      "(call use.0 d.0))"), got

  block d_declared_after_t:
    doAssert unchanged("(stmts (var :t.0 . (f 64) a.0) (var :d.0 . (f 64) .) " &
      "(asgn d.0 t.0) (call use.0 d.0))")

  echo "coalesce.nim: all self-tests passed"
