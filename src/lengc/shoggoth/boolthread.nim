#
#
#           NIFC Bool Jump Threading (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Threads the bool-accumulator diamonds hexer's short-circuit lowering leaves
## behind. `if a or b: A else: B` reaches NIFC as
##
##   (var :x (bool) .)
##   (if (elif COND1 (asgn x (true)))
##       (else … (asgn x COND2)))          ; every path tail-assigns x
##   (if (elif x A) (else B))              ; the ONLY read of x
##
## gcc's jump threading dissolves this; arkham compiles it literally, paying a
## `setcc`/`mov $1`, a register for `x`, and a `test`+`jcc` per evaluation — in
## a lexer's per-char loop that is the whole profile. This pass rewrites the
## diamond into the control flow gcc would produce:
##
##   (if (elif COND1 (jmp LT))
##       (else … (if (elif COND2 (stmts (jmp LT))) (else (stmts (jmp LF))))))
##   (lab :LT) A (jmp LE)
##   (lab :LF) B
##   (lab :LE)
##
## and deletes `x` outright. Conditions that only became bare bool copies after
## `copyprop` ran are caught by the fixpoint driver: threading the outer
## accumulator turns the inner one's read into an `if` condition, so chained
## `or`/`and` chains unravel one round at a time.
##
## The transformation is applied only when it is provably total:
##  * `x` is a local declared `(var :x . (bool) .)` — no initializer;
##  * every write is an `(asgn x …)` in TAIL position of the single statement
##    `W` preceding the reading `if` (through `stmts`/`scope`/`if`/`case`
##    nesting; `if`/`case` need an `else`, and a branch may instead end in
##    `jmp`/`ret`, which never reaches the read);
##  * `x` has exactly one read: the bare condition of that adjacent
##    `(if (elif x THEN) [(else ELSE)])` with a single `elif`.
## Anything else — partial assignment, address-taking, multiple reads — fails
## the counts and the diamond is left alone.

import std / [assertions, tables]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/typeKind
import ".." / ".." / "models" / tags         # tag ids for synthesis
import patchsets

type
  Candidate = object
    sym: SymId
    declPos: int
    wPos, rPos: int
    asgnPositions: seq[int]     ## every tail `(asgn x …)`, as orig positions

  Context = object
    orig: ptr TokenBuf
    suffix: string
    counter: int
    uses, writes: Table[SymId, int]

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc countUses(c: Cursor; uses, writes: var Table[SymId, int]) =
  ## `uses` counts every Symbol token; `writes` counts `(asgn SYM …)` heads.
  var n = c
  case n.kind
  of Symbol:
    uses.mgetOrPut(symId(n), 0) += 1
  of TagLit:
    if n.stmtKind == AsgnS:
      let lhs = child0(n)
      if lhs.kind == Symbol:
        writes.mgetOrPut(symId(lhs), 0) += 1
    n.loopInto:
      countUses(n, uses, writes)
      skip n
  else:
    discard

proc isBoolDecl(n: Cursor; sym: var SymId): bool =
  ## `(var :x <pragmas> (bool) .)` — dot initializer, bool type.
  if n.kind != TagLit or n.stmtKind != VarS: return false
  result = false
  var c = n
  c.into:
    if c.hasMore and c.kind == SymbolDef:
      sym = symId(c)
      skip c                                   # name
      if c.hasMore: skip c                     # pragmas
      if c.hasMore and c.kind == TagLit and c.typeKind == BoolT:
        skip c                                 # type
        if c.hasMore and c.kind == DotToken:
          result = true
    while c.hasMore: skip c

proc isAsgnTo(n: Cursor; x: SymId): bool =
  if n.kind != TagLit or n.stmtKind != AsgnS: return false
  let lhs = child0(n)
  result = lhs.kind == Symbol and symId(lhs) == x

proc lastStmtOf(n: Cursor; last: var Cursor): bool =
  ## The last child statement of a `stmts`/`scope` body; false if empty.
  result = false
  var c = n
  c.into:
    while c.hasMore:
      last = c
      result = true
      skip c

proc tailAssigns(n: Cursor; x: SymId; found: var seq[int]; orig: ptr TokenBuf): bool =
  ## True iff every path through statement `n` ends in an `(asgn x …)` (all of
  ## them collected into `found`) or in a `jmp`/`ret` that never falls through.
  if n.kind != TagLit: return false
  case n.stmtKind
  of AsgnS:
    if isAsgnTo(n, x):
      found.add cursorToPosition(orig[], n)
      true
    else:
      false
  of JmpS, RetS:
    true                                        # this path never reaches the read
  of StmtsS, ScopeS:
    var last = default(Cursor)
    lastStmtOf(n, last) and tailAssigns(last, x, found, orig)
  of IfS:
    var ok = true
    var hasElse = false
    var c = n
    c.loopInto:
      case c.substructureKind
      of ElifU:
        var b = c
        var body = default(Cursor)
        var haveBody = false
        b.into:
          if b.hasMore: skip b                 # condition
          if b.hasMore:
            body = b
            haveBody = true
            skip b
          while b.hasMore: skip b
        if not haveBody or not tailAssigns(body, x, found, orig):
          ok = false
      of ElseU:
        hasElse = true
        var b = c
        var body = default(Cursor)
        var haveBody = false
        b.into:
          if b.hasMore:
            body = b
            haveBody = true
            skip b
          while b.hasMore: skip b
        if not haveBody or not tailAssigns(body, x, found, orig):
          ok = false
      else:
        ok = false
      skip c
    ok and hasElse
  of CaseS:
    var ok = true
    var hasElse = false
    var c = n
    c.into:
      if c.hasMore: skip c                     # selector
      while c.hasMore:
        case c.substructureKind
        of OfU:
          var b = c
          var body = default(Cursor)
          var haveBody = false
          b.into:
            if b.hasMore: skip b               # ranges
            if b.hasMore:
              body = b
              haveBody = true
              skip b
            while b.hasMore: skip b
          if not haveBody or not tailAssigns(body, x, found, orig):
            ok = false
        of ElseU:
          hasElse = true
          var b = c
          var body = default(Cursor)
          var haveBody = false
          b.into:
            if b.hasMore:
              body = b
              haveBody = true
              skip b
            while b.hasMore: skip b
          if not haveBody or not tailAssigns(body, x, found, orig):
            ok = false
        else:
          ok = false
        skip c
    ok and hasElse
  else:
    false

proc readingIf(n: Cursor; x: SymId): bool =
  ## `(if (elif x THEN) [(else ELSE)])` — single elif, bare `x` condition.
  if n.kind != TagLit or n.stmtKind != IfS: return false
  var elifs = 0
  var ok = true
  var c = n
  c.loopInto:
    case c.substructureKind
    of ElifU:
      inc elifs
      let cond = child0(c)
      if not (cond.kind == Symbol and symId(cond) == x):
        ok = false
    of ElseU:
      discard
    else:
      ok = false
    skip c
  ok and elifs == 1

proc subtreeSpan(orig: ptr TokenBuf; n: Cursor): (int, int) =
  let a = cursorToPosition(orig[], n)
  var e = n
  skip e
  (a, cursorToPosition(orig[], e))

# ---- candidate discovery ---------------------------------------------------

proc scanLists(c: var Context; n: Cursor; cands: var seq[Candidate]) =
  ## Visit every `stmts`/`scope` sibling list in the tree; append each
  ## qualifying decl/W/R triple. The caller applies the LAST one found — a
  ## flat `y`-feeds-`x` chain must thread `x` first, or `y`'s threaded label
  ## block no longer tail-assigns and the chain jams.
  if n.kind != TagLit: return
  if n.stmtKind in {StmtsS, ScopeS}:
    var decls = initTable[SymId, int]()        # sym -> decl position
    var prev = default(Cursor)
    var havePrev = false
    var m = n
    m.loopInto:
      var s = SymId(0)
      if isBoolDecl(m, s):
        decls[s] = cursorToPosition(c.orig[], m)
      elif m.kind == TagLit and m.stmtKind == IfS and havePrev and prev.kind == TagLit:
        # Is this the reading if of some declared bool, with W right before?
        for s, declPos in decls:
          if readingIf(m, s) and c.uses.getOrDefault(s) == c.writes.getOrDefault(s) + 1:
            var tails: seq[int] = @[]
            if tailAssigns(prev, s, tails, c.orig) and
               tails.len == c.writes.getOrDefault(s) and tails.len >= 1:
              cands.add Candidate(sym: s, declPos: declPos,
                                  wPos: cursorToPosition(c.orig[], prev),
                                  rPos: cursorToPosition(c.orig[], m),
                                  asgnPositions: tails)
              break
      prev = m
      havePrev = true
      skip m
  # Recurse generically: nested lists live under if/case/while/loop branches,
  # and under the list's own composite statements.
  var it = n
  it.loopInto:
    scanLists(c, it, cands)
    skip it

# ---- rewrite ---------------------------------------------------------------

proc freshLabel(c: var Context): string =
  inc c.counter
  result = "`bt." & $c.counter & "." & c.suffix

proc synthJmp(c: var Context; dest: var TokenBuf; label: string) =
  dest.openTag TagId(ord(JmpTagId))
  dest.addSymUse label
  dest.closeTag()

proc synthLab(c: var Context; dest: var TokenBuf; label: string) =
  dest.openTag TagId(ord(LabTagId))
  dest.addSymDef label
  dest.closeTag()

proc applyCandidate(c: var Context; cand: Candidate): TokenBuf =
  let lt = freshLabel(c)
  let lf = freshLabel(c)
  let le = freshLabel(c)
  var synth: seq[TokenBuf] = @[]

  # decl -> deleted
  var dotBuf = createTokenBuf(2, c.orig[].pool, c.orig[].tags)
  dotBuf.addDotToken()

  # each tail asgn -> jmp / guarded jmp
  var patches: seq[(int, int)] = @[]   # (origPos, synth index); -1 = dotBuf
  for pos in cand.asgnPositions:
    var asgn = cursorAt(c.orig[], pos)
    # RHS is the 2nd child
    var rhs = child0(asgn)
    skip rhs                                    # past the LHS symbol
    var buf = createTokenBuf(16, c.orig[].pool, c.orig[].tags)
    if rhs.kind == TagLit and rhs.exprKind == TrueC:
      synthJmp(c, buf, lt)
    elif rhs.kind == TagLit and rhs.exprKind == FalseC:
      synthJmp(c, buf, lf)
    else:
      buf.openTag TagId(ord(IfTagId))
      buf.openTag TagId(ord(ElifTagId))
      buf.addSubtree rhs
      buf.openTag TagId(ord(StmtsTagId))
      synthJmp(c, buf, lt)
      buf.closeTag()
      buf.closeTag()
      buf.openTag TagId(ord(ElseTagId))
      buf.openTag TagId(ord(StmtsTagId))
      synthJmp(c, buf, lf)
      buf.closeTag()
      buf.closeTag()
      buf.closeTag()
    patches.add((pos, synth.len))
    synth.add ensureMove(buf)

  # the reading if -> label block
  var rbuf = createTokenBuf(64, c.orig[].pool, c.orig[].tags)
  block:
    var rif = cursorAt(c.orig[], cand.rPos)
    var thenBody = default(Cursor)
    var elseBody = default(Cursor)
    var haveThen = false
    var haveElse = false
    var it = rif
    it.loopInto:
      case it.substructureKind
      of ElifU:
        var b = it
        b.into:
          if b.hasMore: skip b                 # condition (the bare x)
          if b.hasMore:
            thenBody = b
            haveThen = true
            skip b
          while b.hasMore: skip b
      of ElseU:
        var b = it
        b.into:
          if b.hasMore:
            elseBody = b
            haveElse = true
            skip b
          while b.hasMore: skip b
      else: discard
      skip it
    rbuf.openTag TagId(ord(StmtsTagId))
    synthLab(c, rbuf, lt)
    if haveThen:
      rbuf.addSubtree thenBody
    if haveElse:
      synthJmp(c, rbuf, le)
      synthLab(c, rbuf, lf)
      rbuf.addSubtree elseBody
      synthLab(c, rbuf, le)
    else:
      synthLab(c, rbuf, lf)                    # false path: skip THEN; no else
    rbuf.closeTag()

  var ps = initPatchset(c.orig)
  ps.addSubst(cand.declPos, cursorAt(dotBuf, 0))
  for (pos, idx) in patches:
    ps.addSubst(pos, cursorAt(synth[idx], 0))
  ps.addSubst(cand.rPos, cursorAt(rbuf, 0))
  result = ps.apply()

# ---- public entry ----------------------------------------------------------

proc runBoolThread*(buf: var TokenBuf; suffix = "bt"): int {.discardable.} =
  ## Fixpoint: thread one diamond, rebuild, rescan — chained accumulators
  ## become candidates only after the outer one is threaded. Returns the
  ## number of diamonds threaded.
  result = 0
  var rounds = 0
  while rounds < 32:
    inc rounds
    var c = Context(orig: addr buf, suffix: suffix, counter: result * 3,
                    uses: initTable[SymId, int](), writes: initTable[SymId, int]())
    block:
      let root = beginRead(buf)
      countUses(root, c.uses, c.writes)
    var cands: seq[Candidate] = @[]
    block:
      let root = beginRead(buf)
      scanLists(c, root, cands)
    if cands.len == 0:
      break
    var newBuf = applyCandidate(c, cands[^1])
    buf = ensureMove(newBuf)
    inc result

# ---- self-tests ------------------------------------------------------------

when isMainModule:
  import std / strutils

  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc thread(src: string): string =
    var b = parse(src)
    runBoolThread(b, "t")
    toString(b)

  block simple_diamond:
    let got = thread(
      "(stmts (var :x.0.M . (bool) .) " &
      "(if (elif (call c1.0.M) (stmts (asgn x.0.M (true)))) " &
      "(else (stmts (asgn x.0.M (call c2.0.M))))) " &
      "(if (elif x.0.M (stmts (call a.0.M))) (else (stmts (call b.0.M)))))")
    doAssert "jmp" in got and "lab" in got and "x.0.M" notin got, got

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  block partial_assignment_left_alone:
    # no else on the writer if: a path reaches the read unassigned
    let src =
      "(stmts (var :x.0.M . (bool) .) " &
      "(if (elif (call c1.0.M) (stmts (asgn x.0.M (true))))) " &
      "(if (elif x.0.M (stmts (call a.0.M)))))"
    doAssert thread(src) == canon(src)

  block second_read_left_alone:
    let src =
      "(stmts (var :x.0.M . (bool) .) " &
      "(if (elif (call c1.0.M) (stmts (asgn x.0.M (true)))) " &
      "(else (stmts (asgn x.0.M (false))))) " &
      "(if (elif x.0.M (stmts (call a.0.M)))) " &
      "(call use.0.M x.0.M))"
    doAssert thread(src) == canon(src)

  block chained_accumulators:
    # inner bool feeds outer via copy: takes two rounds
    let got = thread(
      "(stmts (var :y.0.M . (bool) .) (var :x.0.M . (bool) .) " &
      "(if (elif (call c1.0.M) (stmts (asgn y.0.M (true)))) " &
      "(else (stmts (asgn y.0.M (call c2.0.M))))) " &
      "(if (elif y.0.M (stmts (asgn x.0.M (true)))) " &
      "(else (stmts (asgn x.0.M (false))))) " &
      "(if (elif x.0.M (stmts (call a.0.M))) (else (stmts (call b.0.M)))))")
    doAssert "x.0.M" notin got and "y.0.M" notin got, got

  echo "boolthread.nim: all self-tests passed"
