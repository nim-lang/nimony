#
#
#        Constant-branch pruning (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Remove `(if …)` branches whose `(elif COND BODY)` condition is the literal
## `(true)` or `(false)`. This is the second half of realizing the inliner's
## bet: the splice substitutes literal arguments into a callee body, the
## rewrite engine folds the conditions those literals feed (`(eq (nil) (nil))`
## → `(true)`, `(not (true))` → `(false)`), and THIS pass deletes the branches
## the folding decided — so the guarded-deref pattern
##
##   proc use(c: ptr T) = (if c != nil: …c.field…)   called as   use(nil)
##
## loses its dead `…(deref (nil)).field…` arm entirely instead of shipping
## ill-typed dead code to a backend. The C backend happens to swallow such
## arms; arkham's typed `getType` cannot, and it should not have to.
##
## Rewrites, per `(if …)` node (branches scanned in order):
##   - an elif with a `(false)` condition is dropped;
##   - the first elif with a `(true)` condition terminates the scan: it
##     becomes the `(else …)` of whatever kept branches precede it — or
##     replaces the whole `if` with its body when nothing precedes it;
##   - branches after a taken `(true)` elif (and any original `(else …)`
##     it shadows) are dead and dropped;
##   - an `if` whose every branch is dropped vanishes.
##
## The pass rebuilds the buffer only when something changed. It recurses into
## kept branch bodies, so nested constant `if`s prune in the same run.

import std / [assertions, tables]
import ".." / ".." / "lib" / nifcoreparse   # parse/serialize; re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "models" / tags         # TagEnum ordinals (ElseTagId)

proc condConst(branch: Cursor): int =
  ## -1 unknown, 0 `(false)`, 1 `(true)` for an `(elif COND BODY)`'s COND.
  ## A literal `(nil)` condition is pointer truthiness (`if p:` with `p`
  ## substituted by the inliner) and reads as false — only here, in condition
  ## position; `(nil)` anywhere else is a value and no rewrite may touch it.
  var cc = branch
  inc cc                                     # into the elif: at COND
  case cc.exprKind
  of TrueC: 1
  of FalseC, NilC: 0
  else: -1

type
  PruneCtx = object
    symUses: Table[SymId, int]     ## Symbol (use) occurrences, whole buffer

proc countSymUses(c: var Cursor; uses: var Table[SymId, int]) =
  case c.kind
  of Symbol:
    uses.mgetOrPut(c.symId, 0) += 1
    inc c
  of TagLit:
    c.into:
      while c.hasMore:
        countSymUses(c, uses)
  else:
    inc c

proc collectBranchLabels(c: var Cursor; localUses: var Table[SymId, int];
                         defs: var seq[SymId]) =
  case c.kind
  of Symbol:
    localUses.mgetOrPut(c.symId, 0) += 1
    inc c
  of TagLit:
    if c.stmtKind == LabS:
      var lc = c
      inc lc                                 # into the lab: at the symbol def
      if lc.kind == SymbolDef: defs.add lc.symId
    c.into:
      while c.hasMore:
        collectBranchLabels(c, localUses, defs)
  else:
    inc c

proc branchPinned(px: PruneCtx; branch: Cursor): bool =
  ## Does the branch define a `(lab :name)` some OUTSIDE code jumps to? Such a
  ## branch is reachable however its condition folds: hexer's try/except
  ## lowering parks the handler in an `(elif (false) (stmts (lab :`exlab.N)
  ## …))` entered only via `(jmp …)` from the try body, so "condition is
  ## (false)" does NOT mean "dead" for it. A label whose every use sits INSIDE
  ## the branch (the inliner's own returnLabel: `(jmp L)` + trailing
  ## `(lab :L)` in the same spliced body) pins nothing — the branch takes the
  ## label and its jumps with it. Compares the branch's own use counts against
  ## the buffer-wide ones collected up front.
  var localUses = initTable[SymId, int]()
  var defs: seq[SymId] = @[]
  var c = branch
  collectBranchLabels(c, localUses, defs)
  for L in defs:
    if px.symUses.getOrDefault(L, 0) > localUses.getOrDefault(L, 0):
      return true                            # someone outside jumps in
  false

proc pruneTree(px: PruneCtx; dest: var TokenBuf; n: var Cursor; changed: var bool)

proc emitBranchBody(px: PruneCtx; dest: var TokenBuf; branch: Cursor; isElif: bool;
                    changed: var bool) =
  ## Emit the BODY of an `(elif COND BODY)` / `(else BODY)`, pruned.
  var b = branch
  inc b                                      # into the branch
  if isElif: skip b                          # past COND
  px.pruneTree(dest, b, changed)

proc pruneIf(px: PruneCtx; dest: var TokenBuf; n: var Cursor; changed: var bool) =
  ## `n` is at an `(if …)`; emit its pruned form and advance `n` past it.
  # Scan the branches first; decide, then emit.
  var kept: seq[Cursor] = @[]                # elifs with unknown conditions
  var taken = default(Cursor)                # first (true) elif, or the else
  var takenIsElif = false
  var haveTaken = false
  var dropped = false                        # anything decided at all?
  var bailout = false                        # a to-be-dropped branch defines a label
  var probe = n
  probe.into:
    while probe.hasMore:
      let sk = probe.substructureKind
      if haveTaken:
        # Dead branch after a taken one — droppable only when no outside code
        # jumps into it (see `branchPinned`).
        if px.branchPinned(probe): bailout = true
        dropped = true
      elif sk == ElifU:
        case condConst(probe)
        of 1: (taken = probe; takenIsElif = true; haveTaken = true; dropped = true)
        of 0:
          if px.branchPinned(probe): bailout = true
          dropped = true
        else: kept.add probe
      elif sk == ElseU:
        taken = probe; takenIsElif = false; haveTaken = true
      else:
        kept.add probe                       # unexpected shape: keep verbatim
      skip probe

  if bailout or not dropped:
    # Nothing decided at this level: keep the `if`, but still recurse into
    # branch bodies (they may contain prunable ifs).
    let tag = n.cursorTagId
    let li = rawLineInfo(n)
    dest.openTag tag
    if li.isValid: dest.appendLineInfo li
    n.into:
      while n.hasMore:
        px.pruneTree(dest, n, changed)
    dest.closeTag()
    return

  changed = true
  if kept.len == 0:
    # No undecided elifs before the taken branch: the whole `if` collapses to
    # the taken branch's body (or to nothing when every branch was dropped).
    if haveTaken:
      px.emitBranchBody(dest, taken, takenIsElif, changed)
    skip n
    return

  # Some undecided elifs survive: rebuild the `if` from them, the taken
  # `(true)` elif (if any) demoted to the terminal `(else …)`.
  let tag = n.cursorTagId
  let li = rawLineInfo(n)
  dest.openTag tag
  if li.isValid: dest.appendLineInfo li
  for b in kept:
    let btag = cursorTagId(b)
    let bli = rawLineInfo(b)
    dest.openTag btag
    if bli.isValid: dest.appendLineInfo bli
    var bc = b
    bc.into:
      while bc.hasMore:
        px.pruneTree(dest, bc, changed)
    dest.closeTag()
  if haveTaken:
    # A taken `(true)` elif behind undecided ones IS the else branch now; an
    # original else keeps its tag.
    let btag = if takenIsElif: TagId(ord(ElseTagId)) else: cursorTagId(taken)
    dest.openTag btag
    let tli = rawLineInfo(taken)
    if tli.isValid: dest.appendLineInfo tli
    px.emitBranchBody(dest, taken, takenIsElif, changed)
    dest.closeTag()
  dest.closeTag()
  skip n

proc hasAnyDef(c: var Cursor): bool =
  ## Any SymbolDef in the subtree: a `(lab :L)` someone may jump to, or a
  ## `(var :v …)` declaration later reachable code may reference. Either makes
  ## a dead statement unsafe to drop.
  case c.kind
  of SymbolDef:
    inc c
    true
  of TagLit:
    var found = false
    c.into:
      while c.hasMore:
        if hasAnyDef(c): found = true
    found
  else:
    inc c
    false

proc hasAnyLabelDef(px: PruneCtx; c: Cursor): bool =
  var cc = c
  hasAnyDef(cc)

proc pruneStmtList(px: PruneCtx; dest: var TokenBuf; n: var Cursor;
                   changed: var bool) =
  ## Walk a `(stmts …)`/`(scope …)` child list dropping UNREACHABLE
  ## statements: after an unconditional `(jmp …)`/`(ret …)`, nothing executes
  ## until the next `(lab …)`, so label-free statements in between are dead.
  ## The value-splice epilogue produces exactly this — the callee's every path
  ## returns via `(asgn dest X) (jmp RL)`, leaving the trailing
  ## `dest = result` self-copy dead with `result` never written — and a typed
  ## backend verifier rightly rejects the dead read (the C backend happens to
  ## swallow it). A statement that contains a label anywhere is kept and ends
  ## the dead region (something can jump into it and fall out of it).
  let tag = n.cursorTagId
  let li = rawLineInfo(n)
  dest.openTag tag
  if li.isValid: dest.appendLineInfo li
  var unreachable = false
  n.into:
    while n.hasMore:
      let sk = n.stmtKind
      if sk == LabS:
        unreachable = false
        dest.addSubtree n
        skip n
      elif unreachable and not px.hasAnyLabelDef(n):
        changed = true
        skip n                               # dead: drop
      else:
        if unreachable: unreachable = false  # kept label-carrier resumes flow
        px.pruneTree(dest, n, changed)
        if sk in {JmpS, RetS}: unreachable = true
  dest.closeTag()

proc pruneTree(px: PruneCtx; dest: var TokenBuf; n: var Cursor; changed: var bool) =
  case n.kind
  of TagLit:
    case n.stmtKind
    of IfS:
      px.pruneIf(dest, n, changed)
    of StmtsS, ScopeS:
      px.pruneStmtList(dest, n, changed)
    else:
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      n.into:
        while n.hasMore:
          px.pruneTree(dest, n, changed)
      dest.closeTag()
  else:
    dest.addSubtree n
    inc n

proc runBranchPrune*(buf: var TokenBuf): bool =
  ## Prune constant branches everywhere in `buf`. Returns true (and replaces
  ## `buf`) when something was pruned; leaves `buf` untouched otherwise.
  var px = PruneCtx(symUses: initTable[SymId, int]())
  block:
    var n = buf.beginRead()
    while n.hasMore:
      countSymUses(n, px.symUses)
  var changed = false
  var dest = createTokenBuf(buf.len, buf.pool, buf.tags)
  var n = buf.beginRead()
  while n.hasMore:
    px.pruneTree(dest, n, changed)
  result = changed
  if changed:
    buf = ensureMove(dest)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc runOn(s: string): (string, bool) =
    var b = parseFromBuffer(s, "t", 100, sharedTags = createLengTagPool())
    let ch = runBranchPrune(b)
    (toString(b), ch)

  proc expectPrune(input, expected: string) =
    let (got, ch) = runOn(input)
    var e = parseFromBuffer(expected, "t", 100, sharedTags = createLengTagPool())
    let want = toString(e)
    doAssert ch, "expected a prune for: " & input
    doAssert got == want, "prune MISMATCH\n  got:  " & got & "\n  want: " & want

  proc expectNoChange(input: string) =
    let (_, ch) = runOn(input)
    doAssert not ch, "unexpected prune for: " & input

  # false elif + else → else body.
  expectPrune("(stmts (if (elif (false) (stmts (call foo.0))) (else (stmts (call bar.0)))))",
              "(stmts (stmts (call bar.0)))")
  # true elif first → its body; trailing branches dead.
  expectPrune("(stmts (if (elif (true) (stmts (call foo.0))) (else (stmts (call bar.0)))))",
              "(stmts (stmts (call foo.0)))")
  # false elif, no else → if vanishes.
  expectPrune("(stmts (if (elif (false) (stmts (call foo.0)))))",
              "(stmts)")
  # unknown elif kept, true elif behind it demotes to else.
  expectPrune("(stmts (if (elif x.0 (stmts (call a.0))) (elif (true) (stmts (call b.0))) (else (stmts (call c.0)))))",
              "(stmts (if (elif x.0 (stmts (call a.0))) (else (stmts (call b.0)))))")
  # unknown-only conditions: untouched.
  expectNoChange("(stmts (if (elif x.0 (stmts (call a.0))) (else (stmts (call b.0)))))")
  # hexer's try/except shape: a (false) elif holding a label the try body
  # jumps into stays untouched.
  expectNoChange("(stmts (jmp L.0) (if (elif (false) (stmts (lab :L.0) (call h.0)))))")
  # a dead branch AFTER a taken (true) elif likewise pins the if when outside
  # code jumps into it.
  expectNoChange("(stmts (jmp L.1) (if (elif (true) (stmts (call a.0))) (else (stmts (lab :L.1) (call h.0)))))")
  # a label whose jumps all sit INSIDE the dropped branch (the inliner's own
  # returnLabel) pins nothing.
  expectPrune("(stmts (if (elif (false) (stmts (jmp L.2) (lab :L.2)))))",
              "(stmts)")
  # a nil condition reads as false (pointer truthiness after substitution).
  expectPrune("(stmts (if (elif (nil) (stmts (call a.0))) (else (stmts (call b.0)))))",
              "(stmts (stmts (call b.0)))")
  # unreachable statements between a jmp and its label are dropped — the
  # value-splice's dead `dest = result` self-copy.
  expectPrune("(stmts (jmp RL.0) (asgn d.0 result.9) (lab :RL.0) (call u.0 d.0))",
              "(stmts (jmp RL.0) (lab :RL.0) (call u.0 d.0))")
  # …but a declaration in the dead region stays (later code may use the name),
  # and a label-carrying statement resumes reachability.
  expectNoChange("(stmts (jmp RL.1) (var :v.0 . (i 64).) (lab :RL.1) (asgn v.0 1))")
  # nested: inner if prunes inside a kept branch.
  expectPrune("(stmts (if (elif x.0 (stmts (if (elif (false) (stmts (call a.0))))))))",
              "(stmts (if (elif x.0 (stmts))))")
  echo "branchprune self-tests passed"
