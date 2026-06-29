## Very simple copy propagation and dead store elimination as it might be the
## result of inlining.
##
## ```
## var x = f()
## var y = x
## use y
## -->
## var x = f()
## use x
## ```
##
## We do this transformation only for local variables that have its address not
## been taken. We can also only do this if `x` is not changed after `y` was bound
## to it. But if `y` is changed and then `x` is used after the binding the
## optimization is not correct either!
##
## And finally should `x` be used after `y` has been killed that is fine. Only the
## statements between the binding and its `kill` instruction matter.
##
## Dead store elimination is simpler: Every assignment such as `local = f()` or
## binding `let local = f()` is dead code if `local` is not used afterward.
##
## Implementation (mirrors `cse.nim`: nifcore cursors + `patchsets` for the
## rewrite, `trackers` for the flow-sensitive state across `if`/`case`/loop/
## `jmp`/`lab`).
##
## The whole pass rests on one observation the docstring makes: we restrict both
## the copied variable `y` and its source `x` to **local** variables whose
## **address is never taken**. Such a local cannot be mutated by a call or a
## through-pointer store — only by a direct assignment to it. That removes all the
## aliasing reasoning: the copy `y = x` stays valid until something assigns to `x`
## or to `y`.
##
## * A flow-sensitive `copyOf: SymId -> SymId` (a branch-aware `Tracker`) records
##   that `y` currently holds the same value as `x`. At a value-use of `y` we
##   rewrite the token to `x`. The source is resolved to its *root* at bind time,
##   so chains (`z = y = x`) collapse to `z -> x` directly.
## * An assignment to a symbol `s` (or a field/through-pointer store whose base is
##   `s`) invalidates `s` as a copy (its value changed) and every alias `k` with
##   `copyOf[k] == s` (the source moved). Calls invalidate nothing — see above.
## * **Dead store elimination** of the now-useless `var y = x` (and of any pure
##   leaf-initialised local that ends up unused) falls out for free: a local
##   binding to a side-effect-free leaf is a deletion candidate, and we delete it
##   iff *every* use of the bound symbol was rewritten away (`substituted == total
##   uses`). A surviving un-rewritten use — a reassignment LHS, an `addr` operand,
##   or a read past an invalidation — keeps `substituted < total`, so the decl
##   stays. The deleted decl is replaced by an empty `.` statement (a no-op in the
##   statement list).

import std / [tables, sets, hashes, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import trackers, patchsets

# ---- nifcore helpers ------------------------------------------------------

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

const LeafKinds = {Symbol, IntLit, UIntLit, FloatLit, CharLit, StrLit}
  ## Side-effect-free initializers a dead local binding may be deleted with.

# ---- context --------------------------------------------------------------

type
  Context = object
    orig: ptr TokenBuf
    copyOf: Tracker[SymId, SymId]      ## y -> x: y currently aliases x
    addrTaken: HashSet[SymId]          ## syms whose address is taken anywhere
    localDefs: HashSet[SymId]          ## syms declared by a local `(var …)`
    useCount: Table[SymId, int]        ## every `Symbol` (read) token per sym
    substCount: Table[SymId, int]      ## uses we rewrote away per sym
    names: Table[SymId, string]        ## sym -> its textual name (for synthesis)
    delCandidates: Table[SymId, int]   ## deletable local binding -> its decl pos
    patchset: Patchset
    synth: seq[TokenBuf]
    dotBuf: TokenBuf                   ## a single `.` token: replaces a dead decl

proc createContext(orig: ptr TokenBuf): Context =
  result = Context(orig: orig,
          copyOf: initTracker[SymId, SymId](),
          addrTaken: initHashSet[SymId](),
          localDefs: initHashSet[SymId](),
          useCount: initTable[SymId, int](),
          substCount: initTable[SymId, int](),
          names: initTable[SymId, string](),
          delCandidates: initTable[SymId, int](),
          patchset: initPatchset(orig),
          synth: @[],
          dotBuf: createTokenBuf(2, orig[].pool, orig[].tags))
  result.dotBuf.addDotToken()

proc resolve(c: Context; s: SymId): SymId =
  ## Chase `s` to the root of its copy chain. Bindings are stored flat (resolved
  ## at bind time) so this is usually a single step; the loop only guards against
  ## a degenerate chain.
  result = s
  var guard = 0
  while true:
    let nxt = c.copyOf[result]
    if nxt == SymId(0): break
    result = nxt
    inc guard
    if guard > 1_000_000: break

# ---- invalidation ---------------------------------------------------------

proc invalidate(c: var Context; s: SymId) =
  ## `s` was written: it is no longer a copy of anything, and every alias whose
  ## source is `s` is now stale.
  if s == SymId(0): return
  c.copyOf[s] = SymId(0)
  var toClear: seq[SymId] = @[]
  for k, v in c.copyOf.pairs:
    if v == s: toClear.add k
  for k in toClear:
    c.copyOf[k] = SymId(0)

# ---- branch-state forwarding (same surface as cse.nim) --------------------

proc openBranches(c: var Context) = c.copyOf.openBranches()
proc openBranch(c: var Context) = c.copyOf.openBranch()
proc openFinalBranch(c: var Context) = c.copyOf.openFinalBranch()
proc closeBranch(c: var Context) = c.copyOf.closeBranch()
proc closeBranches(c: var Context) = c.copyOf.closeBranches()
proc gotoLabel(c: var Context; L: LabelId) = c.copyOf.gotoLabel L
proc landLabel(c: var Context; L: LabelId) = c.copyOf.landLabel L
proc clearAll(c: var Context) = c.copyOf.clearAll()

# ---- substitution synthesis -----------------------------------------------

proc substituteSym(c: var Context; n: Cursor; root: SymId) =
  ## Rewrite the `Symbol` token at `n` to a bare use of `root`.
  let pos = cursorToPosition(c.orig[], n)
  let idx = c.synth.len
  var buf = createTokenBuf(2, c.orig[].pool, c.orig[].tags)
  buf.addSymUse c.names.getOrDefault(root)
  c.synth.add ensureMove(buf)
  c.patchset.addSubst(pos, cursorAt(c.synth[idx], 0))
  c.substCount.mgetOrPut(symId(n), 0) += 1

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward

proc trExpr(c: var Context; n: var Cursor) =
  case n.kind
  of Symbol:
    let s = symId(n)
    let root = resolve(c, s)
    if root != s:
      substituteSym(c, n, root)
    inc n
  of TagLit:
    case n.exprKind
    of AddrC:
      # `addr x`: the *address* of `x`, not its value. `&y` and `&x` differ even
      # when `y == x`, so never substitute inside an `addr`.
      skip n
    of CallC:
      n.into:
        if n.hasMore: skip n             # callee
        while n.hasMore: trExpr(c, n)    # args
      # A call cannot touch a non-addr-taken local, so nothing to invalidate.
    of DotC:
      # `(dot OBJ FIELD inheritance)`: only OBJ is a value expression. FIELD is a
      # field-selector symbol — NEVER a value read. A local var can share a field's
      # symbol name (e.g. a `f.0` temp alongside a `:f.0` object field), so blindly
      # substituting it would rewrite genuine field selectors and produce accesses
      # to a non-existent member.
      n.into:
        if n.hasMore: trExpr(c, n)       # object expression
        if n.hasMore: skip n             # field selector — leave untouched
        while n.hasMore: skip n          # inheritance depth (IntLit)
    else:
      if n.substructureKind == KvU:
        # `(kv FIELD VALUE)` inside an object constructor: FIELD is a field
        # selector (same hazard as in `dot`), only VALUE carries a value.
        n.into:
          if n.hasMore: skip n           # field selector — leave untouched
          while n.hasMore: trExpr(c, n)  # value(s)
      else:
        n.loopInto:
          trExpr(c, n)
  else:
    inc n

proc trVar(c: var Context; n: var Cursor) =
  let isLocal = n.stmtKind == VarS
  let defPos = cursorToPosition(c.orig[], n)
  var nameSym = SymId(0)
  n.into:
    if n.hasMore:
      if n.kind == SymbolDef: nameSym = symId(n)
      skip n                             # name
    if n.hasMore: skip n                 # pragmas
    if n.hasMore: skip n                 # type
    if n.hasMore:
      let initStart = n
      let initKind = initStart.kind
      let initSym = if initKind == Symbol: symId(initStart) else: SymId(0)
      trExpr(c, n)                       # propagate inside the initializer
      if isLocal and nameSym != SymId(0) and nameSym notin c.addrTaken:
        # Record the copy relation for a plain local-symbol initializer.
        if initSym != SymId(0) and initSym in c.localDefs and
           initSym notin c.addrTaken:
          c.copyOf[nameSym] = resolve(c, initSym)
        # Deletable if its initializer is a side-effect-free leaf: dead once all
        # its uses are rewritten away (copy) or it was never used (pure store).
        if initKind in LeafKinds:
          c.delCandidates[nameSym] = defPos
    while n.hasMore: skip n

proc trAsgn(c: var Context; n: var Cursor) =
  let isStore = n.stmtKind == StoreS
  var first, second = default(Cursor)
  var haveFirst, haveSecond = false
  n.into:
    if n.hasMore:
      first = n; haveFirst = true; skip n
    if n.hasMore:
      second = n; haveSecond = true; skip n
    while n.hasMore: skip n
  # `asgn` is `(asgn dest src)`; `store` is `(store src dest)` (reversed).
  let lhs = if isStore: second else: first
  let rhs = if isStore: first else: second
  if haveSecond:
    var r = rhs
    trExpr(c, r)                         # propagate inside the value
  # The write target's base symbol changed value — invalidate it (and its
  # aliases). We deliberately do NOT substitute inside the LHS: its base names a
  # storage location, and aliasing two locals' *values* does not alias their
  # storage.
  let haveLhs = if isStore: haveSecond else: haveFirst
  if haveLhs:
    invalidate(c, rootOf(lhs))

proc trCallStmt(c: var Context; n: var Cursor) =
  n.into:
    if n.hasMore: skip n                 # callee
    while n.hasMore: trExpr(c, n)
  # No invalidation: a call cannot mutate a non-addr-taken local.

proc trIf(c: var Context; n: var Cursor) =
  openBranches c
  n.loopInto:
    case n.substructureKind
    of ElifU:
      n.into:
        if n.hasMore: trExpr(c, n)       # condition
        openBranch c
        if n.hasMore: tr(c, n)           # body
        while n.hasMore: skip n
        closeBranch c
    of ElseU:
      n.into:
        openFinalBranch c
        if n.hasMore: tr(c, n)
        while n.hasMore: skip n
        closeBranch c
    else:
      skip n
  closeBranches c

proc trCase(c: var Context; n: var Cursor) =
  n.into:
    if n.hasMore: trExpr(c, n)           # selector
    openBranches c
    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          if n.hasMore: skip n           # ranges
          openBranch c
          if n.hasMore: tr(c, n)
          while n.hasMore: skip n
          closeBranch c
      of ElseU:
        n.into:
          openFinalBranch c
          if n.hasMore: tr(c, n)
          while n.hasMore: skip n
          closeBranch c
      else:
        skip n
    closeBranches c

proc collectWrites(start: Cursor; writes: var HashSet[SymId]) =
  ## Base symbol of every `asgn`/`store` target in the subtree at `start`.
  if not start.hasMore or start.kind != TagLit: return
  let sk = start.stmtKind
  if sk in {AsgnS, StoreS}:
    var lhs = child0(start)
    if sk == StoreS: skip lhs            # dest is the 2nd child
    let root = rootOf(lhs)
    if root != SymId(0): writes.incl root
  var n = start
  n.loopInto:
    collectWrites(n, writes)
    skip n

proc trLoopBody(c: var Context; n: var Cursor) =
  case n.stmtKind
  of WhileS:
    n.into:
      if n.hasMore: trExpr(c, n)         # condition
      if n.hasMore: tr(c, n)             # body
      while n.hasMore: skip n
  of LoopS:
    n.into:
      if n.hasMore: tr(c, n)             # before-cond
      if n.hasMore: trExpr(c, n)         # cond
      if n.hasMore: tr(c, n)             # body
      if n.hasMore: tr(c, n)             # after
      while n.hasMore: skip n
  else:
    skip n

proc trLoop(c: var Context; n: var Cursor) =
  # The body may run any number of times. Model it as a 2-sibling group
  # (no-iteration + body); a copy of a symbol written in the body cannot be
  # carried across the back-edge, so invalidate every such write before walking.
  var writes = initHashSet[SymId]()
  collectWrites(n, writes)
  openBranches c
  openBranch c
  closeBranch c
  openBranch c
  for s in writes: invalidate(c, s)
  trLoopBody(c, n)
  closeBranch c
  closeBranches c

proc trJmp(c: var Context; n: var Cursor) =
  let probe = child0(n)
  if probe.kind == Symbol:
    gotoLabel(c, LabelId(uint32(symId(probe))))
  skip n

proc trLab(c: var Context; n: var Cursor) =
  let probe = child0(n)
  if probe.kind == SymbolDef:
    landLabel(c, LabelId(uint32(symId(probe))))
  skip n

proc trBreakOrRet(c: var Context; n: var Cursor) =
  skip n
  clearAll c

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of TagLit:
    case n.stmtKind
    of VarS, GvarS, TvarS, ConstS: trVar(c, n)
    of AsgnS, StoreS:              trAsgn(c, n)
    of CallS:                      trCallStmt(c, n)
    of IfS:                        trIf(c, n)
    of CaseS:                      trCase(c, n)
    of WhileS, LoopS:              trLoop(c, n)
    of JmpS:                       trJmp(c, n)
    of LabS:                       trLab(c, n)
    of BreakS, RetS, RaiseS:       trBreakOrRet(c, n)
    of StmtsS, ScopeS:
      n.loopInto:
        tr(c, n)
    else:
      trExpr(c, n)
  else:
    inc n

# ---- pre-pass: addr-taken, local decls, use counts, names -----------------

proc preScan(c: var Context; n: Cursor) =
  case n.kind
  of Symbol:
    let s = symId(n)
    c.names[s] = symName(n)
    c.useCount.mgetOrPut(s, 0) += 1
  of SymbolDef:
    c.names[symId(n)] = symName(n)
  of TagLit:
    if n.stmtKind == VarS:
      let nameCur = child0(n)
      if nameCur.kind == SymbolDef:
        c.localDefs.incl symId(nameCur)
    if n.exprKind == AddrC:
      let s = rootOf(child0(n))
      if s != SymId(0): c.addrTaken.incl s
    var m = n
    m.loopInto:
      preScan(c, m)
      skip m
  else:
    discard

# ---- public entry ---------------------------------------------------------

proc runCopyProp*(buf: var TokenBuf) =
  ## In-place copy propagation + dead-binding elimination for a single proc body.
  var ctx = createContext(addr buf)
  block:
    let pn = beginRead(buf)
    preScan(ctx, pn)
  var n = beginRead(buf)
  tr(ctx, n)
  # Delete every candidate decl all of whose uses were rewritten away (or which
  # had no uses at all). A surviving un-rewritten use keeps subst < total.
  for s, defPos in ctx.delCandidates:
    if ctx.substCount.getOrDefault(s) == ctx.useCount.getOrDefault(s):
      ctx.patchset.addSubst(defPos, cursorAt(ctx.dotBuf, 0))
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests -----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  template chk(input, expected: string) =
    var buf = parse(input)
    runCopyProp buf
    let got = toString(buf)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runCopyProp buf
    doAssert toString(buf) == before, "expected unchanged:\n  " & input

  block headline_copy_then_dead_binding:
    # var x = f(); var y = x; use y  -->  var x = f(); use x
    chk(
      "(stmts (var :x.0.M . . (call f.0.M)) " &
      "(var :y.0.M . . x.0.M) " &
      "(call use.0.M y.0.M))",
      "(stmts (var :x.0.M . . (call f.0.M)) . " &
      "(call use.0.M x.0.M))")

  block chained_copy:
    # var x = f(); var y = x; var z = y; use z  -->  ...; use x  (y, z deleted)
    chk(
      "(stmts (var :x.0.M . . (call f.0.M)) " &
      "(var :y.0.M . . x.0.M) (var :z.0.M . . y.0.M) " &
      "(call use.0.M z.0.M))",
      "(stmts (var :x.0.M . . (call f.0.M)) . . " &
      "(call use.0.M x.0.M))")

  block source_reassigned_blocks:
    # `x` changes after the copy, so `y` may not be propagated past it.
    assertUnchanged(
      "(stmts (var :x.0.M . . (call f.0.M)) " &
      "(var :y.0.M . . x.0.M) (asgn x.0.M (call g.0.M)) " &
      "(call use.0.M y.0.M))")

  block copy_reassigned_blocks:
    # `y` itself is reassigned, so the original copy can't be propagated.
    assertUnchanged(
      "(stmts (var :x.0.M . . (call f.0.M)) " &
      "(var :y.0.M . . x.0.M) (asgn y.0.M (call g.0.M)) " &
      "(call use.0.M y.0.M))")

  block addr_taken_blocks:
    # `addr y` is taken → y is excluded from copy propagation entirely.
    assertUnchanged(
      "(stmts (var :x.0.M . . (call f.0.M)) " &
      "(var :y.0.M . . x.0.M) (call sink.0.M (addr y.0.M)) " &
      "(call use.0.M y.0.M))")

  block call_does_not_kill_copy:
    # A call cannot touch a non-addr-taken local, so the copy survives it.
    chk(
      "(stmts (var :x.0.M . . (call f.0.M)) " &
      "(var :y.0.M . . x.0.M) (call side.0.M) " &
      "(call use.0.M y.0.M))",
      "(stmts (var :x.0.M . . (call f.0.M)) . " &
      "(call side.0.M) (call use.0.M x.0.M))")

  block dead_store_unused_pure_binding:
    # var t = 5 with t never used → dead store, deleted.
    chk(
      "(stmts (var :t.0.M . . 5) (call side.0.M))",
      "(stmts . (call side.0.M))")

  block dead_store_keeps_impure_binding:
    # var t = f() unused: the call's side effect must run, so it is kept.
    assertUnchanged(
      "(stmts (var :t.0.M . . (call f.0.M)) (call side.0.M))")

  block propagate_into_both_branches:
    chk(
      "(stmts (var :x.0.M . . (call f.0.M)) (var :y.0.M . . x.0.M) " &
      "(if (elif c.0.M (call a.0.M y.0.M)) (else (call b.0.M y.0.M))))",
      "(stmts (var :x.0.M . . (call f.0.M)) . " &
      "(if (elif c.0.M (call a.0.M x.0.M)) (else (call b.0.M x.0.M))))")

  block loop_preserves_unwritten_copy:
    # `x` is not written in the loop → its copy `y` survives, inside and after.
    chk(
      "(stmts (var :x.0.M . . (call f.0.M)) (var :y.0.M . . x.0.M) " &
      "(while c.0.M (stmts (call use.0.M y.0.M))) " &
      "(call use2.0.M y.0.M))",
      "(stmts (var :x.0.M . . (call f.0.M)) . " &
      "(while c.0.M (stmts (call use.0.M x.0.M))) " &
      "(call use2.0.M x.0.M))")

  block loop_writing_source_blocks:
    # `x` is reassigned in the loop → the copy can't be carried across it.
    assertUnchanged(
      "(stmts (var :x.0.M . . (call f.0.M)) (var :y.0.M . . x.0.M) " &
      "(while c.0.M (stmts (asgn x.0.M (call g.0.M)))) " &
      "(call use.0.M y.0.M))")

  echo "copyprop.nim: all self-tests passed"
