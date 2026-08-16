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
##   so chains (`z = y = x`) collapse to `z -> x` directly. Bindings come from
##   both `var y = x` **and** `y = x` (assignment-shaped copies are what inlining
##   actually produces).
## * A parallel `litOf` tracker snapshots leaf literals (`y = 5`, `y = (true)`).
##   A snapshot is a value, not an alias: it survives a later write to the
##   symbol that happened to hold those bits at bind time (`var x = 5; var y = x;
##   x = 6; use y` → `use 5`).
## * An assignment to a local's **storage** invalidates that local as a copy
##   (its value changed) and every alias `k` with `copyOf[k] == s`. A
##   through-pointer store (`(*p).f = v`) writes the pointee, not `p` — `p` is
##   only read, so copies of `p` stay valid. Calls invalidate nothing — see above.
## * On an assignment LHS, the storage spine is not substituted (`&y` ≠ `&x`
##   even when `y == x`), but a `deref`/`pat` breaks that spine: `(*p2).f = v`
##   with `p2 = p` rewrites to `(*p).f = v`.
## * **Dead store elimination** of the now-useless `var y = x` / `y = x` (and of
##   any pure leaf-initialised local that ends up unused) falls out for free: a
##   local binding to a side-effect-free leaf is a deletion candidate, and we
##   delete it iff every *read* was rewritten away (`subst + pureWrites == uses`).
##   A surviving un-rewritten use — an impure reassignment, an `addr` operand,
##   or a read past an invalidation — keeps the decl. Deleted stmts become `.`.

import std / [tables, sets, hashes, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import trackers, patchsets

# ---- nifcore helpers ------------------------------------------------------

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc addrRootOf(c: Cursor): SymId =
  ## The *storage* root of an `addr` operand — the local whose address is actually
  ## taken. Unlike the deref-blind `rootOf`, it STOPS at a through-pointer step
  ## (`deref`/`pat`): `addr((*p).f)` addresses the POINTEE — a heap location computed
  ## from `p`'s value — so it does NOT take the address of `p`'s own storage; `p` is
  ## only READ. Returns the base symbol of the leftmost projection spine, or
  ## `SymId(0)` when that spine crosses a `deref`/`pat` (⇒ no local is address-taken).
  ## Mirrors the analyser's DerefC context reset / mover's `CannotFollowDerefs`. Any
  ## spine shape it does not model falls back to the conservative `rootOf` (which still
  ## marks something address-taken), so the relaxation is one-directional: it can only
  ## un-mark a pointer whose pointee — never its storage — is addressed. That copy is
  ## still sound to propagate because the pointer VALUE is what a substitution carries.
  var n = c
  while true:
    case n.kind
    of Symbol: return symId(n)
    of TagLit:
      case n.exprKind
      of DerefC, PatC: return SymId(0)     # through-pointer: the pointee, not the var
      of DotC, AtC: inc n                   # object field / array index: base is 1st child
      of ConvC, CastC:
        inc n; skip n                       # `(conv/cast Type operand)` — skip the type
      else: return rootOf(n)                # unmodelled spine: stay conservative
    else: return SymId(0)

const LeafKinds = {Symbol, IntLit, UIntLit, FloatLit, CharLit, StrLit}
  ## Side-effect-free initializers a dead local binding may be deleted with.

proc isLeafLit(n: Cursor): bool {.inline.} =
  ## A side-effect-free literal we can snapshot: atoms and `(true)`/`(false)`.
  ## `(suf 7 "i64")` is how hexer writes a typed integer literal after inlining;
  ## treating it as a non-leaf left `var size = 7i64; memcpy(..., size)` as a
  ## runtime memcpy of a register — the 7-byte string-prefix sync never unrolled.
  ## Not `(nil)`: substituting `nil` into a pointer use like `(deref p)` /
  ## `(dot (deref p) f)` drops p's pointee type, and arkham then sees a field
  ## access on `(void)`. Not a `Symbol` — that is a copy source.
  case n.kind
  of IntLit, UIntLit, FloatLit, CharLit, StrLit: true
  of TagLit:
    if n.exprKind in {TrueC, FalseC}: true
    elif n.exprKind == SufC:
      var t = n; inc t
      isLeafLit(t)
    else: false
  else: false

proc isStableAddrExpr(c: Cursor; deps: var seq[SymId]): bool =
  ## `(haddr L)` / `(addr L)` over a *stable spine*: a projection chain whose
  ## address is computed from slot VALUES alone — symbols, field offsets,
  ## constant or symbol indices, a `deref`/`pat` step through a pointer symbol.
  ## Evaluating such an expression reads no memory beyond the named slots in
  ## `deps`, so the address stays valid exactly as long as none of `deps` is
  ## assigned — the invalidation copyprop already tracks. This is the compound
  ## sibling of the inliner's bare-symbol `isStableAddrArg`: `inc st.tags`
  ## splices as `(var :p (ptr T) (haddr (dot (deref st) tags)))` and the temp
  ## must be propagated here for the rewriter's `deref_addr` rule to fold it.
  if c.kind != TagLit or c.exprKind notin AddrKinds: return false
  proc spine(n: var Cursor; deps: var seq[SymId]): bool =
    ## Consume exactly one value node; false on any shape whose evaluation
    ## could read memory or have effects.
    case n.kind
    of Symbol:
      deps.add symId(n)
      skip n
      result = true
    of IntLit, UIntLit:
      skip n
      result = true
    of TagLit:
      case n.exprKind
      of DotC:
        result = true
        n.into:
          if n.hasMore: result = spine(n, deps)   # base
          while n.hasMore: skip n                 # field selector, inheritance
      of DerefC:
        result = false
        n.into:
          if n.hasMore: result = spine(n, deps)   # pointer value: a slot read
          while n.hasMore:
            skip n
            result = false
      of AtC, PatC:
        result = false
        n.into:
          if n.hasMore: result = spine(n, deps)   # base
          if n.hasMore:                           # index is a value too
            if not spine(n, deps): result = false
          while n.hasMore:
            skip n
            result = false
      of ConvC, CastC:
        result = false
        n.into:
          if n.hasMore: skip n                    # the type
          if n.hasMore: result = spine(n, deps)
          while n.hasMore:
            skip n
            result = false
      else:
        skip n
        result = false
    else:
      inc n
      result = false
  var op = child0(c)
  result = spine(op, deps)

proc writtenRoot(c: Cursor): SymId =
  ## The local whose STORAGE this lvalue writes. A through-pointer store
  ## (`(*p).f = v`, `p[i] = v`) writes the pointee, not `p` — `p` is only
  ## read, so copies of `p` stay valid. Mirrors `addrRootOf`.
  var n = c
  while true:
    case n.kind
    of Symbol: return symId(n)
    of TagLit:
      case n.exprKind
      of DerefC, PatC: return SymId(0)     # through-pointer: the pointee, not the var
      of DotC, AtC: inc n                   # object field / array index: base is 1st child
      of ConvC, CastC:
        inc n; skip n                       # `(conv/cast Type operand)` — skip the type
      else: return rootOf(n)                # unmodelled spine: stay conservative
    else: return SymId(0)

# ---- context --------------------------------------------------------------

type
  Context = object
    orig: ptr TokenBuf
    copyOf: Tracker[SymId, SymId]      ## y -> x: y currently aliases x
    litOf: Tracker[SymId, int]         ## y -> (litPos+1): y currently holds that leaf
    addrOf: Tracker[SymId, int]        ## p -> (addrPos+1): p holds that stable `(haddr …)`
    addrDeps: Table[int, seq[SymId]]   ## addrPos -> spine symbols the address depends on
    pinned: HashSet[SymId]             ## spine symbols a snapshot may splice: never delete
    addrTaken: HashSet[SymId]          ## syms whose address is taken anywhere
    localDefs: HashSet[SymId]          ## syms declared by a local `(var …)`
    useCount: Table[SymId, int]        ## every `Symbol` (read) token per sym
    substCount: Table[SymId, int]      ## uses we rewrote away per sym
    writeSites: Table[SymId, seq[int]] ## pure (leaf/symbol RHS) write positions
    names: Table[SymId, string]        ## sym -> its textual name (for synthesis)
    delCandidates: Table[SymId, int]   ## deletable local binding -> its decl pos
    scopeDecls: seq[seq[SymId]]        ## locals declared at each open `(scope)`
    patchset: Patchset
    synth: seq[TokenBuf]
    dotBuf: TokenBuf                   ## a single `.` token: replaces a dead decl

proc createContext(orig: ptr TokenBuf): Context =
  result = Context(orig: orig,
          copyOf: initTracker[SymId, SymId](),
          litOf: initTracker[SymId, int](),
          addrOf: initTracker[SymId, int](),
          addrDeps: initTable[int, seq[SymId]](),
          pinned: initHashSet[SymId](),
          addrTaken: initHashSet[SymId](),
          localDefs: initHashSet[SymId](),
          useCount: initTable[SymId, int](),
          substCount: initTable[SymId, int](),
          writeSites: initTable[SymId, seq[int]](),
          names: initTable[SymId, string](),
          delCandidates: initTable[SymId, int](),
          scopeDecls: @[@[]],
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
  ## `s` was written: it is no longer a copy of anything and no longer holds
  ## a snapshotted literal. Every alias whose source is `s` is now stale as a
  ## *symbol* copy; a literal snapshot on the alias is independent and stays.
  if s == SymId(0): return
  c.copyOf[s] = SymId(0)
  c.litOf[s] = 0
  c.addrOf[s] = 0
  var toClear: seq[SymId] = @[]
  for k, v in c.copyOf.pairs:
    if v == s: toClear.add k
  for k in toClear:
    c.copyOf[k] = SymId(0)
  # An address snapshot is a value computed from its spine symbols' slots;
  # a write to any of them recomputes to a DIFFERENT address, so unlike a
  # literal snapshot it does not survive.
  toClear.setLen 0
  for k, v in c.addrOf.pairs:
    if v != 0 and s in c.addrDeps.getOrDefault(v - 1): toClear.add k
  for k in toClear:
    c.addrOf[k] = 0

proc isEligible(c: Context; s: SymId): bool {.inline.} =
  s != SymId(0) and s in c.localDefs and s notin c.addrTaken

proc bindCopy(c: var Context; dest: SymId; src: Cursor) =
  ## Record that `dest` now holds `src`'s value. A leaf literal is snapshotted
  ## (survives a later write to a symbol that happened to hold the same bits);
  ## a symbol copy lasts until either side is assigned.
  if not c.isEligible(dest): return
  c.addrOf[dest] = 0
  if isLeafLit(src):
    c.litOf[dest] = cursorToPosition(c.orig[], src) + 1
    c.copyOf[dest] = SymId(0)
  elif src.kind == Symbol:
    let root = resolve(c, symId(src))
    let lp = c.litOf[root]
    if lp != 0:
      # Snapshot the literal; independent of the source from here on.
      c.litOf[dest] = lp
      c.copyOf[dest] = SymId(0)
    elif c.isEligible(root) and root != dest:
      c.copyOf[dest] = root
      c.litOf[dest] = 0
    else:
      c.copyOf[dest] = SymId(0)
      c.litOf[dest] = 0
  else:
    c.copyOf[dest] = SymId(0)
    c.litOf[dest] = 0
    # The inliner's by-address residue: `var p = (haddr LVAL)` for a compound
    # LVAL (`inc st.tags`). Snapshot the address expression when its spine is
    # stable (every spine symbol a non-addr-taken local/param): substituting it
    # into `(deref p)` re-forms `(deref (haddr LVAL))`, which the rewrite
    # engine's `deref_addr` rule then folds back to the plain lvalue.
    var deps: seq[SymId] = @[]
    if isStableAddrExpr(src, deps):
      var ok = true
      for d in deps:
        if not c.isEligible(d):
          ok = false
          break
      if ok:
        let pos = cursorToPosition(c.orig[], src)
        c.addrOf[dest] = pos + 1
        c.addrDeps[pos] = deps
        # The spliced copies keep reading the spine symbols, but `useCount`
        # was taken before splicing — never delete their decls.
        for d in deps: c.pinned.incl d

# ---- branch-state forwarding (same surface as cse.nim) --------------------

proc openBranches(c: var Context) =
  c.copyOf.openBranches(); c.litOf.openBranches(); c.addrOf.openBranches()
proc openBranch(c: var Context) =
  c.copyOf.openBranch(); c.litOf.openBranch(); c.addrOf.openBranch()
proc openFinalBranch(c: var Context) =
  c.copyOf.openFinalBranch(); c.litOf.openFinalBranch(); c.addrOf.openFinalBranch()
proc closeBranch(c: var Context) =
  c.copyOf.closeBranch(); c.litOf.closeBranch(); c.addrOf.closeBranch()
proc closeBranches(c: var Context) =
  c.copyOf.closeBranches(); c.litOf.closeBranches(); c.addrOf.closeBranches()
proc gotoLabel(c: var Context; L: LabelId) =
  c.copyOf.gotoLabel L; c.litOf.gotoLabel L; c.addrOf.gotoLabel L
proc landLabel(c: var Context; L: LabelId) =
  c.copyOf.landLabel L; c.litOf.landLabel L; c.addrOf.landLabel L
proc clearAll(c: var Context) =
  c.copyOf.clearAll(); c.litOf.clearAll(); c.addrOf.clearAll()

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

proc substituteLit(c: var Context; n: Cursor; litPos: int) =
  ## Rewrite the `Symbol` token at `n` to the snapshotted leaf at `litPos`.
  let pos = cursorToPosition(c.orig[], n)
  c.patchset.addSubst(pos, cursorAt(c.orig[], litPos))
  c.substCount.mgetOrPut(symId(n), 0) += 1

proc trySubstitute(c: var Context; n: Cursor) =
  ## Prefer a snapshotted literal, then a snapshotted address, then a symbol copy.
  let s = symId(n)
  let root = resolve(c, s)
  var lp = c.litOf[s]
  if lp == 0: lp = c.litOf[root]
  var ap = c.addrOf[s]
  if ap == 0: ap = c.addrOf[root]
  if lp != 0:
    substituteLit(c, n, lp - 1)
  elif ap != 0:
    substituteLit(c, n, ap - 1)     # splices the whole `(haddr …)` subtree
  elif root != s:
    substituteSym(c, n, root)

proc enterScope(c: var Context) {.inline.} =
  c.scopeDecls.add @[]

proc leaveScope(c: var Context) =
  ## A `(scope)` is a real Leng lifetime. Substituting an inner local into a
  ## use after that scope closes extends the inner name past its kill — arkham
  ## then reuses the register. Drop every copy sourced from a local declared
  ## here; the outer destination keeps its own storage and later uses stay
  ## as that outer name. Literal snapshots are values, not names, and survive
  ## (`invalidate` does not clear an alias's `litOf`).
  for s in c.scopeDecls[^1]:
    invalidate(c, s)
  c.scopeDecls.setLen c.scopeDecls.len - 1

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward
proc trExpr(c: var Context; n: var Cursor)   # forward
proc trAddrOperand(c: var Context; n: var Cursor)   # forward

proc trExpr(c: var Context; n: var Cursor) =
  case n.kind
  of Symbol:
    trySubstitute(c, n)
    inc n
  of TagLit:
    case n.exprKind
    of AddrC, HaddrC:
      # `addr x`: the *address* of `x`, not its value. On the leftmost STORAGE spine
      # `&y` and `&x` differ even when `y == x` (distinct storage), so the base local
      # is not substituted. But a `deref` breaks the spine: `&((*p).f)` addresses the
      # POINTEE — a location computed from p's VALUE — so `p` (and anything below the
      # deref) is an ordinary value use and IS substitutable. `trAddrOperand` walks the
      # spine with that distinction; this is what collapses an inlined pointer param
      # copy `p2 = p` whose only uses are `&((*p2)…)`.
      n.into:
        while n.hasMore: trAddrOperand(c, n)
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

proc trAddrOperand(c: var Context; n: var Cursor) =
  ## Traverse the operand of `(addr …)` on the storage spine: the leftmost projection
  ## chain whose base local's address is being formed. A bare local here is the storage
  ## root and is NOT substituted (`&x.f` ≠ `&y.f` for a value-copy). A `deref`/`pat`
  ## breaks the spine — below it we address the POINTEE, so the pointer is a value use
  ## and normal substitution (`trExpr`) resumes. Array indices are values too.
  case n.kind
  of Symbol:
    skip n                                 # storage root — never substitute
  of TagLit:
    case n.exprKind
    of DerefC, PatC:
      # through-pointer: the pointer (and its index) are value reads → substitutable
      trExpr(c, n)
    of DotC:
      n.into:
        if n.hasMore: trAddrOperand(c, n)  # object base stays on the storage spine
        if n.hasMore: skip n               # field selector — leave untouched
        while n.hasMore: skip n            # inheritance depth
    of AtC:
      n.into:
        if n.hasMore: trAddrOperand(c, n)  # array base stays on the storage spine
        while n.hasMore: trExpr(c, n)      # index is a value
    of ConvC, CastC:
      n.into:
        if n.hasMore: skip n               # target type
        if n.hasMore: trAddrOperand(c, n)  # operand stays on the storage spine
        while n.hasMore: skip n
    else:
      skip n                               # unmodelled spine: conservative, no subst
  else:
    inc n

proc trVar(c: var Context; n: var Cursor) =
  let isLocal = n.stmtKind == VarS
  let defPos = cursorToPosition(c.orig[], n)
  var nameSym = SymId(0)
  n.into:
    if n.hasMore:
      if n.kind == SymbolDef:
        nameSym = symId(n)
        if isLocal: c.scopeDecls[^1].add nameSym
      skip n                             # name
    if n.hasMore: skip n                 # pragmas
    if n.hasMore: skip n                 # type
    if n.hasMore:
      let initStart = n
      let initKind = initStart.kind
      trExpr(c, n)                       # propagate inside the initializer
      if isLocal and nameSym != SymId(0) and nameSym notin c.addrTaken:
        bindCopy(c, nameSym, initStart)
        # Deletable if its initializer is a side-effect-free leaf or empty:
        # dead once all its reads are rewritten away (copy) or it was never
        # used (pure store). A snapshotted stable address is pure too.
        if initKind in LeafKinds or initKind == DotToken or
           isLeafLit(initStart) or c.addrOf[nameSym] != 0:
          c.delCandidates[nameSym] = defPos
    while n.hasMore: skip n

proc trAsgn(c: var Context; n: var Cursor) =
  let isStore = n.stmtKind == StoreS
  let asgnPos = cursorToPosition(c.orig[], n)
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
  let haveLhs = if isStore: haveSecond else: haveFirst
  let haveRhs = if isStore: haveFirst else: haveSecond
  if haveRhs:
    var r = rhs
    trExpr(c, r)                         # propagate inside the value
  if haveLhs:
    var l = lhs
    # Storage spine is not substituted (`y = …` still writes `y`); a deref
    # breaks the spine, so `(*p2).f = v` with `p2 = p` rewrites `p2` → `p`.
    trAddrOperand(c, l)
    invalidate(c, writtenRoot(lhs))
    if lhs.kind == Symbol:
      let dest = symId(lhs)
      if haveRhs:
        var depsIgnored: seq[SymId] = @[]
        if isLeafLit(rhs) or rhs.kind == Symbol or
           isStableAddrExpr(rhs, depsIgnored):
          c.writeSites.mgetOrPut(dest, @[]).add asgnPos
        bindCopy(c, dest, rhs)

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
  ## Storage root of every `asgn`/`store` target in the subtree at `start`.
  ## Through-pointer stores do not count as writes to the pointer.
  if not start.hasMore or start.kind != TagLit: return
  let sk = start.stmtKind
  if sk in {AsgnS, StoreS}:
    var lhs = child0(start)
    if sk == StoreS: skip lhs            # dest is the 2nd child
    let root = writtenRoot(lhs)
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
  ## `(ret x)` / `(raise x)` carry a VALUE, and it is read like any other — an
  ## inlined body's result temp is very often returned straight back, so skipping
  ## the operand left the pass blind to exactly the shape inlining produces.
  ## `(break L)` carries a LABEL, which must never be substituted.
  if n.stmtKind in {RetS, RaiseS}:
    n.into:
      if n.hasMore and n.kind != DotToken: trExpr(c, n)
      while n.hasMore: skip n
  else:
    skip n
  # Nothing after the transfer is reachable from here; the state is dead.
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
    of ScopeS:
      enterScope c
      n.loopInto:
        tr(c, n)
      leaveScope c
    of StmtsS:
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
    if n.exprKind in AddrKinds:
      # Only a local whose OWN storage is addressed is excluded from copy prop. An
      # `addr((*p).f)` addresses the pointee (computed from p's value), so `p` stays
      # propagatable — `addrRootOf` stops at the `deref`/`pat` and returns SymId(0).
      let s = addrRootOf(child0(n))
      if s != SymId(0): c.addrTaken.incl s
    var m = n
    m.loopInto:
      preScan(c, m)
      skip m
  else:
    discard

# ---- public entry ---------------------------------------------------------

proc registerParams(c: var Context; params: Cursor) =
  ## Mark the proc's parameters as eligible copy *sources*. copyprop only sees the
  ## proc *body* (the params live in the enclosing decl the driver strips off), so
  ## without this a `var y = param` copy is never propagated. A param is as safe a
  ## source as an address-not-taken local: a by-value param only changes by a direct
  ## assignment to it (caught by `trAsgn` → `invalidate`), and a `var`/`out` param
  ## is a pointer whose bare-symbol use is the pointer value (a through-`deref` store
  ## invalidates via `rootOf`). This is what lets SROA's `T(x: a, …)` → `sroa = a`
  ## parameter copies collapse away. The `addrTaken` pre-scan of the body still
  ## excludes any param whose address is taken.
  if not params.hasMore or params.kind != TagLit: return
  var p = params
  p.loopInto:
    if p.kind == TagLit and p.substructureKind == ParamU:
      let nameCur = child0(p)
      if nameCur.kind == SymbolDef:
        let s = symId(nameCur)
        c.localDefs.incl s
        c.names[s] = symName(nameCur)
        c.scopeDecls[0].add s
    skip p

proc runCopyProp*(buf: var TokenBuf; params: Cursor = default(Cursor)) =
  ## In-place copy propagation + dead-binding elimination for a single proc body.
  ## `params` is the enclosing proc's `(params …)` node (empty for a bare body);
  ## its parameters become eligible copy sources (see `registerParams`).
  var ctx = createContext(addr buf)
  registerParams(ctx, params)
  block:
    let pn = beginRead(buf)
    preScan(ctx, pn)
  var n = beginRead(buf)
  tr(ctx, n)
  # Delete every candidate decl whose every read was rewritten away (or which
  # had no reads) and which has no impure writes left. Pure `y = x` / `y = 5`
  # assignments to a now-dead local go with it.
  for s, defPos in ctx.delCandidates:
    if s in ctx.pinned: continue   # an address snapshot's spliced copies read it
    let uses = ctx.useCount.getOrDefault(s)
    let subst = ctx.substCount.getOrDefault(s)
    let sites = ctx.writeSites.getOrDefault(s)
    if subst + sites.len == uses:
      ctx.patchset.addSubst(defPos, cursorAt(ctx.dotBuf, 0))
      for pos in sites:
        ctx.patchset.addSubst(pos, cursorAt(ctx.dotBuf, 0))
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

  block asgn_shaped_copy:
    # Inlining leaves `var y; y = x; use y`, not `var y = x`.
    chk(
      "(stmts (var :x.0.M . . (call f.0.M)) (var :y.0.M . . .) " &
      "(asgn y.0.M x.0.M) (call use.0.M y.0.M))",
      "(stmts (var :x.0.M . . (call f.0.M)) . . (call use.0.M x.0.M))")

  block literal_copy:
    # var y = 5; use y  →  use 5  (decl deleted)
    chk(
      "(stmts (var :y.0.M . . 5) (call use.0.M y.0.M))",
      "(stmts . (call use.0.M 5))")

  block suffixed_int_literal_copy:
    # hexer's typed `7i64` is `(suf 7 "i64")`, not a bare IntLit. Inlining
    # `copyMem(..., AlwaysAvail)` produces `var size = (suf 7 "i64")` and the
    # memcpy size must become the suf node so arkham can unroll.
    chk(
      "(stmts (var :y.0.M . . (suf 7 \"i64\")) (call memcpy.0.M d.0.M s.0.M y.0.M))",
      "(stmts . (call memcpy.0.M d.0.M s.0.M (suf 7 \"i64\")))")

  block literal_snapshot_survives_source_write:
    # y captures 5 at bind time; a later write to x must not poison y.
    # x itself then has no remaining reads, so its decl and the dead `x = 6`
    # store are deleted too.
    chk(
      "(stmts (var :x.0.M . . 5) (var :y.0.M . . x.0.M) " &
      "(asgn x.0.M 6) (call use.0.M y.0.M))",
      "(stmts . . . (call use.0.M 5))")

  block through_ptr_store_keeps_pointer_copy:
    # `(*p).f = v` writes the pointee, not p. A copy of p stays valid, and
    # the LHS pointer is itself a value use — substitute it.
    chk(
      "(stmts (var :p.0.M . . (call f.0.M)) (var :q.0.M . . p.0.M) " &
      "(asgn (dot (deref q.0.M) fld.0.M) v.0.M) " &
      "(call use.0.M q.0.M))",
      "(stmts (var :p.0.M . . (call f.0.M)) . " &
      "(asgn (dot (deref p.0.M) fld.0.M) v.0.M) " &
      "(call use.0.M p.0.M))")

  block loop_through_ptr_store_keeps_pointer_copy:
    # Storing through p inside a loop is not a write to p.
    chk(
      "(stmts (var :p.0.M . . (call f.0.M)) (var :q.0.M . . p.0.M) " &
      "(while c.0.M (stmts (asgn (deref q.0.M) v.0.M))) " &
      "(call use.0.M q.0.M))",
      "(stmts (var :p.0.M . . (call f.0.M)) . " &
      "(while c.0.M (stmts (asgn (deref p.0.M) v.0.M))) " &
      "(call use.0.M p.0.M))")

  block scope_does_not_leak_inner_name:
    # Outer `y = inner x` is a valid copy INSIDE the scope, but substituting
    # the inner name into a use after the `(scope)` closes would outlive x's
    # lifetime (arkham then reuses the register). Keep the outer name.
    chk(
      "(stmts (var :y.0.M . . .) " &
      "(scope (stmts (var :x.0.M . . (call f.0.M)) " &
      "(asgn y.0.M x.0.M) (call inner.0.M y.0.M))) " &
      "(call outer.0.M y.0.M))",
      "(stmts (var :y.0.M . . .) " &
      "(scope (stmts (var :x.0.M . . (call f.0.M)) " &
      "(asgn y.0.M x.0.M) (call inner.0.M x.0.M))) " &
      "(call outer.0.M y.0.M))")

  block addr_snapshot_inlined_inc:
    # The inliner's by-address residue for a COMPOUND lvalue: `inc st.tags`
    # splices as a pointer temp. Substitute the address into both derefs
    # (the rewriter later folds `(deref (haddr …))`) and delete the temp.
    chk(
      "(stmts (var :st.0.M . . (call f.0.M)) " &
      "(var :p.0.M . (ptr (i 64)) (haddr (dot (deref st.0.M) tags.0.M 0))) " &
      "(asgn (deref p.0.M) (add (i 64) (deref p.0.M) 1)))",
      "(stmts (var :st.0.M . . (call f.0.M)) . " &
      "(asgn (deref (haddr (dot (deref st.0.M) tags.0.M 0))) " &
      "(add (i 64) (deref (haddr (dot (deref st.0.M) tags.0.M 0))) 1)))")

  block addr_snapshot_spine_write_blocks:
    # The spine symbol is reassigned between binding and use: the address
    # would differ, so the use may not be substituted (and the decl stays).
    assertUnchanged(
      "(stmts (var :st.0.M . . (call f.0.M)) " &
      "(var :p.0.M . (ptr (i 64)) (haddr (dot (deref st.0.M) tags.0.M 0))) " &
      "(asgn st.0.M (call g.0.M)) " &
      "(asgn (deref p.0.M) 1))")

  block addr_snapshot_loop_spine_write_blocks:
    # Same across a loop back-edge: st is written in the body, so the
    # snapshot cannot be carried into it.
    assertUnchanged(
      "(stmts (var :st.0.M . . (call f.0.M)) " &
      "(while c.0.M (stmts " &
      "(var :p.0.M . (ptr (i 64)) (haddr (dot (deref st.0.M) tags.0.M 0))) " &
      "(asgn st.0.M (call g.0.M)) " &
      "(asgn (deref p.0.M) 1))))")

  block addr_snapshot_pins_spine_local:
    # q's only counted read sits inside the snapshot; splicing duplicates it,
    # so q's decl must survive even though that read is "rewritten away".
    chk(
      "(stmts (var :q.0.M . . (call f.0.M)) " &
      "(var :p.0.M . (ptr (i 64)) (haddr (deref q.0.M))) " &
      "(asgn (deref p.0.M) 1))",
      "(stmts (var :q.0.M . . (call f.0.M)) . " &
      "(asgn (deref (haddr (deref q.0.M))) 1))")

  block inlined_callee_with_early_returns:
    # THE miscompile the literal snapshots were disabled for. An inlined callee
    # with early `return`s lowers to `… (jmp ret) … (lab ret) …` *inside* one
    # branch; the `lab` revives that branch. `(asgn r x)` after it is a branch
    # write like any other and must not survive the join with the else arm's
    # `(asgn r (false))` — substituting `(false)` into the following `if` was
    # `multiReplace` silently replacing nothing.
    assertUnchanged(
      "(stmts (var :r.0.M . . .) (var :v.0.M . . (call f.0.M)) " &
      "(if (elif c.0.M (stmts " &
      "(jmp ret.0.M) (lab :ret.0.M) (asgn r.0.M v.0.M))) " &
      "(else (stmts (asgn r.0.M (false))))) " &
      "(if (elif r.0.M (call use.0.M))))")

  block jump_out_of_branch_still_isolated:
    # Same machinery, but the branch really leaves: the `lab` is *after* the
    # `if`, so the else arm's `(false)` still may not escape the join.
    assertUnchanged(
      "(stmts (var :r.0.M . . .) " &
      "(if (elif c.0.M (stmts (asgn r.0.M (true)) (jmp ret.0.M))) " &
      "(else (stmts (asgn r.0.M (false))))) " &
      "(lab :ret.0.M) " &
      "(if (elif r.0.M (call use.0.M))))")

  echo "copyprop.nim: all self-tests passed"
