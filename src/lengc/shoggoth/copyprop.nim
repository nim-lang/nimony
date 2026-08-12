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
## * A copy is established BOTH at a binding `(var :y . T x)` and at a store
##   `(asgn y x)`. The store form is not an afterthought — it is the shape that
##   matters. hexer's intra-module inliner never emits `var y = x`: an inlined
##   body may return from several points, so its result temp is declared WITHOUT
##   an initializer and the value arrives by assignment before the body's return
##   label. Modelling only the binding form left the pass firing on 244 copies per
##   nifbench build while 845 store-form copies went past it.
## * An assignment to a symbol `s` (or a field/through-pointer store whose base is
##   `s`) invalidates `s` as a copy (its value changed) and every alias `k` with
##   `copyOf[k] == s` (the source moved). Calls invalidate nothing — see above.
##   At a store that both invalidates and establishes, the new source is resolved
##   BEFORE the invalidation: `invalidate(y)` clears every entry whose *value* is
##   `y`, which would otherwise drop the chain being read.
## * A copy is only recorded when the source is declared no deeper than the
##   destination. `(scope …)` is a REAL scope — the C backend emits `{ }` for it
##   and arkham frees its locals' registers at the close — so substituting a
##   symbol declared inside one into a use that outlives it would name a dead
##   variable. For a well-formed assignment both operands are live at that point,
##   so equal depth means the same scope and the guard is exact, not merely safe.
## * **Dead store elimination.** A symbol is dead when every READ of it was
##   rewritten away. `uses` counts bare-symbol assignment targets too — and those
##   are never substituted (`trAsgn` leaves the LHS alone) — so the test is
##   `substituted == uses - writes`. For a dead symbol: every side-effect-free
##   store to it (`(asgn y <leaf>)`) is deleted, and its declaration too, but the
##   declaration only when *all* of its stores were such stores, since a surviving
##   `y = f()` still needs `y` to exist. An initializer-less decl counts as a
##   deletion candidate for exactly this reason — the inliner's result temps are
##   all of that shape, and a leftover unused decl would still take a register.
##   A surviving un-rewritten read — the base of a field store, an `addr` operand,
##   a read past an invalidation — keeps `substituted` below the read count and
##   nothing is deleted. Deletions are replaced by an empty `.` statement (a no-op
##   in the statement list).
## * `(ret x)`'s operand is a value and is walked like any other read. `break`'s
##   is a LABEL and is not.

import std / [tables, sets, hashes, assertions, os, syncio]
when defined(nimony):
  import std / envvars   # host Nim's `os` re-exports it; Nimony's does not
when not defined(nimony):
  import std / exitprocs
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

# ---- COPYPROP_STATS -------------------------------------------------------
# `COPYPROP_STATS=1` makes each process print, at exit, how many copy relations
# the pass established and how many it DECLINED, bucketed by why. The pass is
# cheap to write and easy to fool: it is only worth what it fires on, and the
# only way to know that is to count. Per-process totals; the build is
# multi-process, so sum them (`| awk`) or run `-j1`.

type
  MissKind = enum
    mkVarSymInit,      ## recorded at a decl:   `(var :y . T x)`
    mkAsgnCopy,        ## recorded at a store:  `(asgn y x)`
    mkScopeBlocked,    ## `(asgn y x)` declined: `x` is declared in a deeper `(scope …)`
    mkVarNoInit,       ## `(var :y . T .)` — nothing to learn AT THE DECL (the inliner's
                       ## shape; the value arrives by a later `asgn`)
    mkSrcNotLocal,     ## source is not a propagatable local/param
    mkRetOperand       ## `(ret x)` with a local `x` — a value use inside a return

var statsOn = existsEnv("COPYPROP_STATS")
var gStats: array[MissKind, int]
var gSubst = 0        ## symbol uses actually rewritten
var gDeleted = 0      ## decls deleted as dead

proc note(k: MissKind) {.inline.} =
  if statsOn: inc gStats[k]

# Counting is dual-compiled (it is one `inc` behind a flag); REPORTING is host
# Nim only. `getCurrentProcessId` and `addExitProc` have no counterpart in the
# bootstrap stdlib, and an exit hook is exactly what a Nimony-built shoggoth
# would not have to hang the dump off anyway.
when not defined(nimony):
  proc dumpCopyPropStats() {.noconv.} =
    if not statsOn: return
    stderr.writeLine "COPYPROP_STATS pid=" & $getCurrentProcessId() &
      " recorded(var-sym-init)=" & $gStats[mkVarSymInit] &
      " recorded(asgn-copy)=" & $gStats[mkAsgnCopy] &
      " scope-blocked=" & $gStats[mkScopeBlocked] &
      " var-no-init=" & $gStats[mkVarNoInit] &
      " src-not-local=" & $gStats[mkSrcNotLocal] &
      " ret-value-walked=" & $gStats[mkRetOperand] &
      " substitutions=" & $gSubst & " decls-deleted=" & $gDeleted

  if statsOn: addExitProc dumpCopyPropStats

# ---- context --------------------------------------------------------------

type
  SymProps = object
    ## Everything the pass knows about one symbol. ONE table keyed by `SymId`
    ## rather than a table (or set) per attribute: every site that touches a
    ## symbol wants several of these at once, and this pass runs on every proc
    ## body of every module in every build, so seven hash lookups per symbol
    ## touch is not a rounding error.
    name: string                     ## textual name, for synthesizing a substitution
    uses: int                        ## every `Symbol` token, reads AND bare-symbol writes
    writes: int                      ## occurrences as a BARE-symbol assignment target.
                                     ## `preScan` counts those in `uses` too, but a write
                                     ## is never substituted (`trAsgn` deliberately leaves
                                     ## the LHS alone), so the "every use was rewritten
                                     ## away" test must compare against `uses - writes`.
                                     ## `(asgn (dot y f) …)` is NOT counted: there `y` is
                                     ## read as a base, and correctly keeps `y` alive.
    substituted: int                 ## uses we rewrote away
    declPos: int                     ## position of a DELETABLE decl, else -1
    declDepth: int                   ## `(scope …)` nesting depth of the declaration
    isLocal: bool                    ## declared by a local `(var …)`, or a parameter
    addrTaken: bool                  ## its own storage is addressed somewhere
    pureAsgns: seq[int]              ## positions of `(asgn y <leaf>)` — a store with no
                                     ## side effect, hence deletable once `y` is dead

  Context = object
    orig: ptr TokenBuf
    copyOf: Tracker[SymId, SymId]    ## y -> x: y currently aliases x
    syms: Table[SymId, SymProps]
    patchset: Patchset
    synth: seq[TokenBuf]
    dotBuf: TokenBuf                 ## a single `.` token: replaces a dead decl/store

proc createContext(orig: ptr TokenBuf): Context =
  result = Context(orig: orig,
          copyOf: initTracker[SymId, SymId](),
          syms: initTable[SymId, SymProps](),
          patchset: initPatchset(orig),
          synth: @[],
          dotBuf: createTokenBuf(2, orig[].pool, orig[].tags))
  result.dotBuf.addDotToken()

proc prop(c: var Context; s: SymId): var SymProps {.inline.} =
  ## The symbol's record, created on first touch. `declPos` starts at -1 ("not a
  ## deletion candidate") so a plain default is the right zero value.
  mgetOrPut(c.syms, s, SymProps(declPos: -1))

# The three queries below read a record that may not exist yet. `withValue` is
# the one-lookup form for that, but it does not exist in the bootstrap (Nimony)
# stdlib, and `getOrDefault` is no substitute either: `SymProps` holds a `string`
# and a `seq`, so returning it by value would deep-copy both on every query.
# `hasKey` plus a by-`var` accessor costs a second hash and copies nothing.
when defined(nimony):
  template known(c: var Context; s: SymId): untyped = getOrQuit(c.syms, s)
else:
  template known(c: var Context; s: SymId): untyped = c.syms[s]

proc isLocalSym(c: var Context; s: SymId): bool {.inline.} =
  c.syms.hasKey(s) and c.known(s).isLocal

proc isAddrTaken(c: var Context; s: SymId): bool {.inline.} =
  c.syms.hasKey(s) and c.known(s).addrTaken

proc propagatable(c: var Context; s: SymId): bool {.inline.} =
  ## A symbol whose VALUE may be carried to another name: a local or parameter
  ## whose own storage is never addressed, so only a direct assignment to it can
  ## change it (a call cannot, which is what makes `trCallStmt` invalidate nothing).
  if not c.syms.hasKey(s): return false
  let p = addr c.known(s)
  result = p.isLocal and not p.addrTaken

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
  buf.addSymUse c.prop(root).name
  c.synth.add ensureMove(buf)
  c.patchset.addSubst(pos, cursorAt(c.synth[idx], 0))
  c.prop(symId(n)).substituted += 1
  if statsOn: inc gSubst

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward
proc trExpr(c: var Context; n: var Cursor)   # forward
proc trAddrOperand(c: var Context; n: var Cursor)   # forward

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
      if n.kind == SymbolDef: nameSym = symId(n)
      skip n                             # name
    if n.hasMore: skip n                 # pragmas
    if n.hasMore: skip n                 # type
    if n.hasMore:
      let initStart = n
      let initKind = initStart.kind
      let initSym = if initKind == Symbol: symId(initStart) else: SymId(0)
      trExpr(c, n)                       # propagate inside the initializer
      if isLocal and nameSym != SymId(0) and not c.isAddrTaken(nameSym):
        # Record the copy relation for a plain local-symbol initializer.
        if initSym != SymId(0) and c.propagatable(initSym) and
           c.prop(initSym).declDepth <= c.prop(nameSym).declDepth:
          c.copyOf[nameSym] = resolve(c, initSym)
          note mkVarSymInit
        elif initKind == DotToken:
          note mkVarNoInit
        elif initSym != SymId(0):
          note mkSrcNotLocal
        # Deletable if its initializer is a side-effect-free leaf: dead once all
        # its uses are rewritten away (copy) or it was never used (pure store).
        # An initializer-LESS decl qualifies too — the inliner's result temps are
        # all of that shape (`(var :y . T .)` plus a later `asgn`), and once every
        # read of `y` is rewritten away and every store to it deleted, the bare
        # decl is an unused variable that would still take a register.
        if initKind in LeafKinds or initKind == DotToken:
          c.prop(nameSym).declPos = defPos
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
  if haveSecond:
    var r = rhs
    trExpr(c, r)                         # propagate inside the value
  # The write target's base symbol changed value — invalidate it (and its
  # aliases). We deliberately do NOT substitute inside the LHS: its base names a
  # storage location, and aliasing two locals' *values* does not alias their
  # storage.
  let haveLhs = if isStore: haveSecond else: haveFirst
  # A bare-symbol target that is propagatable: the statement may establish a copy
  # and may itself become deletable. This is the shape hexer's inliner emits —
  # it declares the inlined body's result temp WITHOUT an initializer (the body
  # may return from several points, so the value arrives by assignment before the
  # return label), so `trVar` above never sees a copy to record.
  var newSrc = SymId(0)
  if haveLhs and haveSecond and lhs.kind == Symbol and c.propagatable(symId(lhs)):
    let l = symId(lhs)
    if rhs.kind == Symbol and c.propagatable(symId(rhs)) and
       c.prop(symId(rhs)).declDepth <= c.prop(l).declDepth:
      # Resolve BEFORE invalidating: `invalidate(l)` clears every entry whose
      # VALUE is `l`, which would otherwise silently drop the chain we are about
      # to read (`y = x` where `x` was itself recorded as a copy of `y`).
      newSrc = resolve(c, symId(rhs))
      note mkAsgnCopy
    elif rhs.kind == Symbol and c.propagatable(symId(rhs)):
      note mkScopeBlocked
    elif rhs.kind == Symbol:
      note mkSrcNotLocal
    if rhs.kind in LeafKinds:
      # No side effect in the value, so the whole store dies with `l`.
      c.prop(l).pureAsgns.add asgnPos
  if haveLhs:
    invalidate(c, rootOf(lhs))
  if newSrc != SymId(0) and newSrc != symId(lhs):
    c.copyOf[symId(lhs)] = newSrc

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
  ## `clearAll` afterwards is right — nothing on this path follows. Skipping the
  ## node WHOLESALE was not: `(ret x)`'s operand is a VALUE, so a copy has to be
  ## substituted into it like any other read, and while it was skipped `preScan`
  ## still counted that `Symbol` in `uses` — so `substituted == uses - writes`
  ## could never hold for a local that appears in a `ret`, and its decl could
  ## never be deleted either. Only `ret` is walked: `break`'s operand is a LABEL,
  ## and substituting a symbol there would rewrite the jump target.
  if n.stmtKind == RetS:
    if statsOn:
      let v = child0(n)
      if v.kind == Symbol and c.isLocalSym(symId(v)): note mkRetOperand
    n.into:
      while n.hasMore: trExpr(c, n)
  else:
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

proc preScan(c: var Context; n: Cursor; depth = 0) =
  case n.kind
  of Symbol:
    let s = symId(n)
    let p = addr c.prop(s)
    p.name = symName(n)
    p.uses += 1
  of SymbolDef:
    c.prop(symId(n)).name = symName(n)
  of TagLit:
    if n.stmtKind == VarS:
      let nameCur = child0(n)
      if nameCur.kind == SymbolDef:
        let p = addr c.prop(symId(nameCur))
        p.isLocal = true
        p.declDepth = depth
    if n.stmtKind in {AsgnS, StoreS}:
      # A bare-symbol assignment target: a WRITE, though `preScan` also counts it
      # as a `Symbol` token above. `(asgn (dot y f) …)` is deliberately NOT counted
      # — that `y` is read as a base, is never substituted, and so correctly keeps
      # `y` from ever looking dead.
      var lhs = child0(n)
      if n.stmtKind == StoreS: skip lhs      # `(store src dest)` — dest is 2nd
      if lhs.kind == Symbol:
        c.prop(symId(lhs)).writes += 1
    if n.exprKind in AddrKinds:
      # Only a local whose OWN storage is addressed is excluded from copy prop. An
      # `addr((*p).f)` addresses the pointee (computed from p's value), so `p` stays
      # propagatable — `addrRootOf` stops at the `deref`/`pat` and returns SymId(0).
      let s = addrRootOf(child0(n))
      if s != SymId(0): c.prop(s).addrTaken = true
    let inner = if n.stmtKind == ScopeS: depth + 1 else: depth
    var m = n
    m.loopInto:
      preScan(c, m, inner)
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
        let p = addr c.prop(symId(nameCur))
        p.isLocal = true
        p.name = symName(nameCur)
        p.declDepth = 0                  # a parameter outlives every body scope
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
  # Dead-store / dead-binding elimination. A symbol is dead when every READ of it
  # was rewritten away — `uses` counts bare-symbol writes too, and those are never
  # substituted, so the test is against `uses - writes`. A surviving un-rewritten
  # read (a base of a field store, an `addr` operand, a read past an invalidation)
  # keeps `substituted` below it and everything here is skipped.
  #
  # Then: every side-effect-free store to it dies, and its DECL dies too — but the
  # decl only when *all* of its stores were such stores (`pureAsgns.len == writes`),
  # since a surviving `y = f()` still needs `y` to exist.
  for s, p in ctx.syms.mpairs:
    if p.substituted != p.uses - p.writes: continue
    for pos in p.pureAsgns:
      ctx.patchset.addSubst(pos, cursorAt(ctx.dotBuf, 0))
    if p.declPos >= 0 and p.pureAsgns.len == p.writes:
      ctx.patchset.addSubst(p.declPos, cursorAt(ctx.dotBuf, 0))
      if statsOn: inc gDeleted
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

  # ---- the shapes hexer's inliner actually emits -----------------------------

  block inliner_shape_decl_asgn_copy_ret:
    # An inlined body may return from several points, so its result temp is
    # DECLARED without an initializer and assigned before the return label. The
    # copy into the caller's destination is therefore an `asgn`, not a `var`
    # binding — and the last read is a `ret` operand. Both had to be handled for
    # this to collapse at all.
    chk(
      "(stmts (var :result.0.M . . .) (var :t.0.M . . .) " &
      "(scope (asgn t.0.M (call f.0.M)) (lab :retlab.0.M)) " &
      "(asgn result.0.M t.0.M) (ret result.0.M))",
      "(stmts . (var :t.0.M . . .) " &
      "(scope (asgn t.0.M (call f.0.M)) (lab :retlab.0.M)) . " &
      "(ret t.0.M))")

  block copy_into_ret_operand:
    # `(ret y)` is a value use like any other.
    chk(
      "(stmts (var :x.0.M . . (call f.0.M)) (var :y.0.M . . x.0.M) (ret y.0.M))",
      "(stmts (var :x.0.M . . (call f.0.M)) . (ret x.0.M))")

  block asgn_copy_source_reassigned_blocks:
    # The `asgn`-established copy obeys the same invalidation as a `var` one.
    assertUnchanged(
      "(stmts (var :x.0.M . . (call f.0.M)) (var :y.0.M . . .) " &
      "(asgn y.0.M x.0.M) (asgn x.0.M (call g.0.M)) (call use.0.M y.0.M))")

  block source_in_inner_scope_blocks:
    # A `(scope …)` is a REAL scope — the C backend emits `{ }` and arkham frees
    # its locals' registers at the close. Substituting `inner` into a use that
    # outlives the scope would name a dead variable, so the copy is not recorded
    # when the source is declared deeper than the destination.
    assertUnchanged(
      "(stmts (var :outer.0.M . . .) " &
      "(scope (var :inner.0.M . . (call f.0.M)) (asgn outer.0.M inner.0.M)) " &
      "(call use.0.M outer.0.M))")

  block impure_store_keeps_the_decl:
    # `y = f()` has a side effect and cannot be deleted, so `y` must keep its decl
    # even though the decl itself carries no initializer.
    assertUnchanged(
      "(stmts (var :y.0.M . . .) (asgn y.0.M (call f.0.M)) (call use.0.M y.0.M))")

  block field_store_base_keeps_the_var_alive:
    # `(asgn (dot y f) …)` READS `y` as a base and is never substituted, so `y`
    # never looks dead and neither its decl nor the store may go.
    assertUnchanged(
      "(stmts (var :y.0.M . . .) (asgn (dot y.0.M fld.0.M 0) (call f.0.M)))")

  block dead_pure_store_and_decl_both_go:
    # Nothing ever reads `y`: the store is side-effect free and the decl unused.
    chk(
      "(stmts (var :y.0.M . . .) (asgn y.0.M 42) (call use.0.M 1))",
      "(stmts . . (call use.0.M 1))")

  echo "copyprop.nim: all self-tests passed"
