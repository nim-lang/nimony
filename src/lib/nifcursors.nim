#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Cursors into token streams. Suprisingly effective even for more complex algorithms.
##
## Memory layout
## =============
##
## A `TokenBuf` always owns a contiguous data buffer pointed at by `b.data`.
## Cursors borrow into that buffer. To make borrowing safe across mutations
## of `b`, an extra reference-counted control block — the `CursorOwner`
## header — coordinates sharing.
##
## **Detached state** (no live Cursors). `b.owner == nil`; the TokenBuf
## directly owns its data buffer:
##
## ```
##   TokenBuf b
##   ┌──────────────┐         data buffer (alloc #1)
##   │ data ────────┼──────▶ ┌─────────┐
##   │ len, cap     │        │ tokens  │
##   │ owner: nil   │        │  ...    │
##   └──────────────┘        └─────────┘
## ```
##
## **Attached state** (one or more live Cursors). `beginRead` /
## `cursorAt` lazily allocate a small `CursorOwnerObj` header (alloc #2)
## whose `data` field aliases `b.data`. `rc` counts: 1 for the TokenBuf
## plus 1 per live Cursor.
##
## ```
##   TokenBuf b
##   ┌──────────────┐         data buffer (alloc #1)
##   │ data ────────┼──────▶ ┌─────────┐
##   │ len, cap     │  ┌─▶   │ tokens  │
##   │ owner ───────┼──┼┐    │  ...    │
##   └──────────────┘  ││    └─────────┘
##                     ││
##           Cursor c1 ││         CursorOwner (alloc #2)
##           ┌────────┐││         ┌─────────┐
##           │ owner ─┼┘│         │ rc: 3   │
##           │ p ─────┼─┼─────▶   │ data ───┼─── points at alloc #1
##           │ rem    │ │         └─────────┘
##           └────────┘ │              ▲
##                      │              │
##           Cursor c2  │              │
##           ┌────────┐ │              │
##           │ owner ─┼─┴──────────────┘
##           │ p      │
##           │ rem    │
##           └────────┘
## ```
##
## Allocations #1 and #2 are independent. Freeing the header (#2) does
## not touch the data buffer (#1) and vice versa. `decRcAndFree(owner)`
## frees both — but only when `rc` decrements to 0.
##
## Mutations and COW
## -----------------
##
## `prepareMutation` is called before any mutation that would invalidate
## existing cursors. Two cases:
##
## - `rc == 1`: only the TokenBuf holds a ref; no cursor is reading.
##   Free the header (#2) and revert to detached state — `b.data` keeps
##   pointing at #1, which the TokenBuf now owns directly.
##
## - `rc > 1`: cursors still read #1. Allocate a fresh data buffer and
##   `copyMem` into it; release one ref on the old owner via
##   `decRcAndFree`. Old cursors continue reading the old #1 (kept alive
##   by their own rc refs); the TokenBuf now owns the new buffer.
##
## Hot-path callers can stay on the no-copy branch by calling
## `endRead(c)` on cursors before mutating the parent buffer, and
## `expectUnique(b)` asserts that contract in debug builds.

import std / [assertions, syncio]
import nifreader, nifstreams, bitabs, lineinfos, vfs
export vfs.FileWriteMode

include compat2

when defined(prepMutStats):
  var cowFastCount*: int
  var cowSlowCount*: int
  var cowSlowBytes*: int

type
  Storage = ptr UncheckedArray[PackedToken]
  CursorOwner = ptr CursorOwnerObj
  CursorOwnerObj = object
    rc: int          ## 1 (TokenBuf) + number of live Cursors sharing this control block
    data: Storage    ## the shared backing storage; aliases TokenBuf.data when attached

  Cursor* = object
    owner: CursorOwner
    p: ptr PackedToken
    rem: int

template decRcAndFree(owner: CursorOwner) =
  dec owner.rc
  if owner.rc == 0:
    if owner.data != nil: dealloc(owner.data)
    dealloc(owner)

when defined(nimAllowNonVarDestructor) and defined(gcDestructors):
  proc `=destroy`*(c: Cursor) {.inline.} =
    if c.owner != nil:
      decRcAndFree(c.owner)
else:
  proc `=destroy`*(c: var Cursor) {.inline.} =
    if c.owner != nil:
      decRcAndFree(c.owner)

proc `=wasMoved`*(c: var Cursor) {.inline.} =
  c.owner = nil
  c.p = nil
  c.rem = 0

proc `=copy`*(dest: var Cursor; src: Cursor) {.inline.} =
  if dest.owner != src.owner or dest.p != src.p:
    `=destroy`(dest)
    if src.owner != nil: inc src.owner.rc
    dest.owner = src.owner
    dest.p = src.p
    dest.rem = src.rem

proc `=dup`*(src: Cursor): Cursor {.nodestroy, inline.} =
  result = Cursor(owner: src.owner, p: src.p, rem: src.rem)
  if result.owner != nil: inc result.owner.rc

let
  ErrToken = [parLeToken(ErrT, NoLineInfo),
              parRiToken(NoLineInfo)]

proc errCursor*(): Cursor =
  Cursor(p: addr ErrToken[0], rem: 2)

proc fromBuffer*(x: openArray[PackedToken]): Cursor {.inline.} =
  Cursor(p: addr(x[0]), rem: x.len)

proc setSpan*(c: var Cursor; beyondLast: Cursor) {.inline.} =
  c.rem = (cast[int](beyondLast.p) - cast[int](c.p)) div sizeof(PackedToken)

proc load*(c: Cursor): PackedToken {.inline.} =
  assert c.p != nil and c.rem > 0
  c.p[]

proc kind*(c: Cursor): NifKind {.inline.} =
  when defined(virtualParRi):
    ## A bounded cursor (from `into`/`enterScope`) reports a virtual `ParRi`
    ## once its scope is consumed (`rem == 0`), even though the physical
    ## close token is elided. This keeps classic sentinel idioms
    ## (`n.kind == ParRi`, `typeKind` yielding `NoType` at a subtree's end)
    ## working unchanged. Only `leaveScope`/`into` may move past a virtual
    ## close — `inc` still asserts.
    if c.rem <= 0: ParRi
    else: c.load.kind
  else:
    c.load.kind

proc info*(c: Cursor): PackedLineInfo {.inline.} = c.load.info
proc setInfo*(c: Cursor; info: PackedLineInfo) {.inline.} = c.p[].info = info

proc litId*(c: Cursor): StrId {.inline.} = nifstreams.litId(c.load)
proc symId*(c: Cursor): SymId {.inline.} = nifstreams.symId(c.load)

proc charLit*(c: Cursor): char {.inline.} = nifstreams.charLit(c.load)

proc intId*(c: Cursor): IntId {.inline.} = nifstreams.intId(c.load)
proc uintId*(c: Cursor): UIntId {.inline.} = nifstreams.uintId(c.load)
proc floatId*(c: Cursor): FloatId {.inline.} = nifstreams.floatId(c.load)
proc tagId*(c: Cursor): TagId {.inline.} = nifstreams.tagId(c.load)

proc tag*(c: Cursor): TagId {.inline.} = nifstreams.tag(c.load)

proc uoperand*(c: Cursor): uint32 {.inline.} = nifstreams.uoperand(c.load)
proc soperand*(c: Cursor): int32 {.inline.} = nifstreams.soperand(c.load)

proc getInt28*(c: Cursor): int32 {.inline.} = nifstreams.getInt28(c.load)

proc inc*(c: var Cursor) {.inline.} =
  when defined(virtualParRi):
    assert c.rem != 0, "advancing past end of scope"
    c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
    if c.rem > 0: dec c.rem
  else:
    assert c.rem > 0
    c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
    dec c.rem

proc consumeParRi*(c: var Cursor) {.inline.} =
  ## Advance past a ParRi token. Under `-d:virtualParRi` the real ParRi
  ## may sit at the bounded scope's tail without being counted in `rem`
  ## (under `-d:preserveRealParRi`), so plain `inc` would trip its
  ## `rem != 0` guard — this advances the pointer directly instead.
  when defined(virtualParRi):
    # Must be a *physical* ParRi: a virtual close (rem == 0 at a sealed
    # scope's end) has no token to advance past — use `leaveScope`/`into`
    # for those. `c.p[]` is read directly since `load` requires rem > 0
    # but under `-d:preserveRealParRi` the trailing ParRi is not counted.
    when defined(preserveRealParRi):
      assert c.p != nil and c.p[].kind == ParRi, "consumeParRi: cursor not at ParRi"
    else:
      assert c.rem > 0 and c.p[].kind == ParRi, "consumeParRi: cursor not at real ParRi"
    c.p = cast[ptr PackedToken](cast[uint](c.p) + sizeof(PackedToken).uint)
    if c.rem > 0: dec c.rem
  else:
    assert c.kind == ParRi, "consumeParRi: cursor not at ParRi"
    inc c

# `setTag(PackedToken, TagId)` lives in nifstreams now (gated by `-d:virtualParRi`)

proc peekPastEnd*(n: Cursor): Cursor =
  ## A cursor positioned at the token that physically follows the current
  ## bounded scope's (real or elided) close; `n` must sit at the scope's
  ## end. For read-only peeks at a following sibling that the caller KNOWS
  ## exists (e.g. the body slot after a type decl's pragmas): the result
  ## carries a loose bound, so treat it strictly as a peek.
  result = n
  when defined(virtualParRi):
    if result.rem > 0:
      # overflow scope: a real ParRi terminates it
      assert result.load.kind == ParRi, "peekPastEnd: not at scope end"
      result.p = cast[ptr PackedToken](cast[uint](result.p) + sizeof(PackedToken).uint)
    # else: the close is elided; `p` already points at the following token
    result.rem = high(int)
  else:
    assert result.kind == ParRi, "peekPastEnd: not at scope end"
    inc result

proc unsafeDec*(c: var Cursor) {.inline.} =
  c.p = cast[ptr PackedToken](cast[uint](c.p) - sizeof(PackedToken).uint)
  inc c.rem

proc `+!`*(c: Cursor; diff: int): Cursor {.inline.} =
  assert diff <= c.rem
  result = Cursor(owner: c.owner,
     p: cast[ptr PackedToken](cast[uint](c.p) + diff.uint * sizeof(PackedToken).uint),
     rem: c.rem - diff)
  if result.owner != nil: inc result.owner.rc

proc cursorIsNil*(c: Cursor): bool {.inline.} =
  result = c.p == nil

proc hasCurrentToken*(c: Cursor): bool {.inline.} =
  ## True if `c` points at a readable token. Validator-only — exists so
  ## the grammar checker can survive malformed input that runs the cursor
  ## off the end. Regular traversal code should use `hasMore` instead;
  ## it accepts well-formed input where every open paren has its matching
  ## close, and stays a token-kind check rather than a pointer check.
  result = c.p != nil and c.rem > 0

when defined(virtualParRi):
  template subtreeSpan(n: PackedToken): uint32 =
    ## Number of tokens that `skip` past this token would consume.
    ## Sealed ParLe (jump < MaxJump): 1 + jump (or 2 + jump under
    ## `-d:preserveRealParRi`). Overflow (jump == MaxJump): returns 0
    ## to signal "caller takes the slow balanced walk".
    case n.kind
    of ParLe:
      let j = jump(n)
      if j == MaxJump: 0'u32
      else:
        when defined(preserveRealParRi): 2'u32 + j
        else: 1'u32 + j
    else: 1'u32

proc skip*(c: var Cursor) =
  when defined(virtualParRi):
    if c.kind == ParLe:
      let span = subtreeSpan(c.load)
      if span > 0:
        c.p = cast[ptr PackedToken](cast[uint](c.p) + uint(span) * sizeof(PackedToken).uint)
        if c.rem > 0:
          let s = int(span)
          c.rem = if c.rem >= s: c.rem - s else: 0
        return
      # Overflow tag: balanced walk (real ParRi balances; nested sealed
      # tags still skip via their jump fields).
      var depth = 1
      inc c   # past overflow tag
      while depth > 0:
        if c.kind == ParRi:
          dec depth
          inc c
        elif c.kind == ParLe:
          let innerSpan = subtreeSpan(c.load)
          if innerSpan > 0:
            c.p = cast[ptr PackedToken](cast[uint](c.p) + uint(innerSpan) * sizeof(PackedToken).uint)
            if c.rem > 0:
              let s = int(innerSpan)
              c.rem = if c.rem >= s: c.rem - s else: 0
          else:
            inc depth
            inc c
        else:
          inc c
    else:
      inc c
  else:
    if c.kind == ParLe:
      var nested = 0
      while true:
        inc c
        if c.kind == ParRi:
          if nested == 0: break
          dec nested
        elif c.kind == ParLe: inc nested
    inc c

proc skipUntilEnd*(c: var Cursor) =
  ## skips `c` until an unmatched ParRi is found, then returns without skipping the ParRi
  when defined(virtualParRi):
    # Requires a *bounded* cursor (from `into`/`enterScope`): whole-subtree
    # skips land exactly on the scope's end — the virtual close (rem == 0)
    # or the real ParRi of an overflow scope. (`hasMore` is not yet
    # declared at this point in the file, hence the spelled-out condition.)
    while c.rem > 0 and c.load.kind != ParRi: skip c
  else:
    var nested = 0
    while true:
      if c.kind == ParRi:
        if nested == 0:
          break
        dec nested
      elif c.kind == ParLe:
        inc nested
      inc c

# ── Traversal templates ──────────────────────────────────────────────────
# Pure traversal helpers for reading/analyzing a tree without producing output.

template hasMore*(n: Cursor): bool =
  ## True while there are more tokens to read in the current scope. Safe
  ## at end-of-buffer (`rem == 0`) — returns false rather than dereferencing.
  when defined(virtualParRi):
    n.rem > 0 and n.kind != ParRi
  else:
    n.kind != ParRi

proc endInfo*(n: Cursor): PackedLineInfo {.inline.} =
  ## Line info at the current position, safe to call when `n` may sit at
  ## the end of a bounded scope. Classic mode reads the token there (the
  ## closing ParRi); under `-d:virtualParRi` that closer is elided, so
  ## this yields the next physical token's info if any, else NoLineInfo.
  when defined(virtualParRi):
    if n.rem > 0: n.info else: NoLineInfo
  else:
    n.info

type
  CursorScope* = object
    ## Manual counterpart to `into` for resumable traversals (iterators
    ## that pause mid-walk and cannot use a template's scoped body).
    head: Cursor

proc enterScope*(n: var Cursor): CursorScope =
  ## Advances past the ParLe at `n` and bounds the cursor to the scope's
  ## body. While inside, `n.hasMore` is false exactly when the body is
  ## consumed; then call `leaveScope` to advance past the (real or
  ## virtual) closing `)` and restore the outer scope's bound.
  assert n.kind == ParLe, "enterScope requires cursor at ParLe"
  result = CursorScope(head: n)
  when defined(virtualParRi):
    let j = jump(n.load)
    let isOverflow = j == MaxJump
    n.p = cast[ptr PackedToken](cast[uint](n.p) + sizeof(PackedToken).uint)
    n.rem = if isOverflow: high(int) else: int(j)
  else:
    inc n

proc leaveScope*(n: var Cursor; scope: CursorScope) =
  ## Leaves a scope opened by `enterScope`; the body must have been fully
  ## consumed (`not n.hasMore`).
  when defined(virtualParRi):
    if n.rem > 0:
      # overflow scope: terminated by its real ParRi
      assert n.kind == ParRi, "leaveScope: scope did not end at ParRi"
      n.p = cast[ptr PackedToken](cast[uint](n.p) + sizeof(PackedToken).uint)
    let consumed = int((cast[uint](n.p) - cast[uint](scope.head.p)) div sizeof(PackedToken).uint)
    let savedRem = scope.head.rem
    n.rem = if savedRem >= consumed: savedRem - consumed else: 0
  else:
    assert n.kind == ParRi, "leaveScope: scope did not end at ParRi"
    inc n

template into*(n: var Cursor; body: untyped) =
  ## Enters the current ParLe node, runs `body` to process the children,
  ## then advances past the (real or virtual) closing `)`.
  ##
  ## Under `-d:virtualParRi` this sets a bounded `rem` from the parle's
  ## `jump` field so the body's `hasMore` checks terminate cheaply. The
  ## body must consume all children (leaving `rem == 0` for sealed
  ## scopes, or landing on a real `ParRi` for overflow scopes); the
  ## closing `ParRi` (real or elided) is consumed by the epilogue.
  ##
  ## Overflow scopes (`jump == MaxJump`, i.e. subtree larger than the
  ## 19-bit jump field can encode) seed `rem` with `high(int)` instead
  ## of the exact count. That keeps `hasMore` (`rem > 0 and kind != ParRi`)
  ## working uniformly — the loop falls out of the `rem` decrement before
  ## it ever underflows and terminates on the real `ParRi` instead.
  ## Using `-1` here was the bug that broke `detectToplevelDecls` on
  ## modules whose top-level `(stmts …)` exceeded `MaxJump` tokens.
  assert n.kind == ParLe, "into requires cursor at ParLe"
  when defined(virtualParRi):
    let savedRem = n.rem
    let savedP   = n.p
    let j        = jump(n.load)
    let isOverflow = j == MaxJump
    n.p = cast[ptr PackedToken](cast[uint](n.p) + sizeof(PackedToken).uint)
    n.rem = if isOverflow: high(int) else: int(j)
    body
    if isOverflow:
      assert n.kind == ParRi, "into: overflow scope did not end at real ParRi"
      n.p = cast[ptr PackedToken](cast[uint](n.p) + sizeof(PackedToken).uint)
    else:
      assert n.rem == 0, "into: body did not consume all children of sealed scope"
      when defined(preserveRealParRi):
        if n.kind == ParRi:
          n.p = cast[ptr PackedToken](cast[uint](n.p) + sizeof(PackedToken).uint)
    # Restore the outer scope's bound: subtract what the body consumed.
    # `savedRem` is always non-negative now (overflow scopes contribute
    # `high(int)` rather than `-1`), so the clamping handles both cases.
    let consumed = int((cast[uint](n.p) - cast[uint](savedP)) div sizeof(PackedToken).uint)
    n.rem = if savedRem >= consumed: savedRem - consumed else: 0
  else:
    inc n          # skip opening tag
    body
    assert n.kind == ParRi, "into: body must consume all children"
    inc n          # skip closing ParRi

template peekInto*(n: var Cursor; body: untyped) =
  ## Like `into`, but does **not** require `body` to consume every child.
  ## Enters the current `ParLe`, runs `body` (which may consume only some of
  ## the children and stop early), then skips whatever `body` did not read
  ## and advances `n` past the closing `)`. Only the *unconsumed* remainder
  ## is walked — nothing is traversed twice. Like `into`, `body` must stop
  ## at a child boundary (advance only across whole subtrees, e.g. via
  ## `skip`); it still sees a bounded scope, so `hasMore` terminates
  ## correctly inside it.
  assert n.kind == ParLe, "peekInto requires cursor at ParLe"
  when defined(virtualParRi):
    # O(1) finish: rewind to the opening tag and skip via its jump field.
    let savedParLe = n
    let j = jump(n.load)
    let isOverflow = j == MaxJump
    n.p = cast[ptr PackedToken](cast[uint](n.p) + sizeof(PackedToken).uint)
    n.rem = if isOverflow: high(int) else: int(j)
    body
    n = savedParLe
    skip n
  else:
    inc n          # skip opening tag
    body
    while n.kind != ParRi: skip n   # mop up the unconsumed children
    inc n          # skip closing ParRi

template loopInto*(n: var Cursor; body: untyped) =
  ## Enters a node, iterates all children, then advances past `)`.
  ## The body must advance `n` on every iteration.
  into n:
    while n.hasMore:
      body

proc rootOf*(c: Cursor): SymId =
  ## The access root of an lvalue: the first `Symbol` in the subtree at `c`
  ## — `x` in `x.f[i]` — or `SymId(0)` if there is none.
  result = SymId(0)
  var n = c
  if n.kind == Symbol:
    result = n.symId
  elif n.kind == ParLe:
    n.loopInto:
      if result == SymId(0):
        let inner = rootOf(n)
        if inner != SymId(0): result = inner
      skip n

template linearScan*(n: var Cursor; body: untyped) =
  ## Linearly scans the subtree rooted at `n`, visiting every nested `ParLe`
  ## node in document order and at all depths — the all-depth counterpart to
  ## `loopInto` (which visits direct children only). `body` runs with `n`
  ## positioned at each `ParLe`; it may inspect the node — reading children
  ## through a *copy* so as not to disturb the walk — and `break` to stop
  ## early, leaving `n` at the matching node. `body` must **not** advance
  ## `n`; the template walks it. Use for tag searches over a whole subtree,
  ## e.g. the type-hook pragma lookups.
  when defined(virtualParRi):
    # token-linear walk over the subtree's physical span; closes are elided
    # so the classic ParRi counting cannot terminate the loop
    if n.kind == ParLe:
      var togo = span(n) - 1
      inc n
      while togo > 0:
        if n.kind == ParLe:
          body
        inc n
        dec togo
  else:
    var nested = 0
    if n.kind == ParLe:
      inc nested; inc n
      while nested > 0:
        case n.kind
        of ParLe:
          body
          inc nested; inc n
        of ParRi:
          dec nested; inc n
        else:
          inc n

type
  TokenBuf* = object
    data: Storage
    len, cap: int
    owner: CursorOwner  ## nil = exclusive ownership of data.
                        ## non-nil = data is shared via CursorOwner with Cursors;
                        ## b.data aliases owner.data. TokenBuf holds one rc ref.
    when defined(virtualParRi):
      openTags: seq[int]
        ## Stack of buffer offsets pointing at every still-unsealed `ParLe`.
        ## `add(ParLe)` pushes; `add(ParRi)` pops and seals via `setJump`.
        ## When `count < MaxJump`, the matching ParRi is elided in default
        ## mode (kept under `-d:preserveRealParRi` for legacy walks).

proc `=copy`(dest: var TokenBuf; src: TokenBuf) {.error.}
proc `=wasMoved`(dest: var TokenBuf) {.inline.} =
  dest.data = nil
  dest.len = 0
  dest.cap = 0
  dest.owner = nil

when defined(nimAllowNonVarDestructor) and defined(gcDestructors):
  proc `=destroy`(dest: TokenBuf) {.inline.} =
    if dest.owner != nil:
      decRcAndFree(dest.owner)
    else:
      if dest.data != nil: dealloc(dest.data)
else:
  proc `=destroy`(dest: var TokenBuf) {.inline.} =
    if dest.owner != nil:
      decRcAndFree(dest.owner)
    else:
      if dest.data != nil: dealloc(dest.data)

template allocStorage(cap: int): Storage =
  ## Allocate backing storage for `cap` tokens, never requesting a zero- or
  ## sub-`FreeCell` block. `alloc(0)` (cap 0) — and any request below the
  ## allocator's `sizeof(FreeCell)` minimum, e.g. one 8-byte `PackedToken` —
  ## is undefined for Nim's default (non-`useMalloc`) allocator: it rounds the
  ## size through the small-block freelist and corrupts the heap. That fires
  ## `[SYSASSERT] rawAlloc: requested size too small` under `-d:useSysAssert`
  ## and segfaults in `rawAlloc`/`addToSharedFreeList` on some platforms (WSL2),
  ## while `-d:useMalloc` hides it because `malloc(0)` returns a valid pointer.
  ## Floor the request at 8 slots — the same minimum the grow path uses, so an
  ## empty buffer's first append needs no immediate realloc.
  cast[Storage](alloc(sizeof(PackedToken) * max(cap, 8)))

proc createTokenBuf*(cap = 100): TokenBuf =
  result = TokenBuf(data: allocStorage(cap), len: 0, cap: cap)

proc prepareMutation*(b: var TokenBuf) {.inline.} =
  ## Detach the buffer from its CursorOwner so the next mutation does
  ## not invalidate existing cursors. Two paths:
  ##
  ## - **No live cursors (rc == 1).** The TokenBuf is the only ref to the
  ##   owner header; no cursor reads the data anymore. Reuse the existing
  ##   data buffer in place — only the small owner header gets freed.
  ##   `b.data` already aliases `b.owner.data`, so we capture the
  ##   pointer, drop the header, and re-install it as the TokenBuf's
  ##   single-owner data pointer. No alloc, no copy.
  ##
  ## - **Live cursors (rc > 1).** Cursors still read the data; we must
  ##   detach by deep-copying so the old buffer stays valid for them.
  ##
  ## The fast path is what callers achieve by calling `endRead` on their
  ## cursors before the next mutation; without `endRead`, the slow COW
  ## still keeps things correct.
  if b.owner != nil:
    if b.owner.rc == 1:
      # Only the TokenBuf itself holds a ref; no live cursor reads the
      # buffer. Free the owner header (a separate small allocation) and
      # revert to single-owner mode. `b.data` stays valid — it points at
      # the data buffer, which is a different allocation from the header.
      # See the layout diagram at the top of the file.
      dealloc(b.owner)
      b.owner = nil
      when defined(prepMutStats): inc cowFastCount
    else:
      let newData = allocStorage(b.cap)
      copyMem(newData, b.data, sizeof(PackedToken) * b.len)
      decRcAndFree(b.owner)
      b.owner = nil
      b.data = newData
      when defined(prepMutStats):
        inc cowSlowCount
        cowSlowBytes += b.len * sizeof(PackedToken)

proc freeze*(b: var TokenBuf) {.inline.} = discard
proc thaw*(b: var TokenBuf) {.inline.} = discard

proc expectUnique*(b: var TokenBuf) {.inline.} =
  ## Hot-path mutation sites stamp this to assert that no cursor
  ## currently holds an rc ref against `b`. Catches a missing `endRead`
  ## that would otherwise silently force `prepareMutation` onto the
  ## copying path. No-op in release builds — COW remains the correctness
  ## floor, this is just a tripwire for performance regressions.
  when defined(debug):
    assert b.owner == nil or b.owner.rc == 1,
      "TokenBuf has live cursors; missing endRead before mutation"

proc beginRead*(b: var TokenBuf): Cursor =
  ## Returns a Cursor into the buffer. Creates a CursorOwner on first call
  ## (rc=2: one for TokenBuf, one for the returned Cursor).
  if b.owner == nil:
    b.owner = cast[CursorOwner](alloc0(sizeof(CursorOwnerObj)))
    b.owner.data = b.data
    b.owner.rc = 1  # 1 for TokenBuf
  inc b.owner.rc  # + 1 for the returned Cursor
  result = Cursor(owner: b.owner, p: addr(b.data[0]), rem: b.len)

proc endRead*(b: var TokenBuf) {.inline.} = discard

proc endRead*(c: var Cursor) {.inline.} =
  ## Release the cursor's rc ref against its owner buffer eagerly. After
  ## this call the cursor is moved-out (`c.owner == nil`); subsequent
  ## mutations on the underlying TokenBuf can take the fast no-copy path
  ## in `prepareMutation` if no other cursors are live.
  if c.owner != nil:
    decRcAndFree(c.owner)
  `=wasMoved`(c)

proc addRaw*(b: var TokenBuf; item: PackedToken) {.inline.} =
  ## Append a single token without sealing/elision. Under `-d:virtualParRi`
  ## this bypasses the open-tags stack — use when bulk-copying an already
  ## balanced span (opens and closes paired inside the copy). Otherwise
  ## identical to plain append.
  if b.owner != nil: prepareMutation(b)
  if b.len >= b.cap:
    b.cap = max(b.cap div 2 + b.cap, 8)
    b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
  b.data[b.len] = item
  inc b.len

proc add*(b: var TokenBuf; item: PackedToken) {.inline.} =
  if b.owner != nil: prepareMutation(b)
  when defined(virtualParRi):
    case item.kind
    of ParLe:
      if b.len >= b.cap:
        b.cap = max(b.cap div 2 + b.cap, 8)
        b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
      b.data[b.len] = item
      b.openTags.add b.len
      inc b.len
    of ParRi:
      var elide = false
      if b.openTags.len > 0:
        let openPos = b.openTags.pop()
        let count = uint32(b.len - openPos - 1)
        if count < MaxJump:
          b.data[openPos].setJump count
          elide = true
        else:
          b.data[openPos].setJump MaxJump
      when defined(preserveRealParRi):
        if b.len >= b.cap:
          b.cap = max(b.cap div 2 + b.cap, 8)
          b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
        b.data[b.len] = item
        inc b.len
      else:
        if not elide:
          if b.len >= b.cap:
            b.cap = max(b.cap div 2 + b.cap, 8)
            b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
          b.data[b.len] = item
          inc b.len
    else:
      if b.len >= b.cap:
        b.cap = max(b.cap div 2 + b.cap, 8)
        b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
      b.data[b.len] = item
      inc b.len
  else:
    if b.len >= b.cap:
      b.cap = max(b.cap div 2 + b.cap, 8)
      b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
    b.data[b.len] = item
    inc b.len

proc len*(b: TokenBuf): int {.inline.} = b.len

proc debugOpenTags*(b: TokenBuf): seq[int] =
  ## debugging helper: the current open-tag stack (classic mode has none)
  when defined(virtualParRi):
    b.openTags
  else:
    @[]

when defined(nimony):
  proc `[]`*(b: TokenBuf; i: int): var PackedToken {.inline.} =
    assert i >= 0 and i < b.len
    result = b.data[i]
else:
  proc `[]`*(b: TokenBuf; i: int): PackedToken {.inline.} =
    assert i >= 0 and i < b.len
    result = b.data[i]

  proc `[]`*(b: var TokenBuf; i: int): var PackedToken {.inline.} =
    assert i >= 0 and i < b.len
    result = b.data[i]

proc `[]=`*(b: TokenBuf; i: int; val: PackedToken) {.inline.} =
  assert i >= 0 and i < b.len
  b.data[i] = val

proc cursorAt*(b: var TokenBuf; i: int): Cursor {.inline.} =
  assert i >= 0 and i < b.len
  if b.owner == nil:
    b.owner = cast[CursorOwner](alloc0(sizeof(CursorOwnerObj)))
    b.owner.data = b.data
    b.owner.rc = 1  # 1 for TokenBuf
  inc b.owner.rc  # + 1 for the returned Cursor
  result = Cursor(owner: b.owner, p: addr b.data[i], rem: b.len-i)

proc readonlyCursorAt*(b: TokenBuf; i: int): Cursor {.inline.} =
  assert i >= 0 and i < b.len
  result = Cursor(p: addr b.data[i], rem: b.len-i)
  if b.owner != nil:
    inc b.owner.rc
    result.owner = b.owner

proc shareRead*(b: var TokenBuf; c: Cursor): Cursor =
  result = c

proc cursorToPosition*(b: TokenBuf; c: Cursor): int {.inline.} =
  result = (cast[int](c.p) - cast[int](b.data)) div sizeof(PackedToken)

proc cursorToPosition*(base, c: Cursor): int {.inline.} =
  let c = cast[int](c.p)
  let base = cast[int](base.p)
  assert c >= base
  result = (c - base) div sizeof(PackedToken)
  assert result < 1_000_000

proc toUniqueId*(c: Cursor): int {.inline.} =
  result = cast[int](c.p)

proc add*(result: var TokenBuf; c: Cursor) =
  result.add c.load

proc addToken*(dest: var TokenBuf; t: PackedToken) {.inline.} =
  ## Adds a single token to dest. Prefer this over `dest.add` in compiler
  ## passes so that static analysis tools can track NIF construction.
  dest.add t

proc addToken*(dest: var TokenBuf; c: Cursor) {.inline.} =
  ## Adds a single token (from cursor position) to dest.
  dest.add c.load

proc addSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symToken(s, info)

proc addSymDef*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symdefToken(s, info)

proc addSubtree*(result: var TokenBuf; c: Cursor) =
  assert c.kind != ParRi, "cursor at end?"
  when defined(virtualParRi):
    if c.kind != ParLe:
      result.addRaw c.load
      return
    # Bulk-copy the already-sealed subtree byte-for-byte. Doing this
    # token-by-token through `add` would push/pop `openTags` and re-seal
    # the source's jumps from dest-side positions; since the source's
    # `ParRi`s may be elided, the per-token loop wouldn't even terminate.
    var c2 = c
    let startP = c2.p
    skip c2
    let n = int((cast[uint](c2.p) - cast[uint](startP)) div sizeof(PackedToken).uint)
    if result.owner != nil: prepareMutation(result)
    if result.len + n > result.cap:
      result.cap = max(result.cap div 2 + result.cap, result.len + n)
      result.data = cast[Storage](realloc(result.data, sizeof(PackedToken)*result.cap))
    copyMem(addr result.data[result.len], startP, n * sizeof(PackedToken))
    result.len += n
  else:
    if c.kind != ParLe:
      # atom:
      result.add c.load
    else:
      var c = c
      var nested = 0
      while true:
        let item = c.load
        result.add item
        if item.kind == ParRi:
          dec nested
          if nested == 0: break
        elif item.kind == ParLe: inc nested
        inc c
      inc c

proc addUnstructured*(result: var TokenBuf; c: Cursor) =
  var c = c
  while c.rem > 0:
    result.add c.load
    inc c

proc add*(result: var TokenBuf; s: var Stream) =
  let c = next(s)
  assert c.kind != ParRi, "cursor at end?"
  result.add c
  if c.kind == ParLe:
    var nested = 0
    while true:
      let item = next(s)
      result.add item
      if item.kind == ParRi:
        if nested == 0: break
        dec nested
      elif item.kind == ParLe: inc nested

iterator items*(b: TokenBuf): PackedToken =
  for i in 0 ..< b.len:
    yield b.data[i]

proc span*(c: Cursor): int =
  ## Number of physical tokens the subtree at `c` occupies. Under
  ## `-d:virtualParRi` elided ParRis do not count (they don't exist).
  when defined(virtualParRi):
    var c2 = c
    skip c2
    result = int((cast[uint](c2.p) - cast[uint](c.p)) div sizeof(PackedToken).uint)
  else:
    result = 0
    var c = c
    if c.kind == ParLe:
      var nested = 0
      while true:
        inc c
        inc result
        if c.kind == ParRi:
          if nested == 0: break
          dec nested
        elif c.kind == ParLe: inc nested
    if c.rem > 0:
      inc c
      inc result

proc add*(dest: var TokenBuf; src: TokenBuf) =
  when defined(virtualParRi):
    # `src` may be a *fragment*: sealed subtrees (their ParRi elided), plus
    # still-open ParLes (recorded in src.openTags) and unmatched ParRis that
    # close opens living in `dest`. Replay it with the semantics a
    # token-by-token build would have had: sealed subtrees are bulk-copied
    # (jumps are position-independent), open ParLes become open tags of
    # `dest`, unmatched ParRis seal dest's opens.
    var i = 0
    var openIdx = 0 # src.openTags is ascending
    while i < src.len:
      let t = src.data[i]
      if t.kind == ParLe and
          not (openIdx < src.openTags.len and src.openTags[openIdx] == i):
        # sealed subtree: bulk-copy its physical span
        let n = span(Cursor(p: addr src.data[i], rem: src.len - i))
        if dest.owner != nil: prepareMutation(dest)
        if dest.len + n > dest.cap:
          dest.cap = max(dest.cap div 2 + dest.cap, dest.len + n)
          dest.data = cast[Storage](realloc(dest.data, sizeof(PackedToken)*dest.cap))
        copyMem(addr dest.data[dest.len], addr src.data[i], n * sizeof(PackedToken))
        dest.len += n
        i += n
      else:
        if t.kind == ParLe: inc openIdx
        dest.add t
        inc i
  else:
    for t in items(src): dest.add t

proc fromCursor*(c: Cursor): TokenBuf =
  result = createTokenBuf(4)
  result.add c

proc fromStream*(s: var Stream): TokenBuf =
  result = createTokenBuf(4)
  result.add s

proc shrink*(b: var TokenBuf; newLen: int) =
  if b.owner != nil: prepareMutation(b)
  assert newLen >= 0 and newLen <= b.len
  when defined(virtualParRi):
    # Truncating away an unsealed ParLe must also drop its open-tag entry,
    # otherwise a later add(ParRi) seals a position that no longer exists.
    # Callers use shrink to roll back speculatively emitted subtrees.
    while b.openTags.len > 0 and b.openTags[b.openTags.len - 1] >= newLen:
      b.openTags.setLen b.openTags.len - 1
  b.len = newLen

proc reopenLastTree*(b: var TokenBuf; pos: int) =
  ## Reopens the finished tree headed by the ParLe at `pos` so that more
  ## children can be appended; close it again with `addParRi`. The tree's
  ## subtree must be the last content of `b`. Classic mode removes the
  ## trailing `)`; under `-d:virtualParRi` the ParLe is re-registered as
  ## an open tag (its `)` was elided; overflow scopes drop their real one).
  if b.owner != nil: prepareMutation(b)
  assert b.data[pos].kind == ParLe, "reopenLastTree: no ParLe at pos"
  when defined(virtualParRi):
    if jump(b.data[pos]) == MaxJump:
      assert b.data[b.len-1].kind == ParRi, "reopenLastTree: tree does not end the buffer"
      dec b.len
    else:
      assert pos + 1 + int(jump(b.data[pos])) == b.len,
        "reopenLastTree: tree does not end the buffer"
    var insertAt = b.openTags.len
    for k in 0 ..< b.openTags.len:
      if b.openTags[k] > pos:
        insertAt = k
        break
    b.openTags.insert(pos, insertAt)
  else:
    assert b.data[b.len-1].kind == ParRi, "reopenLastTree: tree does not end the buffer"
    dec b.len

proc grow(b: var TokenBuf; newLen: int) =
  if b.owner != nil: prepareMutation(b)
  assert newLen > b.len
  if b.cap < newLen:
    b.cap = max(b.cap div 2 + b.cap, newLen)
    b.data = cast[Storage](realloc(b.data, sizeof(PackedToken)*b.cap))
  b.len = newLen

template buildTree*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(tag, info)
  body
  dest.addParRi(info)

proc addParLe*(dest: var TokenBuf; tag: TagId; info = NoLineInfo) =
  dest.add parLeToken(tag, info)

proc addParRi*(dest: var TokenBuf) =
  dest.add parRiToken(NoLineInfo)

proc addParRi*(dest: var TokenBuf; info: PackedLineInfo) =
  ## Use this rather than `dest.add parRiToken(info)` so the encoding of a
  ## closing `)` lives in one place — paves the way for a future virtual-
  ## ParRi optimisation that folds the closer into the matching ParLe.
  dest.add parRiToken(info)

proc addParRi*(dest: var seq[PackedToken]; info: PackedLineInfo) =
  ## Same intent for the inline-subtree case (constructing a `seq[…]`
  ## before wrapping with `fromBuffer`).
  dest.add parRiToken(info)

proc addDotToken*(dest: var TokenBuf) =
  dest.add dotToken(NoLineInfo)

proc addStrLit*(dest: var TokenBuf; s: string; info = NoLineInfo) =
  dest.add strToken(pool.strings.getOrIncl(s), info)

proc addIntLit*(dest: var TokenBuf; i: BiggestInt; info = NoLineInfo) =
  dest.add intToken(pool.integers.getOrIncl(i), info)

proc addUIntLit*(dest: var TokenBuf; i: BiggestUInt; info = NoLineInfo) =
  dest.add uintToken(pool.uintegers.getOrIncl(i), info)

proc addIdent*(dest: var TokenBuf; s: string; info = NoLineInfo) =
  dest.add identToken(pool.strings.getOrIncl(s), info)

proc addCharLit*(dest: var TokenBuf; c: char; info = NoLineInfo) =
  dest.add charToken(c, info)

proc addFloatLit*(dest: var TokenBuf; f: BiggestFloat; info = NoLineInfo) =
  dest.add floatToken(pool.floats.getOrIncl(f), info)

when defined(virtualParRi):
  proc spliceRaw(dest: var TokenBuf; p: ptr PackedToken; n: int; pos: int;
                 srcOpenTags: openArray[int]) =
    ## Splices `n` already-normalized tokens at `pos` and fixes the
    ## open-tag bookkeeping: existing entries at/after `pos` shift by `n`,
    ## the fragment's own still-open tags (`srcOpenTags`, fragment-relative,
    ## ascending) slot in between so the stack stays ascending.
    if n <= 0: return
    if dest.owner != nil: prepareMutation(dest)
    let oldLen = dest.len
    dest.grow(oldLen + n)
    if oldLen > pos:
      moveMem(addr dest.data[pos + n], addr dest.data[pos],
              (oldLen - pos) * sizeof(PackedToken))
    copyMem(addr dest.data[pos], p, n * sizeof(PackedToken))
    var insertAt = dest.openTags.len
    for k in 0 ..< dest.openTags.len:
      if dest.openTags[k] >= pos:
        dest.openTags[k] += n
        if insertAt == dest.openTags.len: insertAt = k
    for k in 0 ..< srcOpenTags.len:
      dest.openTags.insert(srcOpenTags[k] + pos, insertAt + k)

proc insert*(dest: var TokenBuf; src: openArray[PackedToken]; pos: int) =
  when defined(virtualParRi):
    # Normalize the raw fragment exactly as a plain build would (sealing
    # matched pairs, eliding their ParRis), then splice. A ParRi that
    # closes an open living in `dest` before `pos` is not supported here.
    var tmp = createTokenBuf(max(src.len, 4))
    for t in src:
      assert t.kind != ParRi or tmp.openTags.len > 0,
        "insert: unmatched ')' in inserted fragment"
      tmp.add t
    spliceRaw dest, addr tmp.data[0], tmp.len, pos,
              toOpenArray(tmp.openTags, 0, tmp.openTags.len-1)
  else:
    var j = len(dest) - 1
    var i = j + len(src)
    dest.grow(i + 1)

    # Move items after `pos` to the end of the sequence.
    while j >= pos:
      dest[i] = dest[j]
      dec i
      dec j
    # Insert items from `dest` into `dest` at `pos`
    inc j
    for item in src:
      dest[j] = item
      inc j

proc insert*(dest: var TokenBuf; src: Cursor; pos: int) =
  when defined(virtualParRi):
    # a complete (sealed) subtree: splice its physical span; it contributes
    # no open tags of its own
    spliceRaw dest, src.p, span(src), pos, []
  else:
    insert dest, toOpenArray(cast[ptr  UncheckedArray[PackedToken]](src.p), 0, span(src)-1), pos

proc insert*(dest: var TokenBuf; src: TokenBuf; pos: int) =
  when defined(virtualParRi):
    # `src` was built through `add`, so it is already normalized; its
    # still-open ParLes carry over as open tags of `dest`.
    if src.len > 0:
      spliceRaw dest, addr src.data[0], src.len, pos,
                toOpenArray(src.openTags, 0, src.openTags.len-1)
  else:
    insert dest, toOpenArray(src.data, 0, src.len-1), pos

proc checkSeals*(b: TokenBuf; maxReports = 5): seq[string] =
  ## Debugging helper: verifies that every sealed scope ends at or before
  ## its parent's end. Returns human-readable reports (empty = consistent).
  ## Only meaningful under `-d:virtualParRi`.
  result = @[]
  when defined(virtualParRi):
    var stack: seq[(string, int)] = @[] # (tag, endPos) per open scope
    var openIdx = 0 # b.openTags is ascending: skip still-open tags,
                    # their jump fields are not sealed yet
    for i in 0 ..< b.len:
      while stack.len > 0 and i >= stack[stack.len-1][1]: discard stack.pop()
      let t = b.data[i]
      if openIdx < b.openTags.len and b.openTags[openIdx] == i:
        inc openIdx
        continue
      if t.kind == ParLe and jump(t) != MaxJump:
        let endPos = i + 1 + int(jump(t))
        if endPos > b.len or (stack.len > 0 and endPos > stack[stack.len-1][1]):
          if result.len < maxReports:
            let u = unpack(pool.man, t.info)
            result.add "(" & pool.tags[nifstreams.tag(t)] & " jump=" & $jump(t) &
              " at token " & $i & " ends at " & $endPos & " but parent (" &
              (if stack.len > 0: stack[stack.len-1][0] else: "") & ") ends at " &
              (if stack.len > 0: $stack[stack.len-1][1] else: "buf:" & $b.len) &
              " src=" & (if u.file != FileId(0): pool.files[u.file] else: "?") & ":" & $u.line & ":" & $u.col
        stack.add (pool.tags[nifstreams.tag(t)], endPos)

proc widenSealed*(dest: var TokenBuf; enclosing: int; growth: int) =
  ## After an `insert`/`replace` grew the contents of an already-sealed
  ## scope (its ParLe at `enclosing`), widen the seal to match — the splice
  ## operations keep open-tag bookkeeping in sync but cannot know which
  ## sealed ancestor spans the splice point. No-op in classic mode and for
  ## overflow scopes (their real ParRi moved along with the content).
  when defined(virtualParRi):
    if growth > 0 and jump(dest[enclosing]) != MaxJump:
      assert jump(dest[enclosing]) + uint32(growth) < MaxJump, "widenSealed: subtree overflows the jump field"
      setJump(dest[enclosing], jump(dest[enclosing]) + uint32(growth))

proc replace*(dest: var TokenBuf; by: Cursor; pos: int) =
  if dest.owner != nil: prepareMutation(dest)
  when defined(virtualParRi):
    # Replaces the sealed subtree at `pos` with the sealed subtree `by`.
    let actualLen = min(span(Cursor(p: addr dest.data[pos], rem: dest.len-pos)),
                        dest.len - pos)
    let byLen = span(by)
    let oldLen = dest.len
    let newLen = oldLen + byLen - actualLen
    let delta = byLen - actualLen
    if delta != 0:
      # keep open-tag positions beyond the replaced region in sync; opens
      # strictly inside it cannot exist (the subtree at `pos` is sealed)
      for k in 0 ..< dest.openTags.len:
        if dest.openTags[k] >= pos + actualLen: dest.openTags[k] += delta
    if delta > 0:
      dest.grow(newLen)
      moveMem(addr dest.data[pos + byLen], addr dest.data[pos + actualLen],
              (oldLen - pos - actualLen) * sizeof(PackedToken))
    elif delta < 0:
      moveMem(addr dest.data[pos + byLen], addr dest.data[pos + actualLen],
              (oldLen - pos - actualLen) * sizeof(PackedToken))
      dest.len = newLen
    copyMem(addr dest.data[pos], by.p, byLen * sizeof(PackedToken))
  else:
    let len = span(Cursor(p: addr dest.data[pos], rem: dest.len-pos))
    let actualLen = min(len, dest.len - pos)
    let byLen = span(by)
    let oldLen = dest.len
    let newLen = oldLen + byLen - actualLen
    if byLen > actualLen:
      # Need to make room for additional elements
      dest.grow(newLen)
      # Move existing elements to the right
      for i in countdown(oldLen - 1, pos + actualLen):
        dest[i + byLen - actualLen] = dest[i]
    elif byLen < actualLen:
      # Need to remove elements
      for i in pos + byLen ..< dest.len - (actualLen - byLen):
        dest[i] = dest[i + actualLen - byLen]
      dest.shrink(newLen)
    # Copy new elements
    var by = by
    for i in 0 ..< byLen:
      dest[pos + i] = by.load
      inc by

proc replaceWithOpenTag*(dest: var TokenBuf; tag: PackedToken; pos: int) =
  ## Replaces the subtree at `pos` with the single open tag `tag` (a ParLe)
  ## and registers it as unsealed, so a later `addParRi` closes it. The
  ## structured way to spell the classic "replace a tree with `(tag` and
  ## close it further down the line" idiom.
  assert tag.kind == ParLe
  replace(dest, fromBuffer([tag]), pos)
  when defined(virtualParRi):
    var insertAt = dest.openTags.len
    for k in 0 ..< dest.openTags.len:
      if dest.openTags[k] > pos:
        insertAt = k
        break
    dest.openTags.insert(pos, insertAt)

proc toString*(b: TokenBuf; produceLineInfo = true): string =
  result = nifstreams.toString(toOpenArray(b.data, 0, b.len-1), produceLineInfo)

proc toString*(b: TokenBuf; first: int; produceLineInfo = true): string =
  var last = first
  var nested = 0
  while last < b.len:
    case b[last].kind
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested == 0: break
    inc last
  if last == b.len: dec last
  result = nifstreams.toString(toOpenArray(b.data, first, last), produceLineInfo)

proc toString*(b: Cursor; produceLineInfo = true): string =
  let counter = span(b)
  result = nifstreams.toString(toOpenArray(cast[ptr UncheckedArray[PackedToken]](b.p), 0, counter-1), produceLineInfo)

proc toStringDebug*(b: Cursor; produceLineInfo = true): string =
  let L = if b.kind == ParLe: 1 else: 0
  result = nifstreams.toString(toOpenArray(cast[ptr UncheckedArray[PackedToken]](b.p), 0, L), produceLineInfo)

proc writeFile*(b: TokenBuf; filename: string; mode: FileWriteMode = AlwaysWrite) {.canRaise.} =
  let content = toModuleString(toOpenArray(b.data, 0, b.len-1), "." & extractModuleSuffix(filename))
  if mode == OnlyIfChanged:
    let existingContent = try: vfsRead(filename) except: ""
    if existingContent == content: return
  vfsWrite(filename, content)

proc `$`*(c: Cursor): string = toString(c, false)

template copyInto*(dest: var TokenBuf; tag: TagId; info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(tag, info)
  body
  dest.addParRi()

proc parLeTokenUnchecked*(tag: string; info: PackedLineInfo): PackedToken {.inline.} =
  parLeToken(pool.tags.getOrIncl(tag), info)

template copyIntoUnchecked*(dest: var TokenBuf; tag: string; info: PackedLineInfo; body: untyped) =
  dest.add parLeTokenUnchecked(tag, info)
  body
  dest.addParRi()

proc parse*(r: var Stream; dest: var TokenBuf;
            parentInfo: PackedLineInfo; debug: bool = false) =
  ## Read tokens from `r` into `dest`. `parentInfo` seeds the parent line-info
  ## stack so that the first token read resolves its diff against the correct
  ## source-tree parent. `parseFromFile`/`parseFromBuffer` pass `NoLineInfo`;
  ## index-jumped reads pass the indexed compound's *parent* info (recorded
  ## by `toModuleString`'s SymbolDef branch — see the comment there).
  r.parents[0] = parentInfo
  var nested = 0
  while true:
    let tok = r.next()
    dest.add tok
    if debug:
      echo "parsing ", toString([tok], false)
      if tok.kind == UnknownToken: break
    if tok.kind == EofToken:
      break
    elif tok.kind == ParLe:
      inc nested
    elif tok.kind == ParRi:
      dec nested
      if nested == 0: break

proc parseFromBuffer*(input: string; thisModule: sink string; sizeHint = 100): TokenBuf =
  var r = nifstreams.openFromBuffer(input, thisModule)
  result = createTokenBuf(sizeHint)
  parse(r, result, NoLineInfo)

proc parseFromFile*(filename: string; sizeHint = 100): TokenBuf =
  var r = nifstreams.open(filename)
  discard processDirectives(r.r)
  result = createTokenBuf(sizeHint)
  parse(r, result, NoLineInfo)

proc isLastSon*(n: Cursor): bool =
  var n = n
  skip n
  result = n.kind == ParRi

proc firstSon*(n: Cursor): Cursor {.inline.} =
  ## Cursor at the first child of the ParLe at `n`. Under `-d:virtualParRi`
  ## the result is *bounded* to the node's children, so `hasMore` on it
  ## terminates at the node's (elided) close.
  result = n
  when defined(virtualParRi):
    discard enterScope(result)
  else:
    inc result

proc takeToken*(buf: var TokenBuf; n: var Cursor) {.inline.} =
  buf.add n
  inc n

type
  SkipIntent* = enum
    SkipTag       ## advance past a ParLe tag (entering a node to rewrite children)
    SkipParRi     ## advance past a closing paren
    SkipName      ## skip a name/SymbolDef child
    SkipExport    ## skip an export marker child
    SkipPragmas   ## skip a pragmas section
    SkipType      ## skip a type child
    SkipExpr    ## skip an expression child
    SkipStmt    ## skip a statement child
    SkipValue     ## skip a value/expression child
    SkipGenParams ## skip generic parameters
    SkipCond      ## skip a condition expression
    SkipBody      ## skip a body/stmts section
    SkipEffects   ## skip an effects section
    SkipResult    ## skip a result that has been handled separately
    SkipFull      ## skip an entire subtree being dropped or replaced

proc skipIntentMatches*(k: NifKind; intent: SkipIntent): bool {.inline.} =
  ## Loose predicate: catches obviously wrong intents (wrong direction,
  ## skipping a stmt where there's no ParLe, etc.) without over-fitting.
  case intent
  of SkipTag:        k == ParLe
  of SkipParRi:      k == ParRi
  of SkipName:       k in {SymbolDef, Symbol, Ident, DotToken, ParLe}
                       # ParLe: `for (unpackdecl ...)` / `(unpacktup ...)`
  of SkipExport:     k in {DotToken, Ident, ParLe}
  of SkipPragmas:    k in {DotToken, ParLe}
  of SkipType:       k in {ParLe, Symbol, Ident, DotToken, IntLit, UIntLit}
  of SkipExpr:       k notin {EofToken, ParRi}
  of SkipStmt:       k in {ParLe, DotToken}
  of SkipValue:      k notin {EofToken, ParRi}
  of SkipGenParams:  k in {DotToken, ParLe}
  of SkipCond:       k notin {EofToken, ParRi}
  of SkipBody:       k in {DotToken, ParLe}
  of SkipEffects:    k in {DotToken, ParLe}
  of SkipResult:     k in {DotToken, ParLe, Symbol, SymbolDef}
  of SkipFull:       k != EofToken

template skip*(c: var Cursor; intent: SkipIntent) =
  ## Skip a subtree with declared intent. The intent is enforced at runtime
  ## via a loose predicate (`skipIntentMatches`) so wrong labels fire early.
  assert skipIntentMatches(c.kind, intent),
    "skip " & $intent & ": cursor at " & $c.kind & " is incompatible"
  skip(c)

template inc*(c: var Cursor; intent: SkipIntent) =
  ## Advance one token with declared intent. Same enforcement as `skip`.
  assert skipIntentMatches(c.kind, intent),
    "inc " & $intent & ": cursor at " & $c.kind & " is incompatible"
  inc c

# ── TagClass: structural categories of tokens ──────────────────────────────
# These complement `SkipIntent` (which describes a *role* in a parent shape)
# with a *kind* category. Used as the intent argument to `inc`/`skip`/`into`
# when the precise tag isn't statically known but a category is.

type
  TagClass* = enum
    Anything    ## any single token (any NifKind except EofToken)
    AnyExpr     ## any expression position — atom or ParLe (anything but EOF/ParRi)
    AnyStmt     ## any statement position — ParLe-tagged or DotToken (no-op)
    AnyType     ## any type position — ParLe-tagged, Symbol/Ident alias, or DotToken (void)

proc tagClassMatches*(k: NifKind; expected: TagClass): bool {.inline.} =
  case expected
  of Anything: k != EofToken
  of AnyExpr:  k notin {EofToken, ParRi}
  of AnyStmt:  k in {ParLe, DotToken}
  of AnyType:  k in {ParLe, Symbol, Ident, DotToken}

template skip*(c: var Cursor; expected: TagClass) =
  assert tagClassMatches(c.kind, expected),
    "skip " & $expected & ": cursor at " & $c.kind & " is incompatible"
  skip(c)

template inc*(c: var Cursor; expected: TagClass) =
  assert tagClassMatches(c.kind, expected),
    "inc " & $expected & ": cursor at " & $c.kind & " is incompatible"
  inc c

template into*(c: var Cursor; expected: TagClass; body: untyped) =
  ## Like the bare `into`, but asserts the entered scope matches `expected`.
  assert tagClassMatches(c.kind, expected),
    "into " & $expected & ": cursor at " & $c.kind & " is incompatible"
  into c:
    body

template loopInto*(c: var Cursor; expected: TagClass; body: untyped) =
  assert tagClassMatches(c.kind, expected),
    "loopInto " & $expected & ": cursor at " & $c.kind & " is incompatible"
  loopInto c:
    body

proc takeTree*(dest: var TokenBuf; n: var Cursor) =
  if n.kind != ParLe:
    dest.add n
    inc n
  else:
    when defined(virtualParRi):
      dest.addSubtree n
      skip n
    else:
      var nested = 0
      while true:
        dest.add n
        case n.kind
        of ParLe: inc nested
        of ParRi:
          dec nested
          if nested == 0:
            inc n
            break
        of EofToken:
          raiseAssert "expected ')', but EOF reached"
        else: discard
        inc n

when isMainModule:
  # Token counts differ per mode: under `-d:virtualParRi` a sealed empty
  # pair occupies one token (its close is elided).
  const PairLen = when defined(virtualParRi): 1 else: 2
  # test replace
  block:
    var dest = createTokenBuf(1)
    dest.addDotToken()
    block:
      let by = [charToken('a', NoLineInfo)]
      replace dest, fromBuffer(by), 0
      assert dest[0] == by[0]
    block:
      let by2 = [parLeToken(TagId 1, NoLineInfo), parRiToken(NoLineInfo)]
      replace dest, fromBuffer(by2), 0
      assert dest.len == PairLen
      assert dest[0].kind == ParLe and dest[0].tagId == TagId 1
      when not defined(virtualParRi):
        assert dest[1] == by2[1]

      let by1 = [strToken(StrId 2, NoLineInfo)]
      replace dest, fromBuffer(by1), 0
      assert dest.len == 1
      assert dest[0] == by1[0]
  block:
    var dest = createTokenBuf(3)
    let dest0 = parLeToken(TagId 123, NoLineInfo)
    dest.add dest0
    dest.addDotToken()
    dest.addParRi
    block:
      let by = [charToken('a', NoLineInfo)]
      replace dest, fromBuffer(by), 1
      assert dest.len == 2 + PairLen - 1
      assert dest[0].kind == ParLe and dest[0].tagId == TagId 123
      assert dest[1] == by[0]
      when not defined(virtualParRi):
        assert dest[2].kind == ParRi

      let by2 = [parLeToken(TagId 456, NoLineInfo), parRiToken(NoLineInfo)]
      replace dest, fromBuffer(by2), 1
      assert dest.len == 2*PairLen
      assert dest[0].kind == ParLe and dest[0].tagId == TagId 123
      assert dest[1].kind == ParLe and dest[1].tagId == TagId 456
      when not defined(virtualParRi):
        assert dest[2] == by2[1]
        assert dest[3].kind == ParRi

      let by1 = [strToken(StrId 789, NoLineInfo)]
      replace dest, fromBuffer(by1), 1
      assert dest.len == 1 + PairLen
      assert dest[0].kind == ParLe and dest[0].tagId == TagId 123
      assert dest[1] == by1[0]
      when not defined(virtualParRi):
        assert dest[2].kind == ParRi
