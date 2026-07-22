#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Move analyser.
import std / [assertions, intsets, syncio]

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, controlflow, programs]

type
  RootOfMode* = enum
    CanFollowDerefs, CannotFollowDerefs, CanFollowCalls

  FindStartEntry = object
    pos: int32       ## position in `cf`, or -1 if no CF token maps to this source pos
    nested: int16    ## paren-nesting depth at that position

  FindStartIndex = object
    ## Source-position → (cf position, nesting) lookup. Indexed by source-buffer
    ## position; `data[srcPos - base]` holds the entry, so only the touched span
    ## `[base, base + data.len)` is allocated rather than the whole buffer (the
    ## LocSpan trick from arkham's register_allocator). Built from the side-channel
    ## `srcMap` that `toControlflowWithMap` returns — the source→CF mapping no
    ## longer rides in the token `info` field (nifcore tokens may carry none).
    base: int32
    data: seq[FindStartEntry]

  MoverContext* = object
    ## Per-pass context for the mover. Holds the controlflow buffer and a
    ## source-position → (cf position, nested depth) lookup built once when
    ## the CF is materialized. Replaces the bare `cf: TokenBuf` the duplifier
    ## used to carry around, and turns `findStart` from an O(N) buffer scan
    ## into an O(1) array index.
    cf*: TokenBuf
    index*: FindStartIndex

proc rootOf*(n: Cursor; mode = CanFollowDerefs): SymId =
  var n = n
  while true:
    case n.exprKind
    of DerefX, HderefX, PatX:
      if mode == CannotFollowDerefs:
        break
      inc n
    of DotX, TupatX, AtX, ArratX, AddrX, HaddrX:
      inc n
    of ConvKinds:
      inc n
      skip n # type part
    of BaseobjX:
      inc n
      skip n # type part
      skip n # skip intlit
    of CallKinds:
      if mode == CanFollowCalls:
        inc n
        skip n # skip fn and continue with the first argument.
        # This is exactly what we want for `addr mgetorPut(table, key)` so
        # that we can mark `table` as aliased.
      else:
        break
    else:
      break
  if n.kind == Symbol:
    result = n.symId
  else:
    result = NoSymId

proc sameTreesIgnoreArrayIndexes*(a, b: Cursor): bool =
  var a = a
  var b = b
  if a.kind != b.kind: return false
  case a.kind
  of OpenTagKind:
    if a.tagId != b.tagId: return false
    if a.exprKind in {PatX, ArratX}:
      # compare the accessed object only, not the array indexes:
      inc a
      inc b
      result = sameTreesIgnoreArrayIndexes(a, b)
    else:
      a = sub(a)
      b = sub(b)
      while true:
        if a.hasMore != b.hasMore: return false
        if not a.hasMore: break
        if not sameTreesIgnoreArrayIndexes(a, b): return false
        skip a
        skip b
      result = true
  of Symbol, SymbolDef:
    result = a.symId == b.symId
  of IntLit:
    result = a.intId == b.intId
  of UIntLit:
    result = a.uintId == b.uintId
  of FloatLit:
    result = a.floatId == b.floatId
  of StrLitKind, Ident:
    result = a.litId == b.litId
  of CharLit:
    result = a.uoperand == b.uoperand
  of DotToken:
    result = true
  else:
    # ParRi/close (classic) or a stray suffix (nifcore); unreachable in a walk.
    result = true

proc disjointDirectField(tree: Cursor; r: SymId; x: Cursor): bool =
  ## True if `tree` is a *direct* field/index access of the moved root `r` that
  ## is statically disjoint from `x`'s own field/index — i.e. `tree` is
  ## `(dot r other)` while `x` is `(dot r field)` with `other != field`, or the
  ## tuple equivalent `(tupat r J)` vs `(tupat r K)` with `J != K`. Such a
  ## location can never alias `x`, so the scan may skip it whole. Only the
  ## direct shapes off a bare root qualify; anything nested or through a deref
  ## returns false, so the conservative scan still runs (and still catches
  ## whole-object reads and deref-of-root).
  ##
  ## Both `(dot OBJ FIELD …)` and `(tupat OBJ INDEX)` lay out as: token 0 is the
  ## accessor tag, token 1 is OBJ, token 2 is the selector (the field symbol, or
  ## the index literal). OBJ here is required to be the bare root symbol `r`, so
  ## each OBJ is a single token and one `inc` steps tag→OBJ, another OBJ→selector.
  result = false
  if tree.kind == OpenTagKind and x.kind == OpenTagKind and tree.exprKind == x.exprKind:
    var treeObj = tree
    inc treeObj                 # tree: accessor tag -> OBJ
    var xObj = x
    inc xObj                    # x:    accessor tag -> OBJ
    let bothRootedAtR =
      treeObj.kind == Symbol and treeObj.symId == r and
      xObj.kind == Symbol and xObj.symId == r
    if bothRootedAtR:
      var treeSel = treeObj
      inc treeSel               # tree: OBJ -> selector
      var xSel = xObj
      inc xSel                  # x:    OBJ -> selector
      case tree.exprKind
      of DotX:                  # disjoint iff the two field names differ
        result = treeSel.kind == Symbol and xSel.kind == Symbol and
                 treeSel.symId != xSel.symId
      of TupatX:                # disjoint iff the two tuple indices differ
        result = treeSel.kind == IntLit and xSel.kind == IntLit and
                 pool.integers[treeSel.intId] != pool.integers[xSel.intId]
      else:
        discard

proc containsRoot(tree: var Cursor; x: Cursor): bool =
  ## True if `tree` contains a read whose location can alias `x` (the location
  ## being moved out of): the whole root `r`, the exact field, a sub-path, or a
  ## read through a deref of `r`. Statically-disjoint sibling fields/indices of
  ## `r` are skipped — they cannot alias `x` (see `disjointDirectField`).
  let r = rootOf(x)
  # scan also correct for `r == NoSymId`:
  result = false
  case tree.kind
  of Symbol:
    if tree.symId == r:
      result = true
    inc tree
  of OpenTagKind:
    if disjointDirectField(tree, r, x):
      # A sibling field/index that cannot alias `x`: skip the whole subtree.
      skip tree
    elif tree.exprKind == DotX:
      tree.into:
        if containsRoot(tree, x):
          result = true
        while tree.hasMore:
          skip tree
    elif tree.substructureKind == KvU:
      tree.into:
        skip tree # key ignored for object construction!
        while tree.hasMore:
          if containsRoot(tree, x):
            result = true
    else:
      tree.loopInto:
        if containsRoot(tree, x):
          result = true
  else:
    inc tree

proc contains(index: FindStartIndex; srcPos: int): bool {.inline.} =
  srcPos >= int(index.base) and srcPos < int(index.base) + index.data.len

proc buildFindStartIndex(cf: TokenBuf; srcMap: openArray[int32]): FindStartIndex =
  ## One-pass scan of the just-built CF buffer that records, for each source
  ## position, the *first* CF token that maps back to it (via the parallel
  ## `srcMap` side-channel) and the paren-nesting depth at that position.
  ## Subsequent `findStart` lookups are then O(1).
  assert srcMap.len == cf.len
  # Size to the touched source span `[lo, hi]` with a `base` (LocSpan trick),
  # rather than allocating an entry per source token.
  var lo = high(int32)
  var hi = -1'i32
  for s in srcMap:
    if s >= 0:
      if s < lo: lo = s
      if s > hi: hi = s
  if hi < 0:
    return FindStartIndex(base: 0, data: @[])
  result = FindStartIndex(base: lo, data: newSeq[FindStartEntry](int(hi - lo) + 1))
  for i in 0 ..< result.data.len:
    result.data[i] = FindStartEntry(pos: -1'i32, nested: 0)
  var nested = 0
  # Under `-d:virtualParRi` the sealed ParRis are elided from `cf`, so counting
  # `dec nested` only on physical ParRis would make `nested` grow monotonically
  # (overflowing `int16` on large modules, then wrapping negative → `findStart`
  # spuriously reports "not indexed"). Track each open sealed scope's last-content
  # index and decrement when we walk past it. Overflow scopes (jump == MaxJump)
  # keep a physical ParRi and are handled by the `of ParRi` branch.
  var closeStack: seq[int] = @[]
  for i in 0 ..< cf.len:
    case cf[i].kind
    of OpenTagKind:
      inc nested
      # nifcore: every TagLit's close is implicit; no MaxJump sentinel.
      closeStack.add(i + span(readonlyCursorAt(cf, i)) - 1)
    else:
      discard # nifcore: no physical ParRi token; closes are tracked below
    let s = srcMap[i]
    if s >= 0:
      let k = int(s - result.base)
      if result.data[k].pos < 0:
        result.data[k] = FindStartEntry(pos: int32(i), nested: int16(nested))
    while closeStack.len > 0 and closeStack[^1] == i:
      dec nested
      discard closeStack.pop()

proc findStart(c: TokenBuf; srcPos: int; n: var Cursor;
               index: FindStartIndex): int =
  if not index.contains(srcPos): return -1
  let e = index.data[srcPos - int(index.base)]
  if e.pos < 0: return -1
  n = c.readonlyCursorAt(int(e.pos))
  result = int(e.nested)

proc singlePath(pc: Cursor; nested: int; x: Cursor; pcs: var seq[Cursor];
                otherUsage: var Cursor; marks: var IntSet; cfBase: Cursor): bool =
  var nested = nested
  var pc = pc
  let root = rootOf(x)
  while true:
    if not pc.hasMore:
      # ran off the end of the CF buffer: the routine ends here, no
      # further usage on this path (classic had a closing ParRi to stop on)
      break
    #echo "PC IS: ", pc.kind
    case pc.kind
    of GotoInstr:
      # GotoInstr == DotToken: a nonzero payload is a jump, zero is the
      # plain no-op dot.
      let diff = pc.getInt28
      if diff == 0:
        inc pc
      elif diff < 0:
        # jump backwards:
        let back = pc +! diff
        if not marks.containsOrIncl(cursorToPosition(cfBase, back)):
          pc = back
        else:
          # finished traversing this path:
          break
      else:
        # ordinary goto, simply follow it:
        pc = pc +! diff
    of Symbol:
      if x.kind == Symbol and pc.symId == x.symId:
        otherUsage = pc
        return false
      inc pc
    of SymbolDef:
      if root != NoSymId and pc.symId == root:
        # found the definition, so it gets a new value:
        break
      inc pc
    of Ident, StrLitKind, CharLit, IntLit, UIntLit, FloatLit:
      inc pc
    of OpenTagKind:
      #echo "PC IS: ", pool.tags[pc.tag]
      if pc.cfKind == IteF:
        inc pc
        # `containsRoot` (not `containsUsage`): an `if` condition that reads the
        # whole root — or derefs it — after a partial move of `x = a.field`
        # still uses the moved location; `containsUsage` matched only the exact
        # path and missed it. `containsRoot` is sound here and skips truly
        # disjoint sibling fields (see `disjointDirectField`).
        if containsRoot(pc, x):
          otherUsage = pc
          return false
        # now 2 goto instructions follow:
        let a = pc +! pc.getInt28
        inc pc
        let b = pc +! pc.getInt28
        # we follow the second goto and remember the first one:
        if not marks.contains(cursorToPosition(cfBase, a)):
          pcs.add a
        pc = b
      else:
        case pc.stmtKind
        of AsgnS:
          # NOTE: this body does NOT fully consume the asgn (it reads the RHS
          # without advancing), so `into` is wrong here — use the parent-cursor
          # form, which jumps past the whole subtree regardless of consumption
          # (like the old forgiving `leaveScope`).
          let asgnStart = pc
          pc = sub(pc)
          let lhsRedefinesRoot = (pc.kind == Symbol and pc.symId == root) or
                                 sameTrees(pc, x)
          skip pc # skip left-hand-side; pc now at the right-hand-side
          # The RHS is evaluated *before* the store, so a read of the old value
          # here (as in `x = f(x)`) means the earlier occurrence is NOT the last
          # use — even when the LHS fully redefines `root`. This MUST be checked
          # before the redefinition `break`; otherwise `x = f(x)` wrongly sinks a
          # still-live `x` (moving it into `f`'s arg before `f` reads it).
          # Use `containsRoot`, not `containsUsage`: when `x` is a partial path
          # like `a.field`, a later *whole-object* read of `a` (e.g. `c = (emove
          # a)`) still reads the moved field, but `containsUsage` only matched
          # the exact path and missed it — wrongly sinking `a.field` so the later
          # whole-object move restored an emptied field. `containsRoot` is sound
          # (it also catches deref-of-root) and lets a disjoint sibling read
          # `a.other` through (see `disjointDirectField`).
          if containsRoot(pc, x):
            # the RHS reads 's' (or only partially writes it) --> can't sink 's'.
            otherUsage = pc # XXX Fixme: pc advanced to ')'
            return false
          if lhsRedefinesRoot:
            # pure redefinition of 's' (old value overwritten unread) --> sink 's'.
            break
          pc = asgnStart; skip pc
        of RetS:
          break
        of StmtsS, ScopeS, BlockS, ContinueS, BreakS:
          inc pc
          inc nested
        of PragmaxS:
          inc pc
          skip pc # pragma itself
          inc nested
        of LocalDecls:
          inc pc
          if root != NoSymId and pc.kind == SymbolDef and pc.symId == root:
            # found the definition, so it gets a new value:
            break
          skip pc # name
          skip pc # export marker
          skip pc # pragmas
          skip pc # type
          inc nested
          # proceed with its value here
        of NoStmt, CallKindsS, DiscardS, EmitS, InclS, ExclS:
          if containsRoot(pc, x):
            otherUsage = pc # XXX Fixme: pc advanced to ')'
            return false
        of YldS:
          # bare `(yld .)` from closure-iter rewrite: a control-flow marker
          # with no operand. Treat as a sequence point — value, if any, was
          # already written by the preceding `(asgn result v)`.
          if containsRoot(pc, x):
            otherUsage = pc
            return false
        of IfS, WhenS, WhileS, ForS, CoroforS, CaseS, TryS, RaiseS,
           ExportS, IncludeS, ImportS, FromimportS, ImportexceptS, CommentS,
           PragmasS, ImportasS, ExportexceptS, BindS, MixinS, UsingS,
           UnpackdeclS, StaticstmtS, AsmS, DeferS:
          bug "statement not eliminated: " & $pc.stmtKind
        of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS,
           AssumeS, AssertS:
          # declarative junk we don't care about:
          skip pc
    else:
      inc pc   # LineInfoLit / stray suffix — just advance
  return true

proc isLastReadImpl(c: TokenBuf; idx: uint32; otherUsage: var Cursor;
                    index: FindStartIndex): bool =
  var n = default Cursor
  let nested = findStart(c, int(idx), n, index)
  if nested < 0:
    return true
  let x = n
  skip n
  # step over the (real) closes that separate `x` from the next CF
  # instruction; under ParRi elision there are none to step over
  let cfBase = c.readonlyCursorAt(0)
  var pcs = @[n]
  var marks = initIntSet()
  while pcs.len > 0:
    let pc = pcs.pop()
    let pcPos = cursorToPosition(cfBase, pc)
    if not marks.contains(pcPos):
      if not singlePath(pc, nested, x, pcs, otherUsage, marks, cfBase):
        return false
      marks.incl pcPos
  return true

proc isLastUse*(n: Cursor; buf: var TokenBuf;
                otherUsage: var PackedLineInfo;
                mover: var MoverContext): bool =
  # XXX Todo: only transform&traverse the innermost scope the variable was declared in.
  if mover.cf.len == 0:
    # First call for this `buf`: build the CF and, alongside it, the `srcMap`
    # side-channel mapping every CF token back to its source position. `buf`
    # itself is left untouched (the old scheme stamped payloads into `buf.info`
    # and restored them; nifcore tokens may carry no info, so the mapping lives
    # in a dedicated seq instead). The same scan then inverts `srcMap` into the
    # source-position → position index so `findStart` runs in O(1) per query.
    var srcMap: seq[int32] = @[]
    mover.cf = toControlflowWithMap(beginRead buf, srcMap)
    freeze mover.cf
    endRead buf
    mover.index = buildFindStartIndex(mover.cf, srcMap)
  let idx = cursorToPosition(buf, n)
  assert idx >= 0
  var other = default Cursor
  result = isLastReadImpl(mover.cf, idx.uint32, other, mover.index)
  if other.cursorIsNil or not other.hasCurrentToken:
    # `other` can sit at a scope's end (see the "pc advanced to ')'"
    # notes in singlePath); there is no token to read info from then.
    otherUsage = NoLineInfo
  else:
    otherUsage = other.info

when isMainModule:
  proc findX(n: Cursor): Cursor =
    var n = n
    linearScan n:
      if n.exprKind == EmoveX:
        result = n
        inc result
        return result
    bug "no 'ensureMove' found"

  proc test(s: string; expected: bool) =
    var input = parseFromBuffer(s, "")
    var otherUsage = NoLineInfo
    let n = findX(beginRead(input))
    var mover = MoverContext(cf: createTokenBuf(300))
    let res = isLastUse(n, input, otherUsage, mover)
    if res != expected:
      echo "FAILED Test case: ", s

  const BasicTest1 = """(stmts
  (let :my.var . . (array (i +8) +6) .)
  (var :i.0 . . (i -1) +0)
  (asgn (arrat my.var i.0) +56)

  (discard (emove my.var))
  (call use my.var)

  )
  """

  const BasicTest2 = """(stmts
  (let :my.var . . (array (i +8) +6) .)
  (var :i.0 . . (i -1) +0)
  (asgn (arrat my.var i.0) +56)

  (discard (emove my.var))

  )
  """

  test BasicTest1, false
  test BasicTest2, true

  const LoopTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (while (true)
      (discard (emove my.var))
    )

  )"""
  test LoopTest, false

  const LoopTestB = """(stmts
    (while (true) (stmts
      (var :my.var . . (array (i +8) +6) .)
      (discard (emove my.var))
    ))

  )"""
  test LoopTestB, true
