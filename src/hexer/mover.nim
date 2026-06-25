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
    pos: int32       ## position in `cf`, or -1 if no CF token has this payload
    nested: int16    ## paren-nesting depth at that position

  MoverContext* = object
    ## Per-pass context for the mover. Holds the controlflow buffer and a
    ## position-payload → (cf position, nested depth) lookup built once when
    ## the CF is materialized. Replaces the bare `cf: TokenBuf` the duplifier
    ## used to carry around, and turns `findStart` from an O(N) buffer scan
    ## into an O(1) array index.
    cf*: TokenBuf
    index*: seq[FindStartEntry]   ## indexed by payload value (= srcPos + PayloadOffset)

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
  var nested = 0
  let isAtom = a.kind != ParLe
  while true:
    if a.kind != b.kind: return false
    case a.kind
    of ParLe:
      if a.tagId != b.tagId: return false
      if a.exprKind in {PatX, ArratX}:
        inc a
        inc b
        if not sameTreesIgnoreArrayIndexes(a, b):
          return false
        # do not compare the array indexes:
        while a.hasMore: skip a
        consumeParRi a
        while b.hasMore: skip b
        consumeParRi b
      else:
        inc a
        inc b
        inc nested
    of ParRi:
      dec nested
      if nested == 0: return true
    of Symbol, SymbolDef:
      if a.symId != b.symId: return false
    of IntLit:
      if a.intId != b.intId: return false
    of UIntLit:
      if a.uintId != b.uintId: return false
    of FloatLit:
      if a.floatId != b.floatId: return false
    of StringLit, Ident:
      if a.litId != b.litId: return false
    of CharLit, UnknownToken:
      if a.uoperand != b.uoperand: return false
    of DotToken, EofToken: discard "nothing else to compare"
    if isAtom: return true
    inc a
    inc b
  return false

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
  if tree.kind == ParLe and x.kind == ParLe and tree.exprKind == x.exprKind:
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
  # scan loop also correct for `r == NoSymId`:
  var nested = 0
  result = false
  while true:
    case tree.kind
    of Symbol:
      if tree.symId == r:
        # MUST continue here as we must `skip tree` properly
        result = true
      inc tree
    of ParLe:
      if disjointDirectField(tree, r, x):
        # A sibling field/index that cannot alias `x`: skip the whole subtree.
        # `skip` consumed its matching `)` too, so `nested` is unchanged;
        # replicate the loop's terminator before continuing, or a disjoint path
        # that *is* the top-level scanned expression would run the scan off the
        # end into the following CF instructions.
        skip tree
        if nested == 0: break
        continue
      elif tree.exprKind == DotX:
        inc tree
        if containsRoot(tree, x):
          result = true
        while tree.hasMore:
          skip tree
      elif tree.substructureKind == KvU:
        inc tree
        skip tree # key ignored for object construction!
      else:
        inc tree
      inc nested
    of ParRi:
      dec nested
      inc tree
    else:
      inc tree
    if nested == 0: break

proc buildFindStartIndex(cf: TokenBuf; srcLen: int): seq[FindStartEntry] =
  ## One-pass scan of the just-built CF buffer that records, for each
  ## payload value, the *first* CF token whose `info` carries it and the
  ## paren-nesting depth at that position. Subsequent `findStart` lookups
  ## are then O(1).
  result = newSeq[FindStartEntry](srcLen + int(PayloadOffset))
  for i in 0 ..< result.len:
    result[i] = FindStartEntry(pos: -1'i32, nested: 0)
  var nested = 0
  for i in 0 ..< cf.len:
    case cf[i].kind
    of ParLe: inc nested
    of ParRi: dec nested
    else: discard
    let info = cf[i].info
    if isPayload(info):
      let p = int(getPayload(info))
      if p < result.len and result[p].pos < 0:
        result[p] = FindStartEntry(pos: int32(i), nested: int16(nested))

proc findStart(c: TokenBuf; idx: PackedLineInfo; n: var Cursor;
               index: openArray[FindStartEntry]): int =
  if not isPayload(idx):
    return -1
  let p = int(getPayload(idx))
  if p < 0 or p >= index.len: return -1
  let e = index[p]
  if e.pos < 0: return -1
  n = c.readonlyCursorAt(int(e.pos))
  result = int(e.nested)

proc singlePath(pc: Cursor; nested: int; x: Cursor; pcs: var seq[Cursor];
                otherUsage: var Cursor; marks: var IntSet; cfBase: Cursor): bool =
  var nested = nested
  var pc = pc
  let root = rootOf(x)
  while true:
    #echo "PC IS: ", pc.kind
    case pc.kind
    of GotoInstr:
      let diff = pc.getInt28
      assert diff != 0
      if diff < 0:
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
    of ParRi:
      if nested == 0:
        bug "unpaired ')'"
      dec nested
      inc pc
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
    of EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
      inc pc
    of ParLe:
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
          inc pc
          if (pc.kind == Symbol and pc.symId == root) or sameTrees(pc, x):
            # the path leads to a redefinition of 's' --> sink 's'.
            break
          skip pc # skip left-hand-side
          # right-hand-side is a simple use expression. Use `containsRoot`, not
          # `containsUsage`: when `x` is a partial path like `a.field`, a later
          # *whole-object* read of `a` (e.g. `c = (emove a)`) still reads the
          # moved field, but `containsUsage` only matched the exact path and
          # missed it — wrongly sinking `a.field` so the later whole-object move
          # restored an emptied field. `containsRoot` is sound (it also catches
          # deref-of-root) and lets a disjoint sibling read `a.other` through
          # (see `disjointDirectField`).
          if containsRoot(pc, x):
            # only partially writes to 's' --> can't sink 's', so this def reads 's'
            # or maybe writes to 's' --> can't sink 's'
            otherUsage = pc # XXX Fixme: pc advanced to ')'
            return false
          skipParRi pc
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
  return true

proc isLastReadImpl(c: TokenBuf; idx: uint32; otherUsage: var Cursor;
                    index: openArray[FindStartEntry]): bool =
  var n = default Cursor
  let nested = findStart(c, toPayload(idx + PayloadOffset), n, index)
  if nested < 0:
    return true
  let x = n
  skip n
  while n.kind == ParRi: inc n
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
    # First call for this `buf`: bake the payload-encoded back-pointers into
    # `buf.info`, build the CF from it, then restore `buf` to its original
    # infos. The CF inherits the payloads and keeps them for the lifetime of
    # this analysis pass — they never change once built, because per-walk
    # visited marks now live in a side IntSet (see isLastReadImpl), not in
    # the `info` field. The same scan also builds the payload→position index
    # so `findStart` runs in O(1) instead of O(cf.len) per query.
    let oldInfos = prepare(buf)
    mover.cf = toControlflow(beginRead buf)
    freeze mover.cf
    endRead buf
    restore(buf, oldInfos)
    mover.index = buildFindStartIndex(mover.cf, buf.len)
  let idx = cursorToPosition(buf, n)
  assert idx >= 0
  var other = default Cursor
  result = isLastReadImpl(mover.cf, idx.uint32, other, mover.index)
  if other.cursorIsNil:
    otherUsage = NoLineInfo
  else:
    otherUsage = other.info

when isMainModule:
  proc findX(n: Cursor): Cursor =
    result = n
    var nested = 0
    while true:
      case result.kind
      of ParLe:
        if result.exprKind == EmoveX:
          inc result
          return result
        inc nested
      of ParRi:
        dec nested
      else:
        discard
      if nested == 0: break
      inc result
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
