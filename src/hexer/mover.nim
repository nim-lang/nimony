#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Move analyser: "is this read of a location its *last* read?".

The analysis runs **directly on the Final IR** (`doc/final_ir.md`) — the form
every hexer pass reads and writes — instead of translating the module into a
second, goto-based representation first.

What that translation used to cost: `controlflow.nim` built a whole parallel
`TokenBuf` for the module, plus a `srcMap` side-channel mapping every CF token
back to the source token it came from, plus a `FindStartIndex` that inverted
`srcMap` so the query expression could be *found again* in the copy. Three data
structures whose only job was to undo the translation — and the side-channel
reached into the CF builder itself, which had to keep it aligned through every
buffer it moved or truncated.

None of that is needed once the walk happens on the analysed buffer itself: the
query is already a cursor into it, so its position is the answer `findStart`
used to compute. Final IR is close enough to a flat instruction stream that the
one thing the goto form really provided — "what runs next" — is recovered from a
single `parents` array (token position -> position of the enclosing tag), built
once per module:

- `(stmts …)` / `(scope …)`  — the next sibling, else whatever follows the block
- `(ite cond then else)`     — the condition forks into both branches
- `(loop body)`              — falling off the body is the back-edge
- `(continue .)`             — the back-edge, taken explicitly
- `(jmp L)` / `(lab L)`      — forward and scoped, so a walk up the chain
- `(case sel (of …)* (else …)?)` — the selector forks into the branches
- `(try body (except …)* (fin …)?)` — body and handlers are all successors
- `(ret v)` / `(raise v)`    — read `v`, then the path ends

Reasoning about the same tree the duplifier rewrites also removes a whole class
of bug: the goto form could impose an evaluation order that a later pass then
contradicted (see the `AndX`/`OrX` note in `xelim.isComplex`).
]##

import std / [assertions, intsets]

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs]
import ".." / finalir / finalir_model

type
  RootOfMode* = enum
    CanFollowDerefs, CannotFollowDerefs, CanFollowCalls

  MoverContext* = object
    ## Per-pass context. `parents` is built once, lazily, from the buffer the
    ## pass is reading; it does not change while the pass runs.
    parents: seq[int32]
      ## `parents[i]` is the position of the tag whose subtree directly
      ## contains token `i`, or -1 for the root. This is what makes "what runs
      ## after this subtree" answerable from any position in the middle of the
      ## tree, which is what a flat goto stream gave for free.
    built: bool

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
    of NoExpr, ErrX, SufX, ParX, NilX, InfX, NeginfX, NanX, FalseX, TrueX,
       AndX, OrX, XorX, NotX, NegX, SizeofX, AlignofX, OffsetofX, OconstrX,
       AconstrX, BracketX, CurlyX, CurlyatX, KvX, OvfX, AddX, SubX, MulX,
       DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX,
       LeX, LtX, CchoiceX, OchoiceX, PragmaxX, QuotedX, DdotX, NewrefX,
       NewobjX, TupX, TupconstrX, SetconstrX, TabconstrX, AshrX, CompilesX,
       DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, InstanceofX,
       HighX, LowX, TypeofX, UnpackX, FieldsX, FieldpairsX, EnumtostrX,
       IsmainmoduleX, InstantiationinfoX, DefaultobjX, DefaulttupX,
       DefaultdistinctX, Delay0X, SuspendX, ExprX, DoX, PlussetX, MinussetX,
       MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
       DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, InternalTypeNameX,
       InternalFieldPairsX, FailedX, IsX, EnvpX, ToClosureX, PluginCallX:
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
  of TagLit:
    if a.cursorTagId != b.cursorTagId: return false
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
    result = a.intVal == b.intVal
  of UIntLit:
    result = a.uintVal == b.uintVal
  of FloatLit:
    result = a.floatVal == b.floatVal
  of StrLit, Ident:
    result = a.strId == b.strId
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
  if tree.kind == TagLit and x.kind == TagLit and tree.exprKind == x.exprKind:
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
      # An `if` chain rather than a `case`, for the reason `xelim.trExprToLabel`
      # gives: two kinds are interesting and everything else shares one
      # fall-back, and here that fall-back is the conservative answer -- an
      # accessor this does not know stays "may alias" and the full scan runs.
      if tree.exprKind == DotX:
        # disjoint iff the two field names differ
        result = treeSel.kind == Symbol and xSel.kind == Symbol and
                 treeSel.symId != xSel.symId
      elif tree.exprKind == TupatX:
        # disjoint iff the two tuple indices differ
        result = treeSel.kind == IntLit and xSel.kind == IntLit and
                 treeSel.intVal != xSel.intVal

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
  of TagLit:
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

# ------------------- navigating the Final IR ---------------------------

type
  NodeClass = enum
    ## How a node's children relate to control flow. Only this much of the
    ## grammar matters for "what runs next".
    ncOther      ## an expression, or a statement whose operands all run
    ncStmtList   ## `stmts`/`scope`: children are consecutive statements
    ncRoutine    ## a routine: its body is a control-flow boundary
    ncLoop       ## `(loop body)`: falling off `body` is the back-edge
    ncIte        ## `(ite cond then else)` / `(itec …)`
    ncCase       ## `(case sel (of …)* (else …)?)`
    ncTry        ## `(try body (except …)* (fin …)?)`
    ncBranch     ## `of`/`else`/`except`/`fin`: the body is the last child
    ncExit       ## `ret`/`raise`: evaluating the operand is the last thing
                 ## that happens on this path

const
  StmtListKinds = {StmtsS, ScopeS, UnpackdeclS, StaticstmtS}
  BlockLikeKinds = StmtListKinds + {PragmaxS}
    ## `pragmax` joins them for `classify` only: its children are statements, so
    ## a child that completes continues with the next one. `execStmt` still
    ## enters it past the pragma list.
  RoutineKinds = {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS}
  BranchKinds = {OfU, ElseU, ExceptU, FinU}
  SelectorOnlyExprs = {DotX, DdotX, TupatX, BaseobjX}
    ## Accessors whose trailing operands are a field name, a tuple index or an
    ## inheritance depth — never a read. `containsRoot` skips them for the same
    ## reason; the sibling scan in `afterNode` must too.

proc classify(n: Cursor): NodeClass =
  case n.stmtKind
  of BlockLikeKinds: result = ncStmtList
  of RoutineKinds: result = ncRoutine
  of CaseS: result = ncCase
  of TryS: result = ncTry
  of RetS, RaiseS: result = ncExit
  of NoStmt:
    case n.finalIrKind
    of IteV, ItecV: result = ncIte
    of LoopV: result = ncLoop
    else:
      if n.substructureKind in BranchKinds: result = ncBranch
      else: result = ncOther
  of CallS, CmdS, GvarS, TvarS, VarS, ConstS, ResultS, GletS, TletS, LetS,
     CursorS, PatternvarS, TypeS, EmitS, AsgnS, ContinueS, LabS, JmpS, YldS,
     PragmasS, InclS, ExclS, IncludeS, ImportS, ImportasS, FromimportS,
     ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS, AssumeS,
     AssertS, CallstrlitS, InfixS, PrefixS, HcallS, BindS, MixinS, UsingS:
    # Nothing here forks or joins: whatever follows the node follows the
    # statement. `jmp`/`lab`/`continue` are transfers `execStmt` resolves by
    # name, not by where they sit, so they need no class of their own.
    result = ncOther
  of IfS, WhenS, WhileS, ForS, CoroforS, BlockS, BreakS, AsmS, DeferS:
    # `finalir.nim` lowered all of these; `execStmt` `bug`s on one, and until
    # then it is a node like any other.
    result = ncOther

proc at(base: Cursor; pos: int32): Cursor {.inline.} = base +! int(pos)

proc endOf(base: Cursor; pos: int32): int32 {.inline.} =
  pos + int32(subtreeWidth(at(base, pos)))

proc firstChild(base: Cursor; pos: int32): int32 {.inline.} =
  pos + int32(tokenWidth(at(base, pos)))

proc lastChild(base: Cursor; pos: int32): int32 =
  ## -1 when the tag at `pos` has no children at all, so a caller never reads
  ## past the subtree.
  let fin = endOf(base, pos)
  result = -1
  var p = firstChild(base, pos)
  while p < fin:
    result = p
    p = endOf(base, p)

proc enclosingLoop(m: MoverContext; base: Cursor; pos: int32): int32 =
  ## The `(loop …)` a `(continue .)` at `pos` belongs to, or -1.
  result = -1
  var child = pos
  while true:
    let par = m.parents[child]
    if par < 0: break
    let p = at(base, par)
    if p.finalIrKind == LoopV:
      result = par
      break
    if p.stmtKind in RoutineKinds: break
    child = par

proc labelAfter(m: MoverContext; base: Cursor; jmpPos: int32; lab: SymId): int32 =
  ## Where a `(jmp lab)` at `jmpPos` lands. `jmp` is forward-only and scoped
  ## (`doc/final_ir.md`), so its `(lab lab)` is a *later direct child* of one of
  ## the enclosing statement lists — the same guarantee `destroyer.collectLabels`
  ## relies on. Resolving it this way rather than through a module-wide
  ## symbol->position table is what keeps two routines that mint the same label
  ## name apart.
  result = -1
  var child = jmpPos
  while true:
    let par = m.parents[child]
    if par < 0: break
    let p = at(base, par)
    if classify(p) == ncStmtList:
      let fin = endOf(base, par)
      var q = endOf(base, child)
      while q < fin:
        let s = at(base, q)
        if s.stmtKind == LabS:
          let name = childCursor(s)
          if name.kind in {Symbol, SymbolDef} and name.symId == lab:
            return q
        q = endOf(base, q)
    if p.stmtKind in RoutineKinds: break
    child = par

proc pushHandlers(m: MoverContext; base: Cursor; tryPos: int32;
                  pcs: var seq[int32]) =
  ## Push the `(except …)`/`(fin …)` children of the `(try …)` at `tryPos`.
  let fin = endOf(base, tryPos)
  var b = endOf(base, firstChild(base, tryPos)) # past the protected body
  while b < fin:
    pcs.add b
    b = endOf(base, b)

proc pushEnclosingHandlers(m: MoverContext; base: Cursor; pos: int32;
                           pcs: var seq[int32]) =
  ## A query that sits *inside* a `try` can still be followed by that `try`'s
  ## handlers: the walk never passed the `(try …)` itself, so nothing pushed
  ## them. (The goto form modelled this edge only for a walk that entered the
  ## `try` from outside.)
  var child = pos
  while true:
    let par = m.parents[child]
    if par < 0: break
    let p = at(base, par)
    if p.stmtKind in RoutineKinds: break
    if p.stmtKind == TryS and at(base, child).substructureKind != FinU:
      pushHandlers(m, base, par, pcs)
    child = par

proc afterNode(m: MoverContext; base: Cursor; pos: int32; x: Cursor;
               pcs: var seq[int32]; otherUsage: var Cursor): bool =
  ## `pos` has just been evaluated: push where control goes next. While walking
  ## out of the enclosing *expression* the operands that still run as part of
  ## the same statement are scanned — `f(a, g(a))` reads `a` twice. Returns
  ## false (and sets `otherUsage`) as soon as a read of `x` is found.
  result = true
  var child = pos
  while true:
    let par = m.parents[child]
    if par < 0: return                # ran off the buffer: nothing follows
    let p = at(base, par)
    let parEnd = endOf(base, par)
    let childEnd = endOf(base, child)
    case classify(p)
    of ncStmtList:
      if childEnd < parEnd:
        pcs.add childEnd              # the next statement of the block
        return
      child = par                     # block finished: continue after it
    of ncRoutine:
      return                          # a routine body ends the walk
    of ncLoop:
      pcs.add firstChild(base, par)   # the back-edge
      return
    of ncIte:
      if childEnd >= parEnd:
        child = par                   # nothing follows the arm
      elif child == firstChild(base, par):
        # the condition has been evaluated: both arms are successors
        pcs.add childEnd              # then-part
        let elsePos = endOf(base, childEnd)
        if elsePos < parEnd and not at(base, elsePos).isDotToken:
          pcs.add elsePos
          return
        child = par                   # no else-part: fall through
      else:
        child = par                   # an arm finished: continue after the ite
    of ncCase:
      if child == firstChild(base, par):
        var b = childEnd
        var exhaustive = false
        while b < parEnd:
          if at(base, b).substructureKind == ElseU: exhaustive = true
          pcs.add b
          b = endOf(base, b)
        if exhaustive: return
        child = par                   # no else-part: the case may fall through
      else:
        child = par
    of ncTry:
      if at(base, child).substructureKind == FinU:
        child = par                   # the cleanup ran: continue after the try
      else:
        let fin = lastChild(base, par)
        if fin >= 0 and at(base, fin).substructureKind == FinU:
          pcs.add firstChild(base, fin)
          return
        child = par
    of ncExit:
      # The operand of a `(ret v)`/`(raise v)` the query sat in: control leaves
      # here, so nothing lexically after it runs. (A `raise`'s handlers were
      # already scheduled by `pushEnclosingHandlers`.) Walking on would follow
      # the *fall-through* of the enclosing `ite` instead — which is how a
      # `return` inside an early-exit arm used to look like a later use.
      return
    of ncBranch:
      child = par                     # a branch body finished
    of ncOther:
      if p.exprKind notin SelectorOnlyExprs:
        var q = childEnd
        while q < parEnd:
          var t = at(base, q)
          let nxt = endOf(base, q)
          if containsRoot(t, x):
            otherUsage = at(base, q)
            return false
          q = nxt
      child = par

proc scanAt(base: Cursor; pos: int32; x: Cursor; otherUsage: var Cursor): bool =
  ## True if the subtree at `pos` reads the location `x` stands for.
  var t = at(base, pos)
  result = containsRoot(t, x)
  if result: otherUsage = at(base, pos)

proc execStmt(m: MoverContext; base: Cursor; pc: int32; x: Cursor; root: SymId;
              pcs: var seq[int32]; otherUsage: var Cursor): bool =
  ## Run one Final IR statement through the analysis and schedule its
  ## successors. Returns false when a read of `x` is found: then this is not
  ## the last read and the whole query is answered.
  let n = at(base, pc)
  case n.stmtKind
  of StmtListKinds:
    let fc = firstChild(base, pc)
    if fc < endOf(base, pc):
      pcs.add fc
      result = true
    else:
      result = afterNode(m, base, pc, x, pcs, otherUsage)
  of PragmaxS:
    # `(pragmax pragmas stmt…)`: the pragmas evaluate nothing, the rest are
    # ordinary statements — `classify` calls it a statement list so that
    # falling off the last one continues past the `pragmax`.
    let body = endOf(base, firstChild(base, pc))
    if body < endOf(base, pc):
      pcs.add body
      result = true
    else:
      result = afterNode(m, base, pc, x, pcs, otherUsage)
  of RetS, RaiseS:
    # the operand is read here; afterwards this path is over (a `raise` that
    # lands in a handler is covered: entering the `try` pushed them).
    let v = firstChild(base, pc)
    result = v >= endOf(base, pc) or not scanAt(base, v, x, otherUsage)
  of JmpS:
    let target = at(base, firstChild(base, pc))
    let landing = if target.kind == Symbol: labelAfter(m, base, pc, target.symId)
                  else: -1'i32
    if landing >= 0:
      pcs.add landing
      result = true
    else:
      result = false # unresolvable transfer: assume the location is still live
  of LabS:
    result = afterNode(m, base, pc, x, pcs, otherUsage)
  of ContinueS:
    let loop = enclosingLoop(m, base, pc)
    if loop >= 0: pcs.add firstChild(base, loop)
    result = true
  of AsgnS:
    let lhsPos = firstChild(base, pc)
    let lhs = at(base, lhsPos)
    let lhsRedefinesRoot = (lhs.kind == Symbol and lhs.symId == root) or
                           sameTrees(lhs, x)
    if not lhsRedefinesRoot:
      # A *partial* write — `x.field = v`, `x[i] = v`, `x[] = v` — still READS
      # the base `x`: for a `ref` it dereferences the pointer, for a value
      # object it keeps the object live. `containsRoot` skips statically
      # disjoint sibling fields, so `a.other = v` after a move of `a.field`
      # still sinks.
      if scanAt(base, lhsPos, x, otherUsage): return false
    # The RHS is evaluated *before* the store, so a read of the old value here
    # (as in `x = f(x)`) means the earlier occurrence is NOT the last use —
    # even when the LHS fully redefines `root`. This MUST be checked before the
    # redefinition case below.
    let rhsPos = endOf(base, lhsPos)
    if rhsPos < endOf(base, pc) and scanAt(base, rhsPos, x, otherUsage):
      return false
    if lhsRedefinesRoot:
      result = true # old value overwritten unread: this path is done
    else:
      result = afterNode(m, base, pc, x, pcs, otherUsage)
  of VarS, LetS, CursorS, PatternvarS, ConstS, ResultS, GvarS, TvarS, GletS, TletS:
    let namePos = firstChild(base, pc)
    let name = at(base, namePos)
    let value = lastChild(base, pc) # name, export marker, pragmas, type, VALUE
    if root != NoSymId and name.kind == SymbolDef and name.symId == root:
      result = true # the declaration itself: the location gets a new value
    elif value >= 0 and scanAt(base, value, x, otherUsage):
      result = false
    else:
      result = afterNode(m, base, pc, x, pcs, otherUsage)
  of CaseS:
    let selPos = firstChild(base, pc)
    if scanAt(base, selPos, x, otherUsage):
      result = false
    else:
      result = afterNode(m, base, selPos, x, pcs, otherUsage)
  of TryS:
    pcs.add firstChild(base, pc)
    pushHandlers(m, base, pc, pcs)
    result = true
  of NoStmt:
    case n.finalIrKind
    of IteV, ItecV:
      let condPos = firstChild(base, pc)
      if scanAt(base, condPos, x, otherUsage):
        result = false
      else:
        result = afterNode(m, base, condPos, x, pcs, otherUsage)
    of LoopV:
      pcs.add firstChild(base, pc)
      result = true
    of KillV, UnknownV:
      result = afterNode(m, base, pc, x, pcs, otherUsage) # facts, not code
    else:
      if n.substructureKind in BranchKinds:
        pcs.add lastChild(base, pc) # `of`/`else`/`except`/`fin`: run the body
        result = true
      elif scanAt(base, pc, x, otherUsage): # a bare expression statement
        result = false
      else:
        result = afterNode(m, base, pc, x, pcs, otherUsage)
  of CallS, CmdS, CallstrlitS, InfixS, PrefixS, HcallS, DiscardS, EmitS,
     InclS, ExclS, YldS:
    # `(yld .)` from the closure-iter rewrite carries no operand; scanning it
    # is harmless and keeps the shape uniform.
    if scanAt(base, pc, x, otherUsage):
      result = false
    else:
      result = afterNode(m, base, pc, x, pcs, otherUsage)
  of RoutineKinds + {TypeS,
     AssumeS, AssertS, CommentS, PragmasS, IncludeS, ImportS,
     ImportasS, FromimportS, ImportexceptS, ExportS, ExportexceptS, BindS,
     MixinS, UsingS}:
    # declarative junk we don't care about: it evaluates nothing
    result = afterNode(m, base, pc, x, pcs, otherUsage)
  of IfS, WhenS, WhileS, ForS, CoroforS, BlockS, BreakS, AsmS, DeferS:
    # `finalir.nim` lowered all of these; seeing one means a pass regressed the
    # normal form (see `doc/final_ir.md`).
    bug "statement not eliminated: " & $n.stmtKind

proc isLastReadImpl(m: MoverContext; buf: TokenBuf; xPos: int32; x: Cursor;
                    otherUsage: var Cursor): bool =
  let base = readonlyCursorAt(buf, 0)
  let root = rootOf(x)
  var pcs: seq[int32] = @[]
  # The rest of the statement `x` sits in is evaluated right after it; from
  # there the ordinary statement walk takes over.
  if not afterNode(m, base, xPos, x, pcs, otherUsage): return false
  pushEnclosingHandlers(m, base, xPos, pcs)
  var marks = initIntSet()
  while pcs.len > 0:
    let pc = pcs.pop()
    if pc < 0 or marks.containsOrIncl(int pc): continue
    if not execStmt(m, base, pc, x, root, pcs, otherUsage): return false
  result = true

proc build(m: var MoverContext; buf: TokenBuf) =
  ## One scan over the module recording each token's enclosing tag. nifcore
  ## materialises no closing token, so a tag's extent comes from `subtreeWidth`
  ## and is tracked on a stack of last-content indexes.
  m.parents = newSeq[int32](buf.len)
  # The stack of tags still open at `i`, as two parallel columns: where the tag
  # sits and the index of its last content token.
  var openAt: seq[int32] = @[]
  var openLast: seq[int32] = @[]
  for i in 0 ..< buf.len:
    while openLast.len > 0 and openLast[openLast.len-1] < int32(i):
      discard openAt.pop()
      discard openLast.pop()
    if openAt.len > 0:
      m.parents[i] = openAt[openAt.len-1]
    else:
      m.parents[i] = -1'i32
    if buf[i].kind == TagLit:
      let c = readonlyCursorAt(buf, i)
      openAt.add int32(i)
      openLast.add int32(i + subtreeWidth(c) - 1)
  m.built = true

proc isLastUse*(n: Cursor; buf: var TokenBuf;
                otherUsage: var NifLineInfo;
                mover: var MoverContext): bool =
  if not mover.built:
    build(mover, buf)
  let idx = cursorToPosition(buf, n)
  assert idx >= 0
  var other = default Cursor
  result = isLastReadImpl(mover, buf, int32(idx), n, other)
  if other.cursorIsNil or not other.hasCurrentToken:
    # `other` can sit at a scope's end; there is no token to read info from.
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
    var mover = MoverContext()
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

  # Final IR spells a loop as `(loop …)` with an explicit `(continue .)`
  # back-edge; there is no condition slot.
  const LoopTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (loop (scope
      (discard (emove my.var))
      (continue .)))

  )"""
  test LoopTest, false

  const LoopTestB = """(stmts
    (loop (scope
      (var :my.var . . (array (i +8) +6) .)
      (discard (emove my.var))
      (continue .)))

  )"""
  test LoopTestB, true

  # `(ite cond then else)`: a use in either arm defeats the move, a
  # redefinition in both arms does not.
  const IteUseTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (discard (emove my.var))
    (ite (true)
      (scope (call use my.var))
      (scope (discard +1)))

  )"""
  test IteUseTest, false

  const IteRedefTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (discard (emove my.var))
    (ite (true)
      (scope (asgn my.var +4))
      (scope (asgn my.var +5)))
    (call use my.var)

  )"""
  test IteRedefTest, true

  # A missing else-part is a fall-through edge, so the use after the `ite`
  # is still reachable.
  const IteNoElseTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (discard (emove my.var))
    (ite (true)
      (scope (asgn my.var +4)).)
    (call use my.var)

  )"""
  test IteNoElseTest, false

  # `(jmp L)` skips the use in between and lands on `(lab L)`.
  const JmpTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (discard (emove my.var))
    (jmp L.0)
    (call use my.var)
    (lab :L.0)

  )"""
  test JmpTest, true

  # A `case` forks into every branch.
  const CaseTest = """(stmts
    (var :my.var . . (array (i +8) +6) .)
    (var :i.0 . . (i -1) +0)
    (discard (emove my.var))
    (case i.0
      (of (ranges +0) (scope (discard +1)))
      (else (scope (call use my.var))))

  )"""
  test CaseTest, false
