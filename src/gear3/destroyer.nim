#
#
#           Gear3 Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

##[

The destroyer runs after `to_stmts` as it relies on `var tmp = g()`
injections. It only destroys variables and transforms assignments.

Statements
==========

Assignments and var bindings need to use `=dup`. In the first version, we don't
emit `=copy`.

`x = f()` is turned into `=destroy(x); x =bitcopy f()`.
`x = lastUse y` is turned into either

  `=destroy(x); x =bitcopy y; =wasMoved(y)` # no self assignments possible

or

  `let tmp = y; =wasMoved(y); =destroy(x); x =bitcopy tmp`  # safe for self assignments

`x = someUse y` is turned into either

  `=destroy(x); x =bitcopy =dup(y)` # no self assignments possible

or

  `let tmp = x; x =bitcopy =dup(y); =destroy(tmp)` # safe for self assignments

`var x = f()` is turned into `var x = f()`. There is nothing to do because the backend
interprets this `=` as `=bitcopy`.

`var x = lastUse y` is turned into `var x = y; =wasMoved(y)`.
`var x = someUse y` is turned into `var x = =dup(y)`.

]##

include nifprelude
import nifindexes, symparser, treemangler
import ".." / nimony / [nimony_model, programs, typenav]
import lifter

const
  NoLabel = SymId(-2)
  AnonBlock = SymId(-3)

type
  ScopeKind = enum
    Other
    WhileOrBlock
  DestructorOp = object
    destroyProc: SymId
    arg: SymId

  Scope = object
    p: Program
    label: SymId
    thisModule: ModuleId
    procStart: Cursor
    kind: ScopeKind
    destroyOps: seq[DestructorOp]
    info: PackedLineInfo
    parent: ptr Scope
    lifter: ref LiftingCtx
    isTopLevel: bool

proc createNestedScope(kind: ScopeKind; parent: var Scope; info: PackedLineInfo; label = NoLabel): Scope =
  Scope(p: parent.p, label: label, thisModule: parent.thisModule, procStart: parent.procStart,
    kind: kind, destroyOps: @[], info: info, parent: addr(parent), lifter: parent.lifter,
    isTopLevel: false)

proc createEntryScope(p: Program; thisModule: ModuleId; lifter: ref LiftingCtx;
                      procStart: Cursor; info: PackedLineInfo): Scope =
  Scope(p: p, label: NoLabel, thisModule: thisModule, procStart: procStart,
    kind: Other, destroyOps: @[], info: info, parent: nil, lifter: lifter,
    isTopLevel: true)

proc callDestroy(c: var Scope; dest: var TokenBuf; destroyProc: SymId; arg: SymId) =
  copyIntoKind dest, Call, c.info:
    copyIntoSymUse dest, destroyProc, c.info
    copyIntoSymUse dest, arg, c.info

proc leaveScope(c: var Scope; dest: var TokenBuf) =
  for i in countdown(c.destroyOps.high, 0):
    callDestroy c, dest, c.destroyOps[i].destroyProc, c.destroyOps[i].arg

proc leaveNamedBlock(c: var Scope; dest: var TokenBuf; label: SymId) =
  #[ Consider:

  var x = f()
  block:
    break # do we want to destroy x here? No.

  ]#
  var it = addr(c)
  while it != nil and it.label != label:
    leaveScope(it[], dest)
    it = it.parent
  if it != nil and it.label == label:
    leaveScope(it[], dest)
  else:
    assert false, "do not know which block to leave"

proc leaveAnonBlock(c: var Scope; dest: var TokenBuf) =
  var it = addr(c)
  while it != nil and it.kind != WhileOrBlock:
    leaveScope(it[], dest)
    it = it.parent
  if it != nil and it.kind == WhileOrBlock:
    leaveScope(it[], dest)
  else:
    assert false, "do not know which block to leave"

proc trBreak(c: var Scope; dest: var TokenBuf; n: Cursor) =
  let lab = n.firstSon
  if lab.kind == SymUse:
    leaveNamedBlock(c, dest, lab.symId)
  else:
    leaveAnonBlock(c, dest)
  copyTree dest, tree, n

proc trReturn(c: var Scope; dest: var TokenBuf; n: Cursor) =
  var it = addr(c)
  while it != nil:
    leaveScope(it[], dest)
    it = it.parent
  copyTree dest, tree, n

when not defined(nimony):
  proc tr(c: var Scope; dest: var TokenBuf; n: Cursor)

proc trLocal(c: var Scope; dest: var TokenBuf; n: Cursor) =
  let r = asLocal(tree, n)
  copyIntoKind(dest, n.kind, n.info):
    copyTree(dest, tree, r.name)
    copyTree(dest, tree, r.ex)
    copyTree(dest, tree, r.pragmas)
    copyTree(dest, tree, r.typ)
    let localType = getType(c.p, tree, r.name)
    let destructor = getDestructor(c.lifter[], localType, n.info)
    let s = r.name.symId
    let sk = c.p[tree.m].syms[s].kind
    if destructor.s != SymId(-1) and sk != CursorDecl:
      if not c.isTopLevel and sk != ResultDecl:
        # XXX If we don't free global variables let's at least free temporaries!
        c.destroyOps.add DestructorOp(destroyProc: destructor, arg: s)
    tr c, dest, tree, r.value

proc trScope(c: var Scope; dest: var TokenBuf; body: Cursor) =
  copyIntoKind dest, StmtList, body.info:
    if body.kind == StmtList:
      for ch in sonsReadOnly(tree, body):
        tr c, dest, tree, ch
    else:
      tr c, dest, tree, body
    leaveScope(c, dest)

proc registerSinkParameters(c: var Scope; params: Cursor) =
  for ch in sonsFrom1(tree, params):
    let r = asLocal(tree, ch)
    if r.typ.kind == SinkTy:
      let destructor = getDestructor(c.lifter[], FullTypeId(t: r.typ.firstSon, m: tree.id), ch.info)
      if destructor.s != SymId(-1):
        c.destroyOps.add DestructorOp(destroyProc: destructor, arg: r.name.symId)

proc trProcDecl(c: var Scope; dest: var TokenBuf; n: Cursor) =
  let r = asRoutine(tree, n)
  var c2 = createEntryScope(c.p, c.thisModule, c.lifter, r.body, r.body.info)
  c2.isTopLevel = false
  copyInto(dest, n):
    copyTree dest, tree, r.name
    copyTree dest, tree, r.ex
    copyTree dest, tree, r.pat
    copyTree dest, tree, r.generics
    copyTree dest, tree, r.params
    copyTree dest, tree, r.pragmas
    copyTree dest, tree, r.exc
    if r.body.kind == StmtList and r.generics.kind != GenericParams and
        not hasBuiltinPragma(c.p, tree, r.pragmas, "nodestroy"):
      registerSinkParameters(c2, tree, r.params)
      trScope c2, dest, tree, r.body
    else:
      copyTree dest, tree, r.body

proc trNestedScope(c: var Scope; dest: var TokenBuf; body: Cursor; kind = Other) =
  var bodyScope = createNestedScope(kind, c, body.info)
  trScope bodyScope, dest, tree, body

proc trWhile(c: var Scope; dest: var TokenBuf; n: Cursor) =
  #[ while prop(createsObj())
      was turned into `while (let tmp = createsObj(); prop(tmp))` by  `duplifier.nim`
      already and `to_stmts` did turn it into:

      while true:
        let tmp = createsObj()
        if not prop(tmp): break

      For these reasons we don't have to do anything special with `cond`. The same
      reasoning applies to `if` and `case` statements.
  ]#
  let (cond, body) = sons2(tree, n)
  copyInto(dest, n):
    tr c, dest, tree, cond
    trNestedScope c, dest, tree, body, WhileOrBlock

proc trBlock(c: var Scope; dest: var TokenBuf; n: Cursor) =
  let (label, body) = sons2(tree, n)
  let labelId = if label.kind == SymDef: label.symId else: AnonBlock
  var bodyScope = createNestedScope(WhileOrBlock, c, body.info, labelId)
  copyInto(dest, n):
    copyTree dest, tree, label
    trScope bodyScope, dest, tree, body

proc trIf(c: var Scope; dest: var TokenBuf; n: Cursor) =
  for ch in sons(dest, tree, n):
    case ch.kind
    of ElifBranch:
      let (cond, action) = sons2(tree, ch)
      copyInto(dest, ch):
        tr c, dest, tree, cond
        trNestedScope c, dest, tree, action
    of ElseBranch:
      copyInto(dest, ch):
        trNestedScope c, dest, tree, ch.firstSon
    else:
      copyTree dest, tree, ch

proc trCase(c: var Scope; dest: var TokenBuf; n: Cursor) =
  copyInto(dest, n):
    tr c, dest, tree, n.firstSon
    for ch in sonsFrom1(tree, n):
      case ch.kind
      of OfBranch:
        copyInto(dest, ch):
          let (first, action) = sons2(tree, ch)
          copyTree dest, tree, first
          trNestedScope c, dest, tree, action
      of ElseBranch:
        copyInto(dest, ch):
          trNestedScope c, dest, tree, ch.firstSon
      else:
        copyTree dest, tree, ch

proc tr(c: var Scope; dest: var TokenBuf; n: Cursor) =
  case n.kind
  of ReturnStmt:
    trReturn(c, dest, tree, n)
  of BreakStmt:
    trBreak(c, dest, tree, n)
  of IfStmt:
    trIf c, dest, tree, n
  of CaseStmt:
    trCase c, dest, tree, n
  of BlockStmt:
    trBlock c, dest, tree, n
  #of Asgn, FirstAsgn:
  #  trAsgn c, dest, tree, n
  of VarDecl, LetDecl, ConstDecl, ResultDecl:
    trLocal c, dest, tree, n
  of WhileStmt:
    trWhile c, dest, tree, n
  of DeclarativeNodes, Atoms, Pragmas, TemplateDecl, IteratorDecl,
     UsingStmt, CommentStmt, BindStmt, MixinStmt, ContinueStmt:
    copyTree dest, tree, n
  of ProcDecl, FuncDecl, MacroDecl, MethodDecl, ConverterDecl:
    trProcDecl c, dest, tree, n
  else:
    for ch in sons(dest, tree, n):
      tr(c, dest, tree, ch)

proc injectDestructors*(p: Program; t: TreeId; lifter: ref LiftingCtx): TreeId =
  let thisModule = p[t].m
  let info = p[t][StartPos].info

  var c = createEntryScope(p, thisModule, lifter, StartPos, info)
  result = createTree(p, c.thisModule)
  p[result].flags.incl dontTouch

  tr(c, p[result], p[t], StartPos)
  leaveScope c, p[result]
  genMissingHooks lifter[], p[result]
  patch p[result], PatchPos(0)
  p[result].flags.excl dontTouch
