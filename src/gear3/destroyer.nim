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

import std / assertions
include nifprelude
import nifindexes, symparser, treemangler
import ".." / nimony / [nimony_model, programs, typenav]
import lifter

const
  NoLabel = SymId(0)

type
  ScopeKind = enum
    Other
    WhileOrBlock
  DestructorOp = object
    destroyProc: SymId
    arg: SymId

  Scope = object
    label: SymId
    kind: ScopeKind
    destroyOps: seq[DestructorOp]
    info: PackedLineInfo
    parent: ptr Scope
    isTopLevel: bool

  Context = object
    currentScope: Scope
    #procStart: Cursor
    anonBlock: SymId
    dest: TokenBuf
    lifter: ref LiftingCtx

proc createNestedScope(kind: ScopeKind; parent: var Scope; info: PackedLineInfo; label = NoLabel): Scope =
  Scope(label: label,
    kind: kind, destroyOps: @[], info: info, parent: addr(parent),
    isTopLevel: false)

proc createEntryScope(info: PackedLineInfo): Scope =
  Scope(label: NoLabel,
    kind: Other, destroyOps: @[], info: info, parent: nil,
    isTopLevel: true)

proc callDestroy(c: var Context; destroyProc: SymId; arg: SymId) =
  let info = c.currentScope.info
  copyIntoKind c.dest, CallS, info:
    copyIntoSymUse c.dest, destroyProc, info
    copyIntoSymUse c.dest, arg, info

proc leaveScope(c: var Context; s: var Scope) =
  for i in countdown(s.destroyOps.high, 0):
    callDestroy c, s.destroyOps[i].destroyProc, s.destroyOps[i].arg

proc leaveNamedBlock(c: var Context; label: SymId) =
  #[ Consider:

  var x = f()
  block:
    break # do we want to destroy x here? No.

  ]#
  var it = addr(c.currentScope)
  while it != nil and it.label != label:
    leaveScope(c, it[])
    it = it.parent
  if it != nil and it.label == label:
    leaveScope(c, it[])
  else:
    raiseAssert "do not know which block to leave"

proc leaveAnonBlock(c: var Context) =
  var it = addr(c.currentScope)
  while it != nil and it.kind != WhileOrBlock:
    leaveScope(c, it[])
    it = it.parent
  if it != nil and it.kind == WhileOrBlock:
    leaveScope(c, it[])
  else:
    raiseAssert "do not know which block to leave"

proc trBreak(c: var Context; n: var Cursor) =
  let lab = n.firstSon
  if lab.kind == Symbol:
    leaveNamedBlock(c, lab.symId)
  else:
    leaveAnonBlock(c)
  takeTree c.dest, n

proc trReturn(c: var Context; n: var Cursor) =
  var it = addr(c.currentScope)
  while it != nil:
    leaveScope(c, it[])
    it = it.parent
  takeTree c.dest, n

when not defined(nimony):
  proc tr(c: var Context; n: var Cursor)

proc trLocal(c: var Context; n: Cursor) =
  let r = asLocal(tree, n)
  copyIntoKind(dest, n.kind, n.info):
    copyTree(dest, r.name)
    copyTree(dest, r.ex)
    copyTree(dest, r.pragmas)
    copyTree(dest, r.typ)
    let localType = getType(c.p, r.name)
    let destructor = getDestructor(c.lifter[], localType, n.info)
    let s = r.name.symId
    let sk = c.p[tree.m].syms[s].kind
    if destructor.s != SymId(-1) and sk != CursorDecl:
      if not c.isTopLevel and sk != ResultDecl:
        # XXX If we don't free global variables let's at least free temporaries!
        c.destroyOps.add DestructorOp(destroyProc: destructor, arg: s)
    tr c, dest, r.value

proc trScope(c: var Context; body: Cursor) =
  copyIntoKind dest, StmtList, body.info:
    if body.kind == StmtList:
      for ch in sonsReadOnly(tree, body):
        tr c, dest, ch
    else:
      tr c, dest, body
    leaveScope(c, dest)

proc registerSinkParameters(c: var Scope; params: Cursor) =
  for ch in sonsFrom1(tree, params):
    let r = asLocal(tree, ch)
    if r.typ.kind == SinkTy:
      let destructor = getDestructor(c.lifter[], FullTypeId(t: r.typ.firstSon, m: tree.id), ch.info)
      if destructor.s != SymId(-1):
        c.destroyOps.add DestructorOp(destroyProc: destructor, arg: r.name.symId)

proc trProcDecl(c: var Context; n: Cursor) =
  let r = asRoutine(tree, n)
  var c2 = createEntryScope(c.p, c.thisModule, c.lifter, r.body, r.body.info)
  c2.isTopLevel = false
  copyInto(dest, n):
    copyTree dest, r.name
    copyTree dest, r.ex
    copyTree dest, r.pat
    copyTree dest, r.generics
    copyTree dest, r.params
    copyTree dest, r.pragmas
    copyTree dest, r.exc
    if r.body.kind == StmtList and r.generics.kind != GenericParams and
        not hasBuiltinPragma(c.p, r.pragmas, "nodestroy"):
      registerSinkParameters(c2, r.params)
      trScope c2, dest, r.body
    else:
      copyTree dest, r.body

proc trNestedScope(c: var Context; body: Cursor; kind = Other) =
  var bodyScope = createNestedScope(kind, c, body.info)
  trScope bodyScope, dest, body

proc trWhile(c: var Context; n: Cursor) =
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
    tr c, dest, cond
    trNestedScope c, dest, body, WhileOrBlock

proc trBlock(c: var Context; n: Cursor) =
  let (label, body) = sons2(tree, n)
  let labelId = if label.kind == SymDef: label.symId else: AnonBlock
  var bodyScope = createNestedScope(WhileOrBlock, c, body.info, labelId)
  copyInto(dest, n):
    copyTree dest, label
    trScope bodyScope, dest, body

proc trIf(c: var Context; n: Cursor) =
  for ch in sons(dest, n):
    case ch.kind
    of ElifBranch:
      let (cond, action) = sons2(tree, ch)
      copyInto(dest, ch):
        tr c, dest, cond
        trNestedScope c, dest, action
    of ElseBranch:
      copyInto(dest, ch):
        trNestedScope c, dest, ch.firstSon
    else:
      copyTree dest, ch

proc trCase(c: var Context; n: Cursor) =
  copyInto(dest, n):
    tr c, dest, n.firstSon
    for ch in sonsFrom1(tree, n):
      case ch.kind
      of OfBranch:
        copyInto(dest, ch):
          let (first, action) = sons2(tree, ch)
          copyTree dest, first
          trNestedScope c, dest, action
      of ElseBranch:
        copyInto(dest, ch):
          trNestedScope c, dest, ch.firstSon
      else:
        copyTree dest, ch

proc tr(c: var Context; n: var Cursor) =
  case n.kind
  of ReturnStmt:
    trReturn(c, dest, n)
  of BreakStmt:
    trBreak(c, dest, n)
  of IfStmt:
    trIf c, dest, n
  of CaseStmt:
    trCase c, dest, n
  of BlockStmt:
    trBlock c, dest, n
  #of Asgn, FirstAsgn:
  #  trAsgn c, dest, n
  of VarDecl, LetDecl, ConstDecl, ResultDecl:
    trLocal c, dest, n
  of WhileStmt:
    trWhile c, dest, n
  of DeclarativeNodes, Atoms, Pragmas, TemplateDecl, IteratorDecl,
     UsingStmt, CommentStmt, BindStmt, MixinStmt, ContinueStmt:
    copyTree dest, n
  of ProcDecl, FuncDecl, MacroDecl, MethodDecl, ConverterDecl:
    trProcDecl c, dest, n
  else:
    for ch in sons(dest, n):
      tr(c, dest, ch)

proc injectDestructors*(n: Cursor; lifter: ref LiftingCtx): TokenBuf =
  var c = Context(currentScope: createEntryScope(p, thisModule, lifter, StartPos, info),
    anonBlock: pool.syms.getOrIncl("`anonblock.0"),
    dest: createTokenBuf(400))
  var n = n
  tr(c, n)
  leaveScope c, p[result]
  genMissingHooks lifter[], p[result]
  result = ensureMove c.dest
