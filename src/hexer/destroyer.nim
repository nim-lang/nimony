#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
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

import std / [assertions, tables]
include nifprelude
import nifindexes, symparser, treemangler
import ".." / nimony / [nimony_model, programs, typenav, decls]
import lifter

const
  NoLabel = SymId(0)

type
  ScopeKind = enum
    Other
    OtherPreventFinally
    WhileOrBlock
  DestructorOp = object
    destroyProc: SymId
    arg: SymId

  Scope = object
    label: SymId
    kind: ScopeKind
    isTopLevel: bool
    destroyOps: seq[DestructorOp]
    info: PackedLineInfo
    finallySection: Cursor
    parent: ptr Scope

  Context = object
    currentScope: Scope
    #procStart: Cursor
    anonBlock: SymId
    dest: TokenBuf
    lifter: ref LiftingCtx

proc createNestedScope(kind: ScopeKind; parent: var Scope; info: PackedLineInfo;
                       label = NoLabel; fin = default(Cursor)): Scope =
  Scope(label: label,
    kind: kind, destroyOps: @[], info: info, parent: addr(parent),
    isTopLevel: false, finallySection: fin)

proc createEntryScope(info: PackedLineInfo): Scope =
  Scope(label: NoLabel,
    kind: Other, destroyOps: @[], info: info, parent: nil,
    isTopLevel: true)

proc callDestroy(c: var Context; destroyProc: SymId; arg: SymId) =
  let info = c.currentScope.info
  copyIntoKind c.dest, CallS, info:
    copyIntoSymUse c.dest, destroyProc, info
    if isMutFirstParam(destroyProc):
      copyIntoKind c.dest, HaddrX, info:
        copyIntoSymUse c.dest, arg, info
    else:
      copyIntoSymUse c.dest, arg, info

when not defined(nimony):
  proc tr(c: var Context; n: var Cursor)

proc createFreshVars(c: var Context; n: Cursor): TokenBuf =
  var n = n
  var nested = 0
  var newVars = initTable[SymId, SymId]()
  var idgen = 0
  result = createTokenBuf(30)
  while true:
    case n.kind
    of Symbol:
      let repl = newVars.getOrDefault(n.symId, n.symId)
      result.add symToken(repl, n.info)
      inc n
    of ParRi:
      result.add n
      dec nested
      if nested == 0: break
      inc n
    of ParLe:
      result.add n
      let isLocalDecl = n.stmtKind in {VarS, LetS, CursorS}
      inc n
      inc nested
      if isLocalDecl:
        if n.kind == SymbolDef:
          let repl = pool.syms.getOrIncl("`ffv." & $idgen)
          newVars[n.symId] = repl
          result.add symdefToken(repl, n.info)
          inc idgen
          inc n
    of UIntLit, StringLit, IntLit, FloatLit, CharLit, SymbolDef, UnknownToken, EofToken, DotToken, Ident:
      result.add n
      inc n

proc leaveScope(c: var Context; s: var Scope; kind = Other) =
  if kind != OtherPreventFinally and s.finallySection != default(Cursor):
    var freshVars = createFreshVars(c, s.finallySection)
    var n = beginRead(freshVars)
    tr c, n
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
    bug "do not know which block to leave"

proc leaveAnonBlock(c: var Context) =
  var it = addr(c.currentScope)
  while it != nil and it.kind != WhileOrBlock:
    leaveScope(c, it[])
    it = it.parent
  if it != nil and it.kind == WhileOrBlock:
    leaveScope(c, it[])
  else:
    bug "do not know which block to leave"

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

proc trRaise(c: var Context; n: var Cursor) =
  #[
  Consider:

    try:
      s1
      raise
    except:
      echo "e"
    finally:
      echo "fin"

    We do not want to duplicate the finally here since
    it will run after the `except` block no matter what.
  ]#
  var it = addr(c.currentScope)
  while it != nil:
    leaveScope(c, it[], it.kind)
    it = it.parent
  takeTree c.dest, n

proc trLocal(c: var Context; n: var Cursor) =
  let info = n.info
  c.dest.add n
  var r = takeLocal(n, SkipFinalParRi)
  copyTree c.dest, r.name
  copyTree c.dest, r.exported
  copyTree c.dest, r.pragmas
  copyTree c.dest, r.typ

  tr c, r.val
  c.dest.addParRi()

  let destructor = getDestructor(c.lifter[], r.typ, info)
  if destructor != NoSymId and r.kind notin {CursorY, ResultY, GvarY, TvarY, GletY, TletY, ConstY}:
    c.currentScope.destroyOps.add DestructorOp(destroyProc: destructor, arg: r.name.symId)

proc trScope(c: var Context; body: var Cursor; kind = Other) =
  copyIntoKind c.dest, StmtsS, body.info:
    if body.stmtKind == StmtsS:
      inc body
      while body.kind != ParRi:
        tr c, body
      inc body
    else:
      tr c, body
    leaveScope(c, c.currentScope, kind)

proc registerSinkParameters(c: var Context; params: Cursor) =
  var p = params
  inc p
  while p.kind != ParRi:
    let r = takeLocal(p, SkipFinalParRi)
    if r.typ.typeKind == SinkT:
      let destructor = getDestructor(c.lifter[], r.typ.firstSon, p.info)
      if destructor != NoSymId:
        c.currentScope.destroyOps.add DestructorOp(destroyProc: destructor, arg: r.name.symId)

proc trProcDecl(c: var Context; n: var Cursor) =
  c.dest.add n
  var r = takeRoutine(n, SkipFinalParRi)
  copyTree c.dest, r.name
  copyTree c.dest, r.exported
  copyTree c.dest, r.pattern
  copyTree c.dest, r.typevars
  copyTree c.dest, r.params
  copyTree c.dest, r.retType
  copyTree c.dest, r.pragmas
  copyTree c.dest, r.effects
  if r.body.stmtKind == StmtsS and not isGeneric(r):
    if hasPragma(r.pragmas, NodestroyP):
      copyTree c.dest, r.body
    else:
      var s2 = createEntryScope(r.body.info)
      s2.isTopLevel = false
      swap c.currentScope, s2
      registerSinkParameters(c, r.params)
      trScope c, r.body
      swap c.currentScope, s2
  else:
    copyTree c.dest, r.body
  c.dest.addParRi()

proc trNestedScope(c: var Context; body: var Cursor; kind = Other; fin = default(Cursor)) =
  var oldScope = move c.currentScope
  c.currentScope = createNestedScope(kind, oldScope, body.info, NoLabel, fin)
  trScope c, body, kind
  swap c.currentScope, oldScope

proc trWhile(c: var Context; n: var Cursor) =
  #[ while prop(createsObj())
      was turned into `while (let tmp = createsObj(); prop(tmp))` by  `duplifier.nim`
      already and `to_stmts` did turn it into:

      while true:
        let tmp = createsObj()
        if not prop(tmp): break

      For these reasons we don't have to do anything special with `cond`. The same
      reasoning applies to `if` and `case` statements.
  ]#
  copyInto(c.dest, n):
    tr c, n
    trNestedScope c, n, WhileOrBlock

proc trBlock(c: var Context; n: var Cursor) =
  let label = n.firstSon
  let labelId = if label.kind == SymbolDef: label.symId else: c.anonBlock
  var oldScope = move c.currentScope
  c.currentScope = createNestedScope(WhileOrBlock, oldScope, n.info, labelId)
  copyInto(c.dest, n):
    takeTree c.dest, n
    trScope c, n
    swap c.currentScope, oldScope

proc trIf(c: var Context; n: var Cursor) =
  copyInto(c.dest, n):
    while n.kind != ParRi:
      case n.substructureKind
      of ElifU:
        copyInto(c.dest, n):
          tr c, n
          trNestedScope c, n
      of ElseU:
        copyInto(c.dest, n):
          trNestedScope c, n
      else:
        takeTree c.dest, n

proc trCase(c: var Context; n: var Cursor) =
  copyInto(c.dest, n):
    tr c, n
    while n.kind != ParRi:
      case n.substructureKind
      of OfU:
        copyInto(c.dest, n):
          takeTree c.dest, n
          trNestedScope c, n
      of ElseU:
        copyInto(c.dest, n):
          trNestedScope c, n
      else:
        takeTree c.dest, n

proc trTry(c: var Context; n: var Cursor) =
  var nn = n
  inc nn
  skip nn # try statements
  var hasExcept = false
  while nn.substructureKind == ExceptU:
    hasExcept = true
    skip nn
  copyInto(c.dest, n):
    let fin = if nn.substructureKind == FinU: nn.firstSon else: default(Cursor)
    trNestedScope c, n, OtherPreventFinally, fin
    while n.substructureKind == ExceptU:
      copyInto(c.dest, n):
        takeTree c.dest, n # `E as e`
        trNestedScope c, n, Other, fin
    if n.substructureKind == FinU:
      copyInto(c.dest, n):
        trNestedScope c, n

proc tr(c: var Context; n: var Cursor) =
  if isAtom(n) or isDeclarative(n):
    takeTree c.dest, n
  else:
    case n.stmtKind
    of RetS:
      trReturn(c, n)
    of RaiseS:
      trRaise(c, n)
    of BreakS:
      trBreak(c, n)
    of IfS:
      trIf c, n
    of CaseS:
      trCase c, n
    of BlockS:
      trBlock c, n
    of LocalDecls:
      trLocal c, n
    of WhileS:
      trWhile c, n
    of TryS:
      trTry c, n
    of ProcS, FuncS, MacroS, MethodS, ConverterS:
      trProcDecl c, n
    else:
      if n.kind == ParLe:
        c.dest.add n
        inc n
        while n.kind != ParRi:
          tr(c, n)
        takeParRi(c.dest, n)
      else:
        c.dest.add n
        inc n

proc injectDestructors*(n: Cursor; lifter: ref LiftingCtx): TokenBuf =
  var c = Context(lifter: lifter, currentScope: createEntryScope(n.info),
    anonBlock: pool.syms.getOrIncl("`anonblock.0"),
    dest: createTokenBuf(400))
  var n = n
  assert n.stmtKind == StmtsS
  c.dest.add n
  inc n
  while n.kind != ParRi:
    tr(c, n)

  leaveScope c, c.currentScope
  takeParRi(c.dest, n)
  genMissingHooks lifter[]
  result = ensureMove c.dest
