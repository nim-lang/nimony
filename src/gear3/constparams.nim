#
#
#           Gear3 Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

##[

Implements "pass by const" by introducing more hidden pointers.
We do this right before inlining and before codegen as it interacts with the
codegen's `maybeByConstRef` logic.

]##

import std / sets

include nifprelude

type
  Context = object
    constRefParams: HashSet[SymId]
    dest: TokenBuf
    ptrSize: int

when not defined(nimony):
  proc tr(c: var Context; n: var Cursor)

proc rememberConstRefParams(c: var Context; params: Cursor) =
  var n = params
  while n.kind != ParRi:
    let p = takeLocal(n, SkipFinalParRi)
    if p.name.kind == SymbolDef and passByConstRef(c.p, p.typ):
      c.constRefParams.incl p.name.symId

proc trProcDecl(c: var Context; n: var Cursor) =
  let r = asRoutine(tree, n)
  var c2 = Context(p: c.p)
  copyInto(dest, n):
    copyTree dest, r.name
    copyTree dest, r.ex
    copyTree dest, r.pat
    copyTree dest, r.generics
    copyTree dest, r.params
    copyTree dest, r.pragmas
    copyTree dest, r.exc
    if r.body.kind == StmtList and r.generics.kind != GenericParams:
      rememberConstRefParams c2, r.params
      tr c2, dest, r.body
    else:
      copyTree dest, r.body

proc trConstRef(c: var Context; n: var Cursor) =
  let info = n.info
  assert n.kind notin {AddrExpr, HiddenAddr}
  if constructsValue(c.p, n):
    # We cannot take the address of a literal so we have to copy it to a
    # temporary first:
    let argType = getType(c.p, n)
    copyIntoKind dest, StmtListExpr, info:
      copyIntoKind dest, PtrTy, info:
        copyTree dest, c.p[argType.m], argType.t
      copyIntoKind dest, StmtList, info:
        let symId = declareSym(c.p[dest.m], VarDecl, c.p[dest.m].strings.getOrIncl("constRefTemp"))
        copyIntoKind dest, VarDecl, info:
          addSymDef dest, symId, info
          dest.addEmpty2 info # export marker, pragma
          copyTree dest, c.p[argType.m], argType.t
          # value:
          tr c, dest, n
      copyIntoKind dest, HiddenAddr, info:
        dest.addSymUse symId, info
  else:
    copyIntoKind dest, HiddenAddr, info:
      tr c, dest, n

proc trCallArgs(c: var Context; n: var Cursor) =
  var fnType = getType(c.p, n.firstSon)
  fnType = skipGenericInsts(c.p, fnType, {SkipNilTy, SkipObjectInstantiations})
  assert c.p[fnType].kind in ProcTyNodes
  var paramIter = initSonsIter(c.p, ithSon(c.p, fnType, routineParamsPos))
  var i = 0
  for ch in sons(dest, n):
    var wantConstRefT = false
    if hasCurrent(paramIter):
      if i > 0:
        var paramType = ithSon(c.p, paramIter.current, localTypePos)
        wantConstRefT = passByConstRef(c.p, paramType, c.p[paramIter.current.m],
                            ithSon(c.p, paramIter.current, localPragmasPos).t)
      next paramIter, c.p
    if wantConstRefT:
      trConstRef c, dest, ch
    else:
      tr c, dest, ch
    inc i

proc trObjConstr(c: var Context; n: var Cursor) =
  let t = n.firstSon
  let argType = FullTypeId(m: tree.id, t: t)
  let isRef = isRefType(c.p, argType)
  if isRef:
    let info = n.info
    copyIntoKind dest, StmtListExpr, info:
      copyTree dest, c.p[argType.m], argType.t
      copyIntoKind dest, StmtList, info:
        let symId = declareSym(c.p[dest.m], VarDecl, c.p[dest.m].strings.getOrIncl("refConstrTemp"))
        copyIntoKind dest, VarDecl, info:
          addSymDef dest, symId, info
          dest.addEmpty2 info # export marker, pragma
          copyTree dest, c.p[argType.m], argType.t
          copyIntoKind dest, NewConstr, info:
            copyTree dest, c.p[argType.m], argType.t
        copyIntoKind dest, Asgn, info:
          copyIntoKind dest, DerefExpr, info:
            addSymUse dest, symId, info
          for ch in sons(dest, n):
            tr c, dest, ch
      dest.addSymUse symId, info
  else:
    for ch in sons(dest, n):
      tr c, dest, ch

proc tr(c: var Context; n: var Cursor) =
  case n.kind
  of AtomsExceptSymUse:
    copyTree dest, n
  of Call:
    trCallArgs c, dest, n
  of SymUse:
    if c.constRefParams.contains(n.symId):
      copyIntoKind dest, HiddenDeref, n.info:
        copyTree dest, n
    else:
      copyTree dest, n
  of DeclarativeNodes, Pragmas, TemplateDecl, IteratorDecl,
     UsingStmt, CommentStmt, BindStmt, MixinStmt, ContinueStmt, BreakStmt:
    copyTree dest, n
  of ProcDecl, FuncDecl, MacroDecl, MethodDecl, ConverterDecl:
    trProcDecl c, dest, n
  of ObjConstr:
    # For `MyRef(a: a)` the C codegen needs to generate `x = malloc(...); *x = MyRef()`
    trObjConstr c, dest, n
  else:
    for ch in sons(dest, n):
      tr c, dest, ch

proc injectConstParamDerefs*(n: Cursor; ptrSize: int): TokenBuf =
  var c = Context(dest: createTokenBuf(300), ptrSize: ptrSize)
  result = createTree(p, thisModule)
  tr(c, p[result], p[t], StartPos)
