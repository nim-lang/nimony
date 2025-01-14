#
#
#           Gear3 Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Inliner. We inline function calls that are annotated with `.inline`.
## Inlining requires:
## - copy the body of the function to the caller into a new block `B`.
## - replace the parameter by a local variable that is initialized with the
##   passed value.
## - replace `result` by the assigned destination of `x = toInline(args)` if it exists,
##   otherwise by a temporary variable.
## - replace `return` by `break B`.
## - copy local variables from the body into fresh ones.

import std / [tables, assertions]
include nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof]
import duplifier

type
  TargetKind = enum
    TargetIsNone, TargetIsSym, TargetIsNode
  Target = object
    kind: TargetKind
    sym: SymId
    pos: Cursor

  Context* = object
    thisRoutine: SymId
    thisModuleSuffix: string
    globals, locals: Table[string, int]
    typeCache: TypeCache
    ptrSize: int

  InlineContext* = object
    returnLabel: SymId
    resultSym: SymId
    newVars: Table[SymId, SymId]
    target: Target
    c: ptr Context

proc createInliner(thisModuleSuffix: string; ptrSize: int): Context =
  result = Context(thisRoutine: NoSymId, thisModuleSuffix: thisModuleSuffix,
    typeCache: createTypeCache(), ptrSize: ptrSize)

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.add n
  var r = takeRoutine(n, SkipExclBody)
  let oldThisRoutine = c.thisRoutine
  c.thisRoutine = r.name.symId
  copyTree dest, r.name
  copyTree dest, r.exported
  copyTree dest, r.pattern
  copyTree dest, r.typevars
  copyTree dest, r.params
  copyTree dest, r.pragmas
  copyTree dest, r.effects
  skip n # effects
  if n.stmtKind == StmtsS and not isGeneric(r):
    tr c, dest, n
  else:
    takeTree dest, n
  dest.wantParRi(n)
  c.thisRoutine = oldThisRoutine

proc shouldInlineRoutine(pragmas: Cursor): bool =
  hasBuiltinPragma(pragmas, Inline)

proc shouldInlineCall(c: var Context; n: Cursor; routine: var Routine): bool =
  result = false
  if n.exprKind in CallKinds and c.thisRoutine != NoSymId:
    let callee = n.firstSon
    if callee.kind == Symbol:
      let s = tryLoadSym(callee.symId)
      if s.status == LacksNothing:
        # who says we cannot inline recursions?
        routine = asRoutine(s.decl)
        if routine.kind in {ProcY, FuncY, ConverterY}:
          if shouldInlineRoutine(routine.pragmas):
            result = true

proc makeGlobalSym*(c: var Context; result: var string) =
  var counter = addr c.globals.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]
  result.add '.'
  result.add c.thisModuleSuffix

proc makeLocalSym*(c: var Context; result: var string) =
  var counter = addr c.locals.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]

proc newSymId(c: var Context; s: SymId): SymId =
  var isGlobal = false
  var name = "`" & extractBasename(pool.syms[s], isGlobal)
  if isGlobal:
    c.makeGlobalSym(name)
  else:
    c.makeLocalSym(name)
  result = pool.syms.getOrIncl(name)

proc inlineRoutineBody(c: var InlineContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  case n.kind
  of SymbolDef:
    let id = n.symId
    let freshId = newSymId(c.c[], id)

    c.newVars[id] = freshId
    dest.addSymDef freshId, info
    inc n
  of Symbol:
    if c.resultSym == n.symId:
      case c.target.kind
      of TargetIsNone:
        let toReplace = c.newVars.getOrDefault(n.symId, NoSymId)
        assert toReplace != NoSymId, "cannot find result declaration"
        dest.addSymUse toReplace, info
      of TargetIsSym:
        dest.addSymUse c.target.sym, info
      of TargetIsNode:
        copyTree dest, c.target.pos
    else:
      let toReplace = c.newVars.getOrDefault(n.symId, NoSymId)
      if toReplace != NoSymId:
        dest.addSymUse toReplace, info
      else:
        dest.add n
    inc n
  of Ident, IntLit, UIntLit, FloatLit, CharLit, StringLit, UnknownToken, DotToken, EofToken:
    dest.add n
    inc n
  of ParRi:
    raiseAssert "unhandled ')' in inliner.nim"
  of ParLe:
    case n.stmtKind
    of RetS:
      let retVal = n.firstSon
      if retVal.kind != DotToken and not (retVal.kind == Symbol and c.resultSym == retVal.symId):
        # generate assignment: `dest = result`
        copyIntoKind dest, AsgnS, info:
          case c.target.kind
          of TargetIsNone:
            assert c.resultSym != NoSymId
            let toReplace = c.newVars.getOrDefault(c.resultSym, NoSymId)
            assert toReplace != NoSymId, "cannot find result declaration"
            dest.addSymUse toReplace, info
          of TargetIsSym:
            assert c.target.sym != NoSymId
            dest.addSymUse c.target.sym, info
          of TargetIsNode:
            copyTree dest, c.target.pos
          inc n
          inlineRoutineBody c, dest, n

      copyIntoKind dest, BreakS, info:
        dest.addSymUse c.returnLabel, info
      assert n.kind == ParRi
      inc n
    of ResultS:
      if c.target.kind == TargetIsNone:
        # we need the result declaration. But it is inlined, so
        # it is not a `ResultDecl`!
        copyIntoKind dest, VarS, info:
          while n.kind != ParRi:
            inlineRoutineBody(c, dest, n)
        dest.addParRi()
        assert n.kind == ParRi
        inc n
      else:
        # discard the result declaration!
        discard
      c.resultSym = n.firstSon.symId
    else:
      if isDeclarative(n):
        takeTree dest, n
      else:
        copyInto dest, n:
          while n.kind != ParRi:
            inlineRoutineBody(c, dest, n)

proc mapParamToLocal(c: var InlineContext; dest: var TokenBuf; args: var Cursor; params: var Cursor) =
  # assign parameters: This also ensures that side effects are executed,
  # consider: `inlineCall effect(x)` where `inlineCall` does not even use
  # its first parameter!
  assert params.kind != DotToken
  assert params.kind != ParRi
  let p = params
  let r = takeLocal(params, SkipFinalParRi)
  if r.typ.typeKind == VarargsT: params = p
  if isCompileTimeType(r.typ):
    skip args # ignore compile-time parameters for inlining purposes
  else:
    let info = args.info
    copyIntoKind dest, LetS, info:
      let id = r.name.symId
      let freshId = newSymId(c.c[], id)
      c.newVars[id] = freshId
      dest.addSymDef freshId, info
      dest.addDotToken() # not exported
      dest.addDotToken() # no pragmas
      if typeIsBig(r.typ, c.c.ptrSize) and not constructsValue(args):
        copyIntoKind dest, PtrT, info:
          dest.copyTree(r.typ)
        copyIntoKind dest, AddrX, info:
          tr c.c[], dest, args
      else:
        dest.copyTree(r.typ)
        tr c.c[], dest, args

proc doInline(outer: var Context; dest: var TokenBuf; procCall: var Cursor; routine: Routine;
              target: Target) =
  assert procCall.exprKind in CallKinds
  var c = InlineContext(target: target, resultSym: NoSymId, c: addr outer)

  let info = procCall.info

  var isStmtListExpr = false
  if target.kind == TargetIsNone:
    let t = getType(outer.typeCache, procCall)
    if t.typeKind != VoidT:
      dest.addParLe ExprX, info
      isStmtListExpr = true

  copyIntoKind dest, BlockS, info:
    var labelName = "returnLabel"
    makeLocalSym outer, labelName
    c.returnLabel = pool.syms.getOrIncl(labelName)
    dest.addSymDef c.returnLabel, info

    copyIntoKind dest, StmtsS, info:
      inc procCall # skip `(call`
      takeTree dest, procCall # `fn`
      var params = routine.params
      while procCall.kind != ParRi:
        mapParamToLocal(c, dest, procCall, params)

      var procBody = routine.body
      inlineRoutineBody(c, dest, procBody)

  if isStmtListExpr:
    let toReplace = c.newVars.getOrDefault(c.resultSym, NoSymId)
    assert toReplace != NoSymId, "cannot find result declaration"
    dest.addSymUse toReplace, info
    dest.addParRi()

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let le = n.firstSon
  var ri = le
  skip le
  var routine = default(Routine)
  if shouldInlineCall(c, ri, routine):
    if le.kind == Symbol:
      # target is simple enough to do store forwarding from `result/return` to `le`:
      doInline(c, dest, ri, routine, Target(kind: TargetIsNode, pos: le))
    else:
      copyInto dest, n:
        tr c, dest, n
        doInline(c, dest, n, routine, Target(kind: TargetIsNone))
  else:
    copyInto dest, n:
      tr c, dest, n
      tr c, dest, n

proc trLocalDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let r = asLocal(n)
  var routine = default(Routine)
  let inlineCall = shouldInlineCall(c, r.value, routine)
  copyInto dest, n:
    copyTree dest, r.name
    copyTree dest, r.ex
    copyTree dest, r.pragmas
    copyTree dest, r.typ
    if inlineCall:
      addEmpty dest, r.value.info
    else:
      tr c, dest, r.value

  if inlineCall:
    doInline(c, dest, r.value, routine, Target(kind: TargetIsSym, sym: r.name.symId))

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of AtomsExceptSymUse:
    copyTree dest, n
  of Symbol:
    addSymUse dest, n.symId, n.info
  of ProcDecl, FuncDecl, MacroDecl, MethodDecl, ConverterDecl:
    trProcDecl c, dest, n
  of Asgn:
    trAsgn c, dest, n
  of LetDecl, VarDecl, CursorDecl, ResultDecl:
    trLocalDecl c, dest, n
  of CallKinds:
    var routine = default(Routine)
    let inlineCall = shouldInlineCall(c, n, routine)
    if inlineCall:
      doInline(c, dest, n, routine, Target(kind: TargetIsNone))
    else:
      for ch in sons(dest, n):
        tr c, dest, ch
  of DeclarativeNodes, Pragmas, TemplateDecl, IteratorDecl,
     UsingStmt, CommentStmt, BindStmt, MixinStmt, ContinueStmt:
    copyTree dest, n
  else:
    for ch in sons(dest, n):
      tr c, dest, ch

proc inlineCalls*(n: Cursor; thisModuleSuffix: string; ptrSize: int): TokenBuf =
  var c = createInliner(thisModuleSuffix, ptrSize)
  result = createTokenBuf(300)
  var n = n
  tr(c, result, n)
  #result = lowerExprs(p, result)
