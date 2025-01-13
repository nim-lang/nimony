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
import ".." / nimony / [nimony_model, decls, programs, typenav]


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

  InlineContext* = object
    returnLabel: SymId
    resultSym: SymId
    newVars: Table[SymId, SymId]
    target: Target
    c: ptr Context

proc createInliner(thisModuleSuffix: string): Context =
  result = Context(thisRoutine: NoSymId, thisModuleSuffix: thisModuleSuffix)

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
    var callee = n.firstSon
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


proc doInline(outer: var Context; dest: var TokenBuf; procCall: Cursor; routine: Routine;
              target: Target) =
  assert procCall.exprKind in CallKinds
  var c = InlineContext(target: target, resultSym: NoSymId, c: addr outer)

  let info = procCall.info

  var isStmtListExpr = false
  if target.kind == TargetIsNone:
    let t = getType(procCall)
    let tk = effectiveKind(t)
    if tk != VoidT:
      dest.addParLe ExprX, info
      isStmtListExpr = true

  copyIntoKind dest, BlockS, info:
    c.returnLabel = declareSym(c.inl.p[c.thisModule], LabelDecl, c.inl.p[c.thisModule].strings.getOrIncl("returnLabel"))
    dest.addSymDef c.returnLabel, info

    copyIntoKind dest, StmtsS, info:
      var it = initNodePosIter(treec, procCall)
      next it, treec # skip `fn`
      for param in sonsFrom1(c.inl.p[routine.m], params):
        # assign parameters: This also ensures that side effects are executed,
        # consider: `inlineCall effect(x)` where `inlineCall` does not even use
        # its first parameter!
        let r = asLocal(c.inl.p[routine.m], param)
        let tt = FullTypeId(m: routine.m, t: r.typ)
        if effectiveKind(c.inl.p, tt) != SymKindTy:
          copyIntoKind dest, LetDecl, info:
            assert c.inl.p[routine.m][r.name].kind == SymDef
            let id = c.inl.p[routine.m][r.name].symId
            let freshId = copySym(dest, c.inl.p[dest.m], id, c.inl.p[c.inl.body])
            c.inl.p[c.thisModule].syms[freshId].kind = LetDecl

            c.newVars[id] = freshId
            dest.addSymDef freshId, info

            copyTreeX dest, c.inl.p[routine.m], r.ex, c.inl.p
            copyTreeX dest, c.inl.p[routine.m], r.pragmas, c.inl.p
            if passByConstRef(c.inl.p, tt, c.inl.p[routine.m], r.pragmas):
              copyIntoKind dest, PtrTy, info:
                copyTreeX dest, c.inl.p[routine.m], r.typ, c.inl.p
                # This empty node here marks the type as `const`, see codegen.nim:
                #  if hasXsons(tree, typ, 2): f.add ConstKeyword
                dest.addEmpty info
            else:
              copyTreeX dest, c.inl.p[routine.m], r.typ, c.inl.p
            copyTreeX dest, treec, it.current, c.inl.p
        next it, treec

      let procBody = ithSon(c.inl.p[routine.m], routine.t, routineBodyPos)
      #echo "Inlining ", toString(c.inl.p[routine.m], procBody, c.inl.p)
      inlineRoutineBody(c, dest, c.inl.p[routine.m], procBody, treec)

  if isStmtListExpr:
    let toReplace = c.newVars.getOrDefault(c.resultSym, NoSymId)
    assert toReplace != NoSymId, "cannot find result declaration"
    dest.addSymUse toReplace, info
    dest.addParRi()

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let (le, ri) = sons2(tree, n)
  var routine = default(Routine)
  if shouldInlineCall(c, ri, routine):
    if le.kind == Symbol:
      # target is simple enough to do store forwarding from `result/return` to `le`:
      doInline(c, dest, ri, routine, Target(kind: TargetIsNode, pos: le))
    else:
      copyInto dest, n:
        tr c, dest, le
        doInline(c, dest, ri, routine, Target(kind: TargetIsNone))
  else:
    copyInto dest, n:
      tr c, dest, le
      tr c, dest, ri

proc trLocalDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let r = asLocal(n)
  let routine = shouldInlineCall(c, r.value)
  copyInto dest, n:
    copyTree dest, r.name
    copyTree dest, r.ex
    copyTree dest, r.pragmas
    copyTree dest, r.typ
    if routine.m != TreeId(-1):
      addEmpty dest, r.value.info
    else:
      tr c, dest, r.value

  if routine.m != TreeId(-1):
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
  of Call:
    let routine = shouldInlineCall(c, n)
    if routine.m != TreeId(-1):
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

proc inlineCalls*(n: Cursor; thisModuleSuffix: string): TokenBuf =
  var c = createInliner(thisModuleSuffix)
  result = createTokenBuf(300)
  var n = n
  tr(c, result, n)
  #result = lowerExprs(p, result)
