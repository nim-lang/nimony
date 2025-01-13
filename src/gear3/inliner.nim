#
#
#           Gear3 Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Inliner. We inline function calls that are annotated with `.inline` or that are
## considered simple enough.
## Inlining requires:
## - copy the body of the function to the caller into a new block `B`.
## - replace the parameter by a local variable that is initialized with the
##   passed value.
## - replace `result` by the assigned destination of `x = toInline(args)` if it exists,
##   otherwise by a temporary variable.
## - replace `return` by `break B`.
## - copy local variables from the body into fresh ones.

import std / [tables]
include nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav]


type
  TargetKind = enum
    TargetIsNone, TargetIsSym, TargetIsNode
  Target = object
    kind: TargetKind
    sym: SymId
    pos: Cursor

  InlineContext* = object
    returnLabel: SymId
    resultSym: SymId
    newVars: Table[SymId, SymId]
    target: Target

  Context* = object
    thisRoutine: SymId

proc createInliner(): Context =
  result = Context(thisRoutine: NoSymId)

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.add n
  var r = takeRoutine(n)
  let oldThisRoutine = c.thisRoutine
  c.thisRoutine = r.name.symId
  copyTree dest, r.name
  copyTree dest, r.exported
  copyTree dest, r.pattern
  copyTree dest, r.typevars
  copyTree dest, r.params
  copyTree dest, r.pragmas
  copyTree dest, r.effects
  if r.body.stmtKind == StmtsS and not isGeneric(r):
    tr c, dest, r.body
  else:
    copyTree dest, r.body
  dest.addParRi()
  c.thisRoutine = oldThisRoutine

proc shouldInlineRoutine(pragmas: Cursor): bool =
  hasBuiltinPragma(pragmas, Inline)

proc shouldInlineCall(c: var Context; n: Cursor): FullTypeId =
  result = FullTypeId(m: TreeId(-1), t: StartPos)
  if n.kind == Call and c.thisRoutine != NoSymId:
    var callee = n.firstSon
    if callee.kind == GenericRoutineInst:
      callee = ithSon(tree, callee, 2)
    if callee.kind in {ModuleSymUse, SymUse}:
      let s = accessModuleSym(c.p, tree, callee)
      # who says we cannot inline recursions?
      let declPos = c.p[s].declPos
      if c.p[declPos].kind in {ProcDecl, FuncDecl, ConverterDecl}:
        let pragmas = ithSon(c.p[declPos.m], declPos.t, routinePragmasPos)
        if shouldInlineRoutine(c.p, c.p[declPos.m], pragmas):
          result = declPos

proc inlineRoutineBody(c: var InlineContext; dest: var TokenBuf; n: var Cursor; call: Tree) =
  let info = translateLineInfo(n.info, c.inl)
  case n.kind
  of SymDef, SymRedef:
    let id = n.symId
    let freshId = copySym(dest, c.inl.p[dest.m], id, c.inl.p[tree.m])

    c.newVars[id] = freshId
    #c.inl.p[dest.m].syms[freshId].declPos = FullTypeId(m: dest.id, t: NodePos(n.int - 1))
    dest.addSymDef freshId, info
  of SymUse:
    if c.resultSym == n.symId:
      case c.target.kind
      of TargetIsNone:
        let toReplace = c.newVars.getOrDefault(n.symId, NoSymId)
        assert toReplace != NoSymId, "cannot find result declaration"
        dest.addSymUse toReplace, info
      of TargetIsSym:
        dest.addSymUse c.target.sym, info
      of TargetIsNode:
        copyTree dest, call, c.target.pos
    else:
      let toReplace = c.newVars.getOrDefault(n.symId, NoSymId)
      if toReplace != NoSymId:
        dest.addSymUse toReplace, info
      else:
        translateSymUse(dest, tree, n, c.inl.p, info)
  of ReturnStmt:
    let retVal = n.firstSon
    if retVal.kind != Empty and not (retVal.kind == SymUse and c.resultSym == retVal.symId):
      # generate assignment: `dest = result`
      copyIntoKind dest, Asgn, info:
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
          copyTree dest, call, c.target.pos
        inlineRoutineBody c, dest, tree, retVal, call

    copyIntoKind dest, BreakStmt, info:
      dest.addSymUse c.returnLabel, info

  of CharLit, IntLit, UIntLit, FloatLit:
    dest.add(n.kind, info,
            c.inl.p[c.inl.call].numbers.getOrIncl(c.inl.p[c.inl.body].numbers[n.litId]))

  of Ident, StrLit, VerbatimStrLit:
    dest.add(n.kind, info,
             c.inl.p[c.inl.call].strings.getOrIncl(c.inl.p[c.inl.body].strings[n.litId]))
  of ModuleLit:
    dest.add(n.kind, info,
             c.inl.p[c.inl.call].moduleNames.getOrIncl(c.inl.p[c.inl.body].moduleNames[n.litId]))

  of AtomsWithDirectOperand:
    dest.add(n.kind, info, n.operand)

  of DeclarativeNodes:
    copyTreeX dest, tree, n, c.inl.p
  of ResultDecl:
    if c.target.kind == TargetIsNone:
      # we need the result declaration. But it is inlined, so
      # it is not a `ResultDecl`!
      copyIntoKind dest, VarDecl, info:
        for ch in sonsReadonly(tree, n):
          inlineRoutineBody(c, dest, tree, ch, call)
    else:
      # discard the result declaration!
      discard
    c.resultSym = n.firstSon.symId
  else:
    copyIntoKind dest, n.kind, info:
      for ch in sonsReadonly(tree, n):
        inlineRoutineBody(c, dest, tree, ch, call)


proc doInline(outer: var Context; dest: var TokenBuf; treec: Tree; procCall: NodePos; routine: FullTypeId;
              target: Target) =
  assert treec[procCall].kind == Call
  #echo "NOW INLINING ", toString(outer.p[routine.m], routine.t, outer.p)
  #echo "CALL IS ", toString(treec, procCall, outer.p)
  var c = InlineContext(inl: LineInfoContext(p: outer.p, call: dest.m, body: outer.p[routine.m].m),
                        target: target, resultSym: NoSymId)

  let params = ithSon(c.inl.p[routine.m], routine.t, routineParamsPos)

  let info = treec[procCall].info

  var pp = InvalidPatchPos
  if target.kind == TargetIsNone:
    let t = getType(c.inl.p, treec, procCall)
    let tk = effectiveKind(c.inl.p, t)
    if tk != VoidTy:
      pp = prepare(dest, StmtListExpr, info)
      copyTreeX(dest, c.inl.p[t.m], t.t, c.inl.p)

  copyIntoKind dest, BlockStmt, info:
    c.returnLabel = declareSym(c.inl.p[c.thisModule], LabelDecl, c.inl.p[c.thisModule].strings.getOrIncl("returnLabel"))
    dest.addSymDef c.returnLabel, info

    copyIntoKind dest, StmtList, info:
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

  if pp.isValid:
    let toReplace = c.newVars.getOrDefault(c.resultSym, NoSymId)
    assert toReplace != NoSymId, "cannot find result declaration"
    dest.addSymUse toReplace, info
    dest.patch pp

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let (le, ri) = sons2(tree, n)
  let routine = shouldInlineCall(c, tree, ri)
  if routine.m != TreeId(-1):
    if le.kind in {SymUse, ModuleSymUse}:
      # target is simple enough to do store forwarding from `result/return` to `le`:
      doInline(c, dest, tree, ri, routine, Target(kind: TargetIsNode, pos: le))
    else:
      copyInto dest, n:
        tr c, dest, tree, le
        doInline(c, dest, tree, ri, routine, Target(kind: TargetIsNone))
  else:
    copyInto dest, n:
      tr c, dest, tree, le
      tr c, dest, tree, ri

proc trLocalDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let r = asLocal(tree, n)
  let routine = shouldInlineCall(c, tree, r.value)
  copyInto dest, n:
    copyTree dest, tree, r.name
    copyTree dest, tree, r.ex
    copyTree dest, tree, r.pragmas
    copyTree dest, tree, r.typ
    if routine.m != TreeId(-1):
      addEmpty dest, r.value.info
    else:
      tr c, dest, tree, r.value

  if routine.m != TreeId(-1):
    doInline(c, dest, tree, r.value, routine, Target(kind: TargetIsSym, sym: r.name.symId))

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of AtomsExceptSymUse:
    copyTree dest, tree, n
  of SymUse:
    addSymUse dest, n.symId, n.info
    when false:
      # see if we need to replace this symbol:
      let x = c.newVars.getOrDefault(n.symId, NoSymId)
      if x != NoSymId:
        dest.addSymUse x, n.info
      else:
        copyTree dest, tree, n
  of ProcDecl, FuncDecl, MacroDecl, MethodDecl, ConverterDecl:
    trProcDecl c, dest, tree, n
  of Asgn:
    trAsgn c, dest, tree, n
  of LetDecl, VarDecl, CursorDecl, ResultDecl:
    trLocalDecl c, dest, tree, n
  of Call:
    let routine = shouldInlineCall(c, tree, n)
    if routine.m != TreeId(-1):
      doInline(c, dest, tree, n, routine, Target(kind: TargetIsNone))
    else:
      for ch in sons(dest, tree, n):
        tr c, dest, tree, ch
  of DeclarativeNodes, Pragmas, TemplateDecl, IteratorDecl,
     UsingStmt, CommentStmt, BindStmt, MixinStmt, ContinueStmt:
    copyTree dest, tree, n
  else:
    for ch in sons(dest, tree, n):
      tr c, dest, tree, ch

proc inlineCalls*(n: Cursor): TokenBuf =
  let thisModule = p[t].m

  var c = createInliner()
  result = createTokenBuf(300)

  tr(c, p[result], p[t], StartPos)
  patch p[result], PatchPos(0)

  result = lowerExprs(p, result)
