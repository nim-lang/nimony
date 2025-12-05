#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Implements the mapping from Nim AST -> NIF.

when defined(nifBench):
  import std / monotimes

import std / [assertions, syncio, os]

import compiler / [
  ast, options, pathutils, renderer, lineinfos,
  syntaxes, llstream, idents, msgs]

import ".." / lib / nifbuilder
import ".." / models / nifler_tags

proc nodeKindTranslation(k: TNodeKind): NiflerKind =
  # many of these kinds are never returned by the parser.
  case k
  of nkCommand: CmdL
  of nkCall: CallL
  of nkCallStrLit: CallstrlitL
  of nkInfix: InfixL
  of nkPrefix: PrefixL
  of nkHiddenCallConv: ErrL
  of nkExprEqExpr: VvL
  of nkExprColonExpr: KvL
  of nkPar: ParL
  of nkObjConstr: OconstrL
  of nkCurly: CurlyL
  of nkCurlyExpr: CurlyatL
  of nkBracket: BracketL
  of nkBracketExpr: AtL
  of nkPragmaBlock, nkPragmaExpr: PragmaxL
  of nkDotExpr: DotL
  of nkAsgn, nkFastAsgn: AsgnL
  of nkIfExpr, nkIfStmt: IfL
  of nkWhenStmt, nkRecWhen: WhenL
  of nkWhileStmt: WhileL
  of nkCaseStmt, nkRecCase: CaseL
  of nkForStmt: ForL
  of nkDiscardStmt: DiscardL
  of nkBreakStmt: BreakL
  of nkReturnStmt: RetL
  of nkElifExpr, nkElifBranch: ElifL
  of nkElseExpr, nkElse: ElseL
  of nkOfBranch: OfL
  of nkCast: CastL
  of nkLambda: ProcL
  of nkAccQuoted: QuotedL
  of nkTableConstr: TabconstrL
  of nkStmtListType, nkStmtListExpr, nkStmtList, nkRecList, nkArgList: StmtsL
  of nkBlockStmt, nkBlockExpr, nkBlockType: BlockL
  of nkStaticStmt: StaticstmtL
  of nkBind, nkBindStmt: BindL
  of nkMixinStmt: MixinL
  of nkAddr: AddrL
  of nkGenericParams: TypevarsL
  of nkFormalParams: ParamsL
  of nkImportAs: ImportasL
  of nkRaiseStmt: RaiseL
  of nkContinueStmt: ContinueL
  of nkYieldStmt: YldL
  of nkProcDef: ProcL
  of nkFuncDef: FuncL
  of nkMethodDef: MethodL
  of nkConverterDef: ConverterL
  of nkMacroDef: MacroL
  of nkTemplateDef: TemplateL
  of nkIteratorDef: IteratorL
  of nkExceptBranch: ExceptL
  of nkTypeOfExpr: TypeofL
  of nkFinally: FinL
  of nkTryStmt: TryL
  of nkImportStmt: ImportL
  of nkImportExceptStmt: ImportexceptL
  of nkIncludeStmt: IncludeL
  of nkExportStmt: ExportL
  of nkExportExceptStmt: ExportexceptL
  of nkFromStmt: FromimportL
  of nkPragma: PragmasL
  of nkAsmStmt: AsmL
  of nkDefer: DeferL
  of nkUsingStmt: UsingL
  of nkCommentStmt: CommentL
  of nkObjectTy: ObjectL
  of nkTupleTy, nkTupleClassTy: TupleL
  of nkTypeClassTy: ConceptL
  of nkStaticTy: StaticL
  of nkRefTy: RefL
  of nkPtrTy: PtrL
  of nkVarTy: MutL
  of nkDistinctTy: DistinctL
  of nkIteratorTy: ItertypeL
  of nkEnumTy: EnumL
  #of nkEnumFieldDef: EnumFieldDecl
  of nkTupleConstr: TupL
  of nkOutTy: OutL
  else: ErrL

template addTree(b: var Builder; tag: NiflerKind) = b.addTree $tag

template withTree(b: var Builder; tag: NiflerKind; body: untyped) =
  b.addTree tag
  body
  b.endTree()

type
  TranslationContext = object
    conf: ConfigRef
    section: NiflerKind
    b, deps: Builder
    portablePaths: bool
    depsEnabled, lineInfoEnabled: bool
    inWhen: int

proc absLineInfo(i: TLineInfo; c: var TranslationContext) =
  var fp = toFullPath(c.conf, i.fileIndex)
  if c.portablePaths:
    fp = relativePath(fp, getCurrentDir(), '/')
  c.b.addLineInfo int32(i.col), int32(i.line), fp

proc relLineInfo(n, parent: PNode; c: var TranslationContext;
                 emitSpace = false) =
  if not c.lineInfoEnabled: return
  let i = n.info
  if parent == nil:
    absLineInfo i, c
    return
  let p = parent.info
  if i.fileIndex != p.fileIndex:
    absLineInfo i, c
    return

  let colDiff = int32(i.col) - int32(p.col)
  let lineDiff = int32(i.line) - int32(p.line)
  c.b.addLineInfo colDiff, lineDiff, ""

proc addIntLit*(b: var Builder; u: BiggestInt; suffix: string) =
  assert suffix.len > 0
  b.withTree SufL:
    b.addIntLit u
    b.addStrLit suffix

proc addUIntLit*(b: var Builder; u: BiggestUInt; suffix: string) =
  assert suffix.len > 0
  b.withTree SufL:
    b.addUIntLit u
    b.addStrLit suffix

proc addFloatLit*(b: var Builder; u: BiggestFloat; suffix: string) =
  assert suffix.len > 0
  b.withTree SufL:
    b.addFloatLit u
    b.addStrLit suffix

type IdentDefName = object
  name, visibility, pragma: PNode

proc splitIdentDefName(n: PNode): IdentDefName =
  result = IdentDefName(visibility: nil, pragma: nil)
  if n.kind == nkPragmaExpr:
    result.pragma = n[1]
    if n[0].kind == nkPostfix:
      result.visibility = n[0][0]
      result.name = n[0][1]
    else:
      result.name = n[0]
  elif n.kind == nkPostfix:
    result.visibility = n[0]
    result.name = n[1]
  else:
    result.name = n

proc toNif*(n, parent: PNode; c: var TranslationContext; allowEmpty = false)

proc toVarTuple(v: PNode, n: PNode; c: var TranslationContext) =
  c.b.addTree(UnpacktupL)
  for i in 0..<v.len-1: # ignores typedesc
    c.b.addTree(LetL)

    toNif(v[i], n, c) # name

    c.b.addEmpty 4 # export marker, pragmas, type, value
    c.b.endTree() # LetDecl
  c.b.endTree() # UnpackIntoTuple

proc handleCaseIdentDefs(n, parent: PNode; c: var TranslationContext) =
  if n.kind == nkIdentDefs and n.len > 3:
    # multiple ident defs, we need to add StmtsL
    c.b.addTree(StmtsL)
    toNif(n, parent, c)
    c.b.endTree()
  else:
    toNif(n, parent, c)

proc toNif*(n, parent: PNode; c: var TranslationContext; allowEmpty = false) =
  case n.kind
  of nkNone:
    assert false, "unexpected nkNone"
  of nkEmpty:
    assert allowEmpty, "unexpected nkEmpty"
    c.b.addEmpty 1
  of nkNilLit:
    relLineInfo(n, parent, c)
    c.b.addRaw "(nil)"
  of nkStrLit:
    relLineInfo(n, parent, c)
    c.b.addStrLit n.strVal
  of nkRStrLit:
    relLineInfo(n, parent, c)
    c.b.addStrLit n.strVal, "R"
  of nkTripleStrLit:
    relLineInfo(n, parent, c)
    c.b.addStrLit n.strVal, "T"
  of nkCharLit:
    relLineInfo(n, parent, c)
    c.b.addCharLit char(n.intVal)
  of nkIntLit:
    relLineInfo(n, parent, c, true)
    c.b.addIntLit n.intVal
  of nkInt8Lit:
    relLineInfo(n, parent, c, true)
    c.b.addIntLit n.intVal, "i8"
  of nkInt16Lit:
    relLineInfo(n, parent, c, true)
    c.b.addIntLit n.intVal, "i16"
  of nkInt32Lit:
    relLineInfo(n, parent, c, true)
    c.b.addIntLit n.intVal, "i32"
  of nkInt64Lit:
    relLineInfo(n, parent, c, true)
    c.b.addIntLit n.intVal, "i64"
  of nkUIntLit:
    relLineInfo(n, parent, c, true)
    c.b.addUIntLit cast[BiggestUInt](n.intVal)
  of nkUInt8Lit:
    relLineInfo(n, parent, c, true)
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u8"
  of nkUInt16Lit:
    relLineInfo(n, parent, c, true)
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u16"
  of nkUInt32Lit:
    relLineInfo(n, parent, c, true)
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u32"
  of nkUInt64Lit:
    relLineInfo(n, parent, c, true)
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u64"
  of nkFloatLit:
    relLineInfo(n, parent, c, true)
    c.b.addFloatLit n.floatVal
  of nkFloat32Lit:
    relLineInfo(n, parent, c, true)
    c.b.addFloatLit n.floatVal, "f32"
  of nkFloat64Lit:
    relLineInfo(n, parent, c, true)
    c.b.addFloatLit n.floatVal, "f64"
  of nkFloat128Lit:
    relLineInfo(n, parent, c, true)
    c.b.addFloatLit n.floatVal, "f128"
  of nkIdent:
    relLineInfo(n, parent, c, true)
    c.b.addIdent n.ident.s
  of nkTypeDef:
    relLineInfo(n, parent, c)
    c.b.addTree TypeL
    let split = splitIdentDefName(n[0])

    toNif(split.name, n, c)

    if split.visibility != nil:
      c.b.addRaw " x"
    else:
      c.b.addEmpty

    toNif(n[1], n, c, allowEmpty = true) # generics

    if split.pragma != nil:
      toNif(split.pragma, n, c)
    else:
      c.b.addEmpty

    for i in 2..<n.len:
      toNif(n[i], n, c, allowEmpty = true)
    c.b.endTree()

  of nkTypeSection:
    for i in 0..<n.len:
      toNif(n[i], parent, c)

  of nkVarSection:
    c.section = VarL
    for i in 0..<n.len:
      toNif(n[i], parent, c)
  of nkLetSection:
    c.section = LetL
    for i in 0..<n.len:
      toNif(n[i], parent, c)
  of nkConstSection:
    c.section = ConstL
    for i in 0..<n.len:
      toNif(n[i], parent, c)

  of nkFormalParams:
    c.section = ParamL
    relLineInfo(n, parent, c)
    c.b.addTree(ParamsL)
    for i in 1..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()
    # put return type outside of `(params)`:
    toNif(n[0], n, c, allowEmpty = true)
  of nkGenericParams:
    c.section = TypevarL
    relLineInfo(n, parent, c)
    c.b.addTree(TypevarsL)
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

  of nkIdentDefs, nkConstDef:
    # multiple ident defs are annoying so we remove them here:
    assert c.section != NiflerKind.None
    let last = n.len-1
    for i in 0..last - 2:
      relLineInfo(n[i], parent, c)
      c.b.addTree(c.section)
      # flatten it further:
      let split = splitIdentDefName(n[i])

      toNif(split.name, n[i], c) # name

      if split.visibility != nil:
        c.b.addRaw " x"
      else:
        c.b.addEmpty

      if split.pragma != nil:
        toNif(split.pragma, n[i], c)
      else:
        c.b.addEmpty

      toNif(n[last-1], n[i], c, allowEmpty = true) # type

      toNif(n[last], n[i], c, allowEmpty = true) # value
      c.b.endTree()
  of nkDo:
    relLineInfo(n, parent, c)
    c.b.addTree(DoL)
    toNif(n[paramsPos], n, c, allowEmpty = true)
    toNif(n[bodyPos], n, c)
    c.b.endTree()
  of nkLambda:
    relLineInfo(n, parent, c)
    c.b.addTree(ProcL)
    c.b.addEmpty # adds name placeholder
    for i in 0..<n.len:
      toNif(n[i], n, c, allowEmpty = true)
    c.b.endTree()
  of nkOfInherit:
    if n.len == 1:
      toNif(n[0], parent, c)
    else:
      relLineInfo(n, parent, c)
      c.b.addTree(ParL)
      for i in 0..<n.len:
        toNif(n[i], n, c)
      c.b.endTree()
  of nkOfBranch:
    relLineInfo(n, parent, c)
    c.b.addTree(OfL)
    c.b.addTree(RangesL)
    for i in 0..<n.len-1:
      toNif(n[i], n, c)
    c.b.endTree()
    handleCaseIdentDefs(n[n.len-1], n, c)
    c.b.endTree()
  of nkElse:
    relLineInfo(n, parent, c)
    c.b.addTree(ElseL)
    handleCaseIdentDefs(n[n.len-1], n, c)
    c.b.endTree()

  of nkStmtListType, nkStmtListExpr:
    relLineInfo(n, parent, c)
    c.b.addTree(ExprL)
    c.b.addTree(StmtsL)
    for i in 0..<n.len-1:
      toNif(n[i], n, c)
    c.b.endTree()
    if n.len > 0:
      toNif(n[n.len-1], n, c)
    else:
      c.b.addEmpty
    c.b.endTree()

  of nkProcTy, nkIteratorTy:
    relLineInfo(n, parent, c)
    if n.kind == nkProcTy:
      c.b.addTree(ProctypeL)
    else:
      c.b.addTree(ItertypeL)

    if n.len == 0:
      # it's a type class
      c.b.endTree()
      return

    c.b.addEmpty 4 # 0: name
    # 1: export marker
    # 2: pattern
    # 3: generics

    if n.len > 0:
      toNif n[0], n, c, allowEmpty = true  # 4: params
    else:
      c.b.addEmpty

    if n.len > 1:
      toNif n[1], n, c, allowEmpty = true  # 5: pragmas
    else:
      c.b.addEmpty

    c.b.addEmpty 2 # 6: exceptions
    # 7: body
    c.b.endTree()

  of nkEnumTy:
    # EnumField
    #   SymDef "x"
    #   Empty      # export marker (always empty)
    #   Empty      # pragmas
    #   EnumType
    #   (Integer value, "string value")
    relLineInfo(n, parent, c)
    if n.len == 0:
      # typeclass, compiles to identifier for nimony
      c.b.addIdent "enum"
    else:
      c.b.addTree(EnumL)
      assert n[0].kind == nkEmpty
      c.b.addEmpty # base type
      for i in 1..<n.len:
        let it = n[i]

        var name: PNode
        var val: PNode
        var pragma: PNode

        if it.kind == nkEnumFieldDef:
          let first = it[0]
          if first.kind == nkPragmaExpr:
            name = first[0]
            pragma = first[1]
          else:
            name = it[0]
            pragma = nil
          val = it[1]
        elif it.kind == nkPragmaExpr:
          name = it[0]
          pragma = it[1]
          val = nil
        else:
          name = it
          pragma = nil
          val = nil

        relLineInfo(it, n, c)

        c.b.addTree(EfldL)

        toNif name, it, c
        c.b.addEmpty # export marker

        if pragma == nil:
          c.b.addEmpty
        else:
          toNif(pragma, it, c)

        c.b.addEmpty # type (filled by sema)

        if val == nil:
          c.b.addEmpty
        else:
          toNif(val, it, c)
        c.b.endTree()

      c.b.endTree()

  of nkProcDef, nkFuncDef, nkConverterDef, nkMacroDef, nkTemplateDef, nkIteratorDef, nkMethodDef:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))

    var name: PNode
    var visibility: PNode = nil
    if n[0].kind == nkPostfix:
      visibility = n[0][0]
      name = n[0][1]
    else:
      name = n[0]

    toNif(name, n, c)
    if visibility != nil:
      c.b.addRaw " x"
    else:
      c.b.addEmpty

    for i in 1..<n.len:
      toNif(n[i], n, c, allowEmpty = true)
    c.b.endTree()

  of nkVarTuple:
    relLineInfo(n, parent, c)
    assert n[n.len-2].kind == nkEmpty
    c.b.addTree(UnpackdeclL)
    toNif(n[n.len-1], n, c, allowEmpty = true)

    c.b.addTree(UnpacktupL)
    for i in 0..<n.len-2:
      if n[i].kind == nkVarTuple:
        toNif(n[i], n, c)
      else:
        c.b.addTree(c.section)
        let split = splitIdentDefName(n[i])
        toNif(split.name, n, c) # name

        if split.visibility != nil:
          c.b.addRaw " x"
        else:
          c.b.addEmpty

        if split.pragma != nil:
          toNif(split.pragma, n, c)
        else:
          c.b.addEmpty

        c.b.addEmpty 2 # type, value
        c.b.endTree()
    c.b.endTree()
    c.b.endTree()

  of nkForStmt:
    relLineInfo(n, parent, c)
    c.b.addTree(ForL)

    toNif(n[n.len-2], n, c) # iterator

    if n.len == 3 and n[0].kind == nkVarTuple:
      toVarTuple(n[0], n, c)
    else:
      c.b.addTree(UnpackflatL)
      for i in 0..<n.len-2:
        if n[i].kind == nkVarTuple:
          toVarTuple(n[i], n, c)
        else:
          c.b.addTree(LetL)

          toNif(n[i], n, c) # name

          c.b.addEmpty 4 # export marker, pragmas, type, value
          c.b.endTree() # LetDecl
      c.b.endTree() # UnpackIntoFlat

    # for-loop-body:
    toNif(n[n.len-1], n, c)
    c.b.endTree()

  of nkRefTy, nkPtrTy:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

  of nkObjectTy:
    let kind = nodeKindTranslation(n.kind)
    c.section = FldL
    relLineInfo(n, parent, c)
    c.b.addTree(kind)
    for i in 0..<n.len-3:
      toNif(n[i], n, c, allowEmpty = true)
    # n.len-3: pragmas: must be empty (it is deprecated anyway)
    if n.len == 0:
      # object typeclass, has no children
      discard
    else:
      if n[n.len-3].kind != nkEmpty:
        c.b.addTree ErrL
        c.b.endTree()

      toNif(n[n.len-2], n, c, allowEmpty = true)
      let last {.cursor.} = n[n.len-1]
      if last.kind == nkRecList:
        for child in last:
          c.section = FldL
          toNif(child, n, c)
      elif last.kind != nkEmpty:
        toNif(last, n, c)
    c.b.endTree()

  of nkTupleTy, nkTupleClassTy:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      assert n[i].kind == nkIdentDefs
      let def = n[i]
      let last = def.len - 1
      for j in 0..last - 2:
        relLineInfo(def[j], parent, c)
        c.b.addTree(KvL)
        let split = splitIdentDefName(def[j])

        toNif(split.name, def[j], c) # name

        toNif(def[last-1], def[j], c, allowEmpty = true) # type

        c.b.endTree()
    c.b.endTree()

  of nkImportStmt, nkFromStmt, nkExportStmt, nkExportExceptStmt, nkImportAs, nkImportExceptStmt, nkIncludeStmt:
    # the usual recursion:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

    if c.depsEnabled:
      let oldLineInfoEnabled = c.lineInfoEnabled
      c.lineInfoEnabled = false
      let oldDepsEnabled = c.depsEnabled
      swap c.b, c.deps
      c.depsEnabled = false

      relLineInfo(n, nil, c)
      c.b.addTree(nodeKindTranslation(n.kind))
      if c.inWhen > 0:
        # mark it as a conditional dependency:
        c.b.addKeyw "when"
      for i in 0..<n.len:
        toNif(n[i], nil, c)
      c.b.endTree()

      c.depsEnabled = oldDepsEnabled
      swap c.b, c.deps
      c.lineInfoEnabled = oldLineInfoEnabled
  of nkCallKinds:
    let oldDepsEnabled = c.depsEnabled
    if n.len > 0 and n[0].kind == nkIdent and n[0].ident.s == "runnableExamples":
      c.depsEnabled = false
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()
    c.depsEnabled = oldDepsEnabled
  of nkDiscardStmt, nkBreakStmt, nkContinueStmt, nkReturnStmt, nkRaiseStmt,
      nkBlockStmt, nkBlockExpr, nkBlockType, nkTypeClassTy, nkAsmStmt:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c, allowEmpty = true)
    c.b.endTree()
  of nkExceptBranch:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    if n.len == 1:
      c.b.addEmpty 1
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()
  of nkWhenStmt:
    inc c.inWhen
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()
    dec c.inWhen
  else:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

proc initTranslationContext*(conf: ConfigRef; outfile: string; portablePaths, depsEnabled: bool;
                              depsOnly = false): TranslationContext =
  result = TranslationContext(conf: conf,
    portablePaths: portablePaths, depsEnabled: depsEnabled or depsOnly, lineInfoEnabled: not depsOnly)
  if depsOnly:
    # Memory-only builder for main output (will be discarded)
    result.b = nifbuilder.open(1024)
    result.deps = nifbuilder.open(outfile)
  else:
    result.b = nifbuilder.open(outfile)
    if depsEnabled:
      result.deps = nifbuilder.open(outfile.changeFileExt(".deps.nif"))

proc close*(c: var TranslationContext; depsOnly = false) =
  if depsOnly:
    discard "discard main output"
  else:
    c.b.close()
  if c.depsEnabled:
    c.deps.endTree()
    c.deps.close()

proc moduleToIr*(n: PNode; c: var TranslationContext) =
  c.b.addHeader "Nifler", "nim-parsed"
  if c.depsEnabled:
    c.deps.addHeader "Nifler", "nim-deps"
    c.deps.addTree StmtsL
  toNif(n, nil, c)

proc createConf(): ConfigRef =
  result = newConfigRef()
  #result.notes.excl hintLineTooLong
  result.errorMax = 1000

template bench(task, body) =
  when defined(nifBench):
    let t0 = getMonoTime()
    body
    echo task, " TOOK ", getMonoTime() - t0
  else:
    body

proc parseFile*(thisfile, outfile: string; portablePaths, depsEnabled, depsOnly: bool) =
  let stream = llStreamOpen(AbsoluteFile thisfile, fmRead)
  if stream == nil:
    quit "cannot open file: " & thisfile
  else:
    var conf = createConf()
    let fileIdx = fileInfoIdx(conf, AbsoluteFile thisfile)
    var parser: Parser = default(Parser)
    syntaxes.openParser(parser, fileIdx, stream, newIdentCache(), conf)
    bench "parseAll":
      let fullTree = parseAll(parser)

    if conf.errorCounter > 0:
      closeParser(parser)
      quit QuitFailure

    var tc = initTranslationContext(conf, outfile, portablePaths, depsEnabled, depsOnly)

    bench "moduleToIr":
      moduleToIr(fullTree, tc)
    closeParser(parser)
    tc.close(depsOnly)
