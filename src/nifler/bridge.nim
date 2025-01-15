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
  parser, llstream, idents, msgs]

import ".." / lib / nifbuilder

proc nodeKindTranslation(k: TNodeKind): string =
  # many of these kinds are never returned by the parser.
  case k
  of nkCommand: "cmd"
  of nkCall: "call"
  of nkCallStrLit: "callstrlit"
  of nkInfix: "infix"
  of nkPrefix: "prefix"
  of nkHiddenCallConv: "<error>"
  of nkExprEqExpr: "vv"
  of nkExprColonExpr: "kv"
  of nkPar: "par"
  of nkObjConstr: "obj"
  of nkCurly: "set"
  of nkCurlyExpr: "curlyx"
  of nkBracket: "arr"
  of nkBracketExpr: "at"
  of nkPragmaBlock, nkPragmaExpr: "pragmax"
  of nkDotExpr: "dot"
  of nkAsgn, nkFastAsgn: "asgn"
  of nkIfExpr, nkIfStmt: "if"
  of nkWhenStmt, nkRecWhen: "when"
  of nkWhileStmt: "while"
  of nkCaseStmt, nkRecCase: "case"
  of nkForStmt: "for"
  of nkDiscardStmt: "discard"
  of nkBreakStmt: "break"
  of nkReturnStmt: "ret"
  of nkElifExpr, nkElifBranch: "elif"
  of nkElseExpr, nkElse: "else"
  of nkOfBranch: "of"
  of nkCast: "cast"
  of nkLambda: "proc"
  of nkAccQuoted: "quoted"
  of nkTableConstr: "tab"
  of nkStmtListType, nkStmtListExpr, nkStmtList, nkRecList, nkArgList: "stmts"
  of nkBlockStmt, nkBlockExpr, nkBlockType: "block"
  of nkStaticStmt: "static"
  of nkBind, nkBindStmt: "bind"
  of nkMixinStmt: "mixin"
  of nkAddr: "addr"
  of nkGenericParams: "typevars"
  of nkFormalParams: "params"
  of nkImportAs: "importas"
  of nkRaiseStmt: "raise"
  of nkContinueStmt: "continue"
  of nkYieldStmt: "yld"
  of nkProcDef: "proc"
  of nkFuncDef: "func"
  of nkMethodDef: "method"
  of nkConverterDef: "converter"
  of nkMacroDef: "macro"
  of nkTemplateDef: "template"
  of nkIteratorDef: "iterator"
  of nkExceptBranch: "except"
  of nkTypeOfExpr: "typeof"
  of nkFinally: "fin"
  of nkTryStmt: "try"
  of nkImportStmt: "import"
  of nkImportExceptStmt: "importexcept"
  of nkIncludeStmt: "include"
  of nkExportStmt: "export"
  of nkExportExceptStmt: "exportexcept"
  of nkFromStmt: "from"
  of nkPragma: "pragmas"
  of nkAsmStmt: "asm"
  of nkDefer: "defer"
  of nkUsingStmt: "using"
  of nkCommentStmt: "comment"
  of nkObjectTy: "object"
  of nkTupleTy, nkTupleClassTy: "tuple"
  of nkTypeClassTy: "concept"
  of nkStaticTy: "staticTy"
  of nkRefTy: "ref"
  of nkPtrTy: "ptr"
  of nkVarTy: "mut"
  of nkDistinctTy: "distinct"
  of nkIteratorTy: "iterTy"
  of nkEnumTy: "enum"
  #of nkEnumFieldDef: EnumFieldDecl
  of nkTupleConstr: "tup"
  of nkOutTy: "outTy"
  else: "<error>"

type
  TranslationContext = object
    conf: ConfigRef
    section: string
    b, deps: Builder
    portablePaths: bool
    depsEnabled, lineInfoEnabled: bool

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
  b.withTree "suf":
    b.addIntLit u
    b.addStrLit suffix

proc addUIntLit*(b: var Builder; u: BiggestUInt; suffix: string) =
  assert suffix.len > 0
  b.withTree "suf":
    b.addUIntLit u
    b.addStrLit suffix

proc addFloatLit*(b: var Builder; u: BiggestFloat; suffix: string) =
  assert suffix.len > 0
  b.withTree "suf":
    b.addFloatLit u
    b.addStrLit suffix

proc toNif*(n, parent: PNode; c: var TranslationContext) =
  case n.kind
  of nkNone, nkEmpty:
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
    c.b.addTree("type")
    var name: PNode
    var visibility: PNode = nil
    var pragma: PNode = nil
    if n[0].kind == nkPragmaExpr:
      pragma = n[0][1]
      if n[0][0].kind == nkPostfix:
        visibility = n[0][0][0]
        name = n[0][0][1]
      else:
        name = n[0][0]
    elif n[0].kind == nkPostfix:
      visibility = n[0][0]
      name = n[0][1]
    else:
      name = n[0]

    toNif(name, n, c)

    if visibility != nil:
      c.b.addRaw " x"
    else:
      c.b.addEmpty

    toNif(n[1], n, c) # generics

    if pragma != nil:
      toNif(pragma, n, c)
    else:
      c.b.addEmpty

    for i in 2..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

  of nkTypeSection:
    for i in 0..<n.len:
      toNif(n[i], parent, c)

  of nkVarSection:
    c.section = "var"
    for i in 0..<n.len:
      toNif(n[i], parent, c)
  of nkLetSection:
    c.section = "let"
    for i in 0..<n.len:
      toNif(n[i], parent, c)
  of nkConstSection:
    c.section = "const"
    for i in 0..<n.len:
      toNif(n[i], parent, c)

  of nkFormalParams:
    c.section = "param"
    relLineInfo(n, parent, c)
    c.b.addTree("params")
    for i in 1..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()
    # put return type outside of `(params)`:
    toNif(n[0], n, c)
  of nkGenericParams:
    c.section = "typevar"
    relLineInfo(n, parent, c)
    c.b.addTree("typevars")
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

  of nkIdentDefs, nkConstDef:
    # multiple ident defs are annoying so we remove them here:
    assert c.section != ""
    let last = n.len-1
    for i in 0..last - 2:
      relLineInfo(n[i], parent, c)
      c.b.addTree(c.section)
      # flatten it further:
      var name: PNode
      var visibility: PNode = nil
      var pragma: PNode = nil
      if n[i].kind == nkPragmaExpr:
        pragma = n[i][1]
        if n[i][0].kind == nkPostfix:
          visibility = n[i][0][0]
          name = n[i][0][1]
        else:
          name = n[i][0]
      elif n[i].kind == nkPostfix:
        visibility = n[i][0]
        name = n[i][1]
      else:
        name = n[i]

      toNif(name, n[i], c) # name

      if visibility != nil:
        c.b.addRaw " x"
      else:
        c.b.addEmpty

      if pragma != nil:
        toNif(pragma, n[i], c)
      else:
        c.b.addEmpty

      toNif(n[last-1], n[i], c) # type

      toNif(n[last], n[i], c) # value
      c.b.endTree()
  of nkDo:
    relLineInfo(n, parent, c)
    c.b.addTree("paramsAndBody")
    toNif(n[paramsPos], n, c)
    toNif(n[bodyPos], n, c)
    c.b.endTree()
  of nkOfInherit:
    if n.len == 1:
      toNif(n[0], parent, c)
    else:
      relLineInfo(n, parent, c)
      c.b.addTree("par")
      for i in 0..<n.len:
        toNif(n[i], n, c)
      c.b.endTree()
  of nkOfBranch:
    relLineInfo(n, parent, c)
    c.b.addTree("of")
    c.b.addTree("set")
    for i in 0..<n.len-1:
      toNif(n[i], n, c)
    c.b.endTree()
    toNif(n[n.len-1], n, c)
    c.b.endTree()

  of nkStmtListType, nkStmtListExpr:
    relLineInfo(n, parent, c)
    c.b.addTree("expr")
    c.b.addEmpty # type information of StmtListExpr
    c.b.addTree("stmts")
    for i in 0..<n.len-1:
      toNif(n[i], n, c)
    c.b.endTree()
    if n.len > 0:
      toNif(n[n.len-1], n, c)
    else:
      c.b.addEmpty
    c.b.endTree()

  of nkProcTy:
    relLineInfo(n, parent, c)
    c.b.addTree("proctype")

    c.b.addEmpty 4 # 0: name
    # 1: export marker
    # 2: pattern
    # 3: generics

    if n.len > 0:
      toNif n[0], n, c  # 4: params
    else:
      c.b.addEmpty

    if n.len > 1:
      toNif n[1], n, c  # 5: pragmas
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
    c.b.addTree("enum")
    if n.len > 0:
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

      c.b.addTree("efld")

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
      toNif(n[i], n, c)
    c.b.endTree()

  of nkVarTuple:
    relLineInfo(n, parent, c)
    assert n[n.len-2].kind == nkEmpty
    c.b.addTree("unpackdecl")
    toNif(n[n.len-1], n, c)

    c.b.addTree("unpacktup")
    for i in 0..<n.len-2:
      c.b.addTree(c.section)
      toNif(n[i], n, c) # name

      c.b.addEmpty 4 # 1: export marker
      # 2: pragmas
      # 3: type
      # 4: value
      c.b.endTree()
    c.b.endTree()
    c.b.endTree()

  of nkForStmt:
    relLineInfo(n, parent, c)
    c.b.addTree("for")

    toNif(n[n.len-2], n, c) # iterator

    if n[0].kind == nkVarTuple:
      let v = n[0]
      c.b.addTree("unpacktup")
      for i in 0..<v.len-1: # ignores typedesc
        c.b.addTree("let")

        toNif(v[i], n, c) # name

        c.b.addEmpty 4 # export marker, pragmas, type, value
        c.b.endTree() # LetDecl
      c.b.endTree() # UnpackIntoTuple
    else:
      c.b.addTree("unpackflat")
      for i in 0..<n.len-2:
        c.b.addTree("let")

        toNif(n[i], n, c) # name

        c.b.addEmpty 4 # export marker, pragmas, type, value
        c.b.endTree() # LetDecl
      c.b.endTree() # UnpackIntoFlat

    # for-loop-body:
    toNif(n[n.len-1], n, c)
    c.b.endTree()

  of nkObjectTy:
    c.section = "fld"
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len-3:
      toNif(n[i], n, c)
    # n.len-3: pragmas: must be empty (it is deprecated anyway)
    if n.len == 0:
      # object typeclass, has no children
      discard
    else:
      if n[n.len-3].kind != nkEmpty:
        c.b.addTree "err"
        c.b.endTree()

      toNif(n[n.len-2], n, c)
      let last {.cursor.} = n[n.len-1]
      if last.kind == nkRecList:
        for child in last:
          toNif(child, n, c)
      else:
        toNif(last, n, c)
    c.b.endTree()

  of nkTupleTy, nkTupleClassTy:
    c.section = "fld"
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
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
      toNif(n, nil, c)
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
  of nkPragma:
    if c.depsEnabled:
      var hasCompile = false
      for i in 0..<n.len:
        let child = n[i]
        case child.kind
        of nkExprColonExpr:
          if child[0].kind == nkIdent and child[0].ident.s == "compile":
            if not hasCompile:
              c.deps.addTree(nodeKindTranslation(n.kind))
              hasCompile = true

            let oldLineInfoEnabled = c.lineInfoEnabled
            c.lineInfoEnabled = false
            let oldDepsEnabled = c.depsEnabled
            swap c.b, c.deps
            c.depsEnabled = false

            c.b.addTree(nodeKindTranslation(child.kind))
            if child.len == 2:
              # format (file, obj, customArgs)
              toNif(child[0], child, c)
              c.b.addTree("tup")
              case child[1].kind
              of nkStrLit:
                toNif(child[1], child, c)
                c.b.addStrLit("")
              of nkTupleConstr:
                for i in child[1]:
                  toNif(i, child[1], c)
              else:
                discard
              c.b.addStrLit("")
              c.b.endTree()
            c.b.endTree()

            c.depsEnabled = oldDepsEnabled
            swap c.b, c.deps
            c.lineInfoEnabled = oldLineInfoEnabled
        else:
          discard
      if hasCompile:
        c.deps.endTree()

    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

  else:
    relLineInfo(n, parent, c)
    c.b.addTree(nodeKindTranslation(n.kind))
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree()

proc initTranslationContext*(conf: ConfigRef; outfile: string; portablePaths, depsEnabled: bool): TranslationContext =
  result = TranslationContext(conf: conf, b: nifbuilder.open(outfile),
    portablePaths: portablePaths, depsEnabled: depsEnabled, lineInfoEnabled: true)
  if depsEnabled:
    result.deps = nifbuilder.open(outfile.changeFileExt(".deps.nif"))

proc close*(c: var TranslationContext) =
  c.b.close()
  if c.depsEnabled:
    c.deps.endTree()
    c.deps.close()

proc moduleToIr*(n: PNode; c: var TranslationContext) =
  c.b.addHeader "Nifler", "nim-parsed"
  if c.depsEnabled:
    c.deps.addHeader "Nifler", "nim-deps"
    c.deps.addTree "stmts"
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

proc parseFile*(thisfile, outfile: string; portablePaths, depsEnabled: bool) =
  let stream = llStreamOpen(AbsoluteFile thisfile, fmRead)
  if stream == nil:
    quit "cannot open file: " & thisfile
  else:
    var conf = createConf()
    var parser: Parser = default(Parser)
    openParser(parser, AbsoluteFile(thisfile), stream, newIdentCache(), conf)
    var tc = initTranslationContext(conf, outfile, portablePaths, depsEnabled)

    bench "parseAll":
      let fullTree = parseAll(parser)

    bench "moduleToIr":
      moduleToIr(fullTree, tc)
    closeParser(parser)
    tc.close()
