#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Implements the mapping from Nim sem's AST to NIF ("gear2").

when defined(nifBench):
  import std / monotimes

import std / [strutils, assertions, syncio, tables]

import compiler / [
  ast, options, pathutils, renderer, lineinfos,
  parser, llstream, idents, msgs, modulegraphs]

import ".." / lib / [nifbuilder, nifindexes, nifstreams, nifcursors, bitabs, lineinfos, nifreader,
  symparser]
import modnames, enum2nif

type
  WContext = object # writer context
    conf: ConfigRef
    section: string
    b: Builder
    toSuffix: Table[string, string]

  LoadState = enum
    Created, Loaded, Completed
  LoadedSym = object
    state: LoadState
    sym: PSym
  LoadedType = object
    state: LoadState
    typ: PType

  RModule* = object
    index: NifIndex
    public: Table[string, NifIndexEntry]
    private: Table[string, NifIndexEntry]
    s: Stream
  RContext* = object
    owner: PSym
    idgen: IdGenerator
    conf: ConfigRef
    graph: ModuleGraph
    identCache: IdentCache
    symKind: TSymKind
    lastLit: FileId
    lastFile: FileIndex
    syms: Table[SymId, LoadedSym]
    types: Table[SymId, LoadedType]
    magics: Table[TMagic, LoadedSym]
    thisModule: string
    modules: Table[string, RModule]

proc createRContext*(g: ModuleGraph; identCache: IdentCache; module: PSym): RContext =
  result = RContext(owner: module, idgen: idGeneratorFromModule(module), conf: g.config, graph: g,
    identCache: identCache)

proc closeAll*(r: var RContext) =
  for _, m in mpairs r.modules:
    close m.s

proc openNifModule*(r: var RContext; modname: string) =
  if r.modules.hasKey(modname): return
  if r.thisModule.len == 0: r.thisModule = modname
  let filename = r.conf.nimcacheDir & "/" & modname & ".nif"
  var public = initTable[string, NifIndexEntry]()
  var private = initTable[string, NifIndexEntry]()
  var stream = nifstreams.open(filename)
  let embedded = readEmbeddedIndex(stream)
  if embedded.len > 0:
    for k, v in embedded:
      if v.vis == Exported:
        public[k] = v
      else:
        private[k] = v
  let indexName = r.confi.nimcacheDir & "/" & modname & ".idx.nif"
  let index = readIndex(indexName)
  r.modules[modname] = RModule(index: index, public: public, private: private, s: stream)

proc toFileIndexCached(c: var RContext; f: FileId): FileIndex =
  if f == FileId(0):
    result = InvalidFileIdx
  elif c.lastLit == f:
    result = c.lastFile
  else:
    result = msgs.fileInfoIdx(c.conf, AbsoluteFile pool.files[f])
    c.lastLit = f
    c.lastFile = result

proc translateLineInfo(c: var RContext; x: PackedLineInfo): TLineInfo =
  let (fileId, line, col) = unpack(pool.man, x)
  result = TLineInfo(line: line.uint16, col: col.int16,
            fileIndex: toFileIndexCached(c, fileId))

proc absLineInfo(i: TLineInfo; c: var WContext) =
  c.b.addLineInfo int32(i.col), int32(i.line), toFullPath(c.conf, i.fileIndex)

proc relLineInfo(n, parent: PNode; c: var WContext;
                 emitSpace = false) =
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

proc symToNif(s: PSym; c: var WContext; isDef = false) =
  var m = s.name.s & '.' & $s.disamb
  let ow = s.skipGenericOwner()
  if ow.kind == skModule:
    m.add '.'
    let fp = toFullPath(c.conf, FileIndex ow.position)
    var suf = c.toSuffix.getOrDefault(fp)
    if suf.len == 0:
      suf = moduleSuffix(fp)
      m.add suf
      c.toSuffix[fp] = ensureMove suf
    else:
      m.add suf
  if isDef:
    c.b.addSymbolDef m
  else:
    c.b.addSymbol m

proc toNif*(n, parent: PNode; c: var WContext)
proc toNif*(t: PType; parent: PNode; c: var WContext)

proc symToNif(n: PNode; parent: PNode; c: var WContext; isDef = false) =
  if not isDef and n.typ != n.sym.typ:
    c.b.withTree "htype":
      toNif n.typ, parent, c
      symToNif n.sym, c, isDef
  else:
    symToNif n.sym, c, isDef

include typebridge

proc toNifDecl(n, parent: PNode; c: var WContext) =
  if n.kind == nkSym:
    relLineInfo(n, parent, c)
    symToNif(n, parent, c, true)
  else:
    toNif n, parent, c

proc magicCall(m: TMagic; n: PNode; c: var WContext) =
  c.b.addTree(toNifTag(m))
  for i in 1..<n.len:
    toNif(n[i], n, c)
  c.b.endTree

proc writeNodeFlags(b: var Builder; flags: set[TNodeFlag]) {.inline.} =
  # we know nodes can have been sem'checked:
  writeFlags b, flags - {nfSem}, "nf"

proc toNifPragmas(n, name, parent: PNode; c: var WContext) =
  var b2 = nifbuilder.open(30)
  b2.withTree "pragmas":
    if name.kind == nkSym:
      let flags = name.sym.flags
      if sfImportc in flags:
        b2.withTree "importc":
          b2.addIdent name.sym.loc.snippet
      elif sfExportc in flags:
        b2.withTree "exportc":
          b2.addIdent name.sym.loc.snippet
      b2.writeFlags flags - {sfImportc, sfExportc}, "sf"
    elif n != nil:
      for ch in n:
        toNif ch, n, c
  let s = b2.extract()
  if s.len > len("(pragmas)"):
    c.b.addRaw s
  else:
    c.b.addEmpty

proc addIntLit(b: var Builder; i: BiggestInt; suffix: string) =
  b.withTree "suf":
    b.addIntLit i
    b.addStrLit suffix

proc addFloatLit(b: var Builder; i: BiggestFloat; suffix: string) =
  b.withTree "suf":
    b.addFloatLit i
    b.addStrLit suffix

type
  ProcDecl = object
    name, ex, pattern, generics, params, pragmas, exc, body: PNode

proc asProcDecl(n: PNode): ProcDecl =
  result = ProcDecl(
    name: n[0], pattern: n[1], generics: n[2], params: n[3],
    pragmas: n[4], exc: n[5], body: n[6])

proc procToNif*(s: PSym; parent: PNode; c: var WContext) =
  # name and export marker have already been produced here.
  for i in 1..bodyPos:
    assert s.ast[i] != nil
    toNif s.ast[i], parent, c

proc toNif*(n, parent: PNode; c: var WContext) =
  case n.kind
  of nkNone:
    c.b.addKeyw toNifTag(n.kind)
    relLineInfo(n, parent, c)
  of nkEmpty:
    c.b.addEmpty 1
  of nkSym:
    relLineInfo(n, parent, c)
    symToNif(n, parent, c)
  of nkCommand, nkCall, nkCallStrLit, nkInfix, nkPrefix, nkPostfix, nkHiddenCallConv:
    relLineInfo(n, parent, c)
    if n.len > 0 and n[0].kind == nkSym and n[0].sym.magic != mNone:
      magicCall n[0].sym.magic, n, c
    else:
      c.b.addTree(toNifTag(n.kind))
      c.b.writeNodeFlags n.flags
      for i in 0..<n.len:
        toNif(n[i], n, c)
      c.b.endTree
  of nkNilLit:
    c.b.addKeyw "nil"
    relLineInfo(n, parent, c)
  of nkStrLit:
    c.b.addStrLit n.strVal
    relLineInfo(n, parent, c)
  of nkRStrLit:
    c.b.addStrLit n.strVal, "R"
    relLineInfo(n, parent, c)
  of nkTripleStrLit:
    c.b.addStrLit n.strVal, "T"
    relLineInfo(n, parent, c)
  of nkCharLit:
    c.b.addCharLit char(n.intVal)
    relLineInfo(n, parent, c)
  of nkIntLit:
    c.b.addIntLit n.intVal
    relLineInfo(n, parent, c, true)
  of nkInt8Lit:
    c.b.addIntLit n.intVal, "i8"
    relLineInfo(n, parent, c, true)
  of nkInt16Lit:
    c.b.addIntLit n.intVal, "i16"
    relLineInfo(n, parent, c, true)
  of nkInt32Lit:
    c.b.addIntLit n.intVal, "i32"
    relLineInfo(n, parent, c, true)
  of nkInt64Lit:
    c.b.addIntLit n.intVal, "i64"
    relLineInfo(n, parent, c, true)
  of nkUIntLit:
    c.b.addUIntLit cast[BiggestUInt](n.intVal)
    relLineInfo(n, parent, c, true)
  of nkUInt8Lit:
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u8"
    relLineInfo(n, parent, c, true)
  of nkUInt16Lit:
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u16"
    relLineInfo(n, parent, c, true)
  of nkUInt32Lit:
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u32"
    relLineInfo(n, parent, c, true)
  of nkUInt64Lit:
    c.b.addUIntLit cast[BiggestUInt](n.intVal), "u64"
    relLineInfo(n, parent, c, true)
  of nkFloatLit:
    c.b.addFloatLit n.floatVal
    relLineInfo(n, parent, c, true)
  of nkFloat32Lit:
    c.b.addFloatLit n.floatVal, "f32"
    relLineInfo(n, parent, c, true)
  of nkFloat64Lit:
    c.b.addFloatLit n.floatVal, "f64"
    relLineInfo(n, parent, c, true)
  of nkFloat128Lit:
    c.b.addFloatLit n.floatVal, "f128"
    relLineInfo(n, parent, c, true)
  of nkIdent:
    c.b.addIdent n.ident.s
    relLineInfo(n, parent, c, true)
  of nkTypeDef:
    c.b.addTree("type")
    relLineInfo(n, parent, c)
    var name: PNode
    var visibility: PNode
    var pragma: PNode
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

    toNifDecl(name, n, c)

    if visibility != nil:
      c.b.addIdent "x"
    else:
      c.b.addEmpty

    toNifPragmas(pragma, name, n, c)

    for i in 1..<n.len-1:
      toNif(n[i], n, c)
    let last = n[n.len-1]
    if name.kind == nkSym:
      # ^ if this was sem'checked
      if last.kind == nkEmpty and name.sym.typ != nil:
        toNif name.sym.typ, n, c
      elif name.sym.typ != nil and name.sym.typ.kind in {tyEnum, tyObject} and name.sym.typ.n != nil:
        toNif name.sym.typ.n, n, c
      else:
        toNif(last, n, c)
    else:
      toNif(last, n, c)

    c.b.endTree

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
    c.b.addTree(toNifTag(n.kind))
    relLineInfo(n, parent, c)
    c.b.writeNodeFlags n.flags
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree
  of nkGenericParams:
    c.section = "typevar"
    c.b.addTree(toNifTag(n.kind))
    relLineInfo(n, parent, c)
    c.b.writeNodeFlags n.flags
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree

  of nkIdentDefs, nkConstDef:
    # multiple ident defs are annoying so we remove them here:
    assert c.section != ""
    let last = n.len-1
    for i in 0..last - 2:
      c.b.addTree(c.section)
      relLineInfo(n[i], parent, c)
      # flatten it further:
      var name: PNode
      var visibility: PNode
      var pragma: PNode
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

      toNifDecl(name, n[i], c) # name

      if visibility != nil:
        c.b.addIdent "x"
      else:
        c.b.addEmpty

      toNifPragmas(pragma, name, n[i], c)

      if name.kind == nkSym:
        toNif(name.sym.typ, n[i], c)
      else:
        toNif(n[last-1], n[i], c) # type
      toNif(n[last], n[i], c) # value
      c.b.endTree
  of nkDo:
    c.b.addTree("do")
    relLineInfo(n, parent, c)

    toNif(n[paramsPos], n, c)
    toNif(n[bodyPos], n, c)
    c.b.endTree
  of nkOfInherit:
    if n.len == 1:
      toNif(n[0], parent, c)
    else:
      c.b.addTree("par")
      relLineInfo(n, parent, c)
      for i in 0..<n.len:
        toNif(n[i], n, c)
      c.b.endTree
  of nkOfBranch:
    c.b.addTree(toNifTag(n.kind))
    c.b.addTree("sconstr")
    relLineInfo(n, parent, c)
    for i in 0..<n.len-1:
      toNif(n[i], n, c)
    c.b.endTree

    toNif(n[n.len-1], n, c)
    c.b.endTree

  of nkStmtListType, nkStmtListExpr:
    c.b.addTree(toNifTag(n.kind))
    relLineInfo(n, parent, c)
    c.b.writeNodeFlags n.flags

    c.b.addEmpty # type information of StmtListExpr
    c.b.addTree(toNifTag(n.kind))
    for i in 0..<n.len-1:
      toNif(n[i], n, c)
    c.b.endTree
    if n.len > 0:
      toNif(n[n.len-1], n, c)
    else:
      c.b.addEmpty
    c.b.endTree

  of nkProcTy:
    c.b.addTree(toNifTag(n.kind))
    c.b.addEmpty 4 # 0: name

    relLineInfo(n, parent, c)
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
    c.b.endTree

  of nkEnumTy:
    # EnumField
    #   SymDef "x"
    #   Empty      # export marker (always empty)
    #   Empty      # pragmas
    #   EnumType
    #   (Integer value, "string value")
    relLineInfo(n, parent, c)
    let start = if n.len > 0 and n[0].kind == nkEmpty: 1 else: 0
    c.b.addTree(if start == 0: "enum" else: toNifTag(n.kind))
    for i in start..<n.len:
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

      c.b.addTree("efld")

      relLineInfo(it, n, c)
      toNifDecl name, it, c
      c.b.addEmpty # export marker
      toNifPragmas(pragma, name, it, c)
      c.b.addEmpty # type (filled by sema)
      if val == nil:
        c.b.addEmpty
      else:
        toNif(val, it, c)
      c.b.endTree
    c.b.endTree

  of nkProcDef, nkFuncDef, nkConverterDef, nkMacroDef, nkTemplateDef, nkIteratorDef, nkMethodDef:
    c.b.addTree(toNifTag(n.kind))
    relLineInfo(n, parent, c)
    c.b.writeNodeFlags n.flags

    var name: PNode
    var visibility: PNode = nil
    if n[0].kind == nkPostfix:
      visibility = n[0][0]
      name = n[0][1]
    else:
      name = n[0]

    toNifDecl(name, n, c)
    if name.kind == nkSym and sfExported in name.sym.flags:
      c.b.addRaw "x"
    elif visibility != nil:
      c.b.addRaw "x"
    else:
      c.b.addEmpty

    if name.kind == nkSym and name.sym.ast != nil:
      procToNif name.sym, parent, c
    else:
      for i in 1..<n.len:
        toNif(n[i], n, c)
    c.b.endTree

  of nkVarTuple:
    relLineInfo(n, parent, c)
    assert n[n.len-2].kind == nkEmpty
    c.b.addTree(toNifTag(n.kind))
    c.b.writeNodeFlags n.flags
    toNif(n[n.len-1], n, c)
    c.b.addTree("unpacktuple")
    for i in 0..<n.len-2:
      c.b.addTree(c.section)
      toNifDecl(n[i], n, c) # name
      c.b.addEmpty 4 # 1: export marker
      # 2: pragmas
      # 3: type
      # 4: value
      c.b.endTree
    c.b.endTree
    c.b.endTree

  of nkForStmt:
    c.b.addTree(toNifTag(n.kind))
    relLineInfo(n, parent, c)
    c.b.writeNodeFlags n.flags
    toNif(n[n.len-2], n, c) # iterator
    if n[0].kind == nkVarTuple:
      let v = n[0]
      c.b.addTree("unpacktuple")
      for i in 0..<v.len-1:
        c.b.addTree("let")
        toNifDecl(v[i], n, c) # name
        c.b.addEmpty 4 # export marker, pragmas, type, value
        c.b.endTree # LetDecl
      c.b.endTree # UnpackIntoTuple
    else:
      c.b.addTree("unpackflat")
      for i in 0..<n.len-2:
        c.b.addTree("let")
        toNifDecl(n[i], n, c) # name
        c.b.addEmpty 4 # export marker, pragmas, type, value
        c.b.endTree # LetDecl
      c.b.endTree # UnpackIntoFlat

    # for-loop-body:
    toNif(n[n.len-1], n, c)
    c.b.endTree

  of nkObjectTy:
    c.section = "fld"
    c.b.addTree(toNifTag(n.kind))
    relLineInfo(n, parent, c)
    c.b.writeNodeFlags n.flags
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree
  else:
    c.b.addTree(toNifTag(n.kind))
    relLineInfo(n, parent, c)
    c.b.writeNodeFlags n.flags
    for i in 0..<n.len:
      toNif(n[i], n, c)
    c.b.endTree

proc initTranslationContext*(conf: ConfigRef): WContext =
  result = WContext(conf: conf, b: nifbuilder.open(500, true))

proc moduleToIr*(n: PNode; c: var WContext) =
  #c.b = nifbuilder.open(100)
  c.b.addHeader "gear2", "nim-sem"
  toNif(n, nil, c)

proc toNif*(conf: ConfigRef; n: PNode; filename: string) =
  var w = initTranslationContext(conf)
  moduleToIr(n, w)
  writeFile filename, w.b.extract()
  nifindexes.createIndex filename

proc createConf(): ConfigRef =
  result = newConfigRef()
  #result.notes.excl hintLineTooLong
  result.errorMax = 1000

proc loadInterface*(r: var RContext; module: PSym; suffix: string) =
  let m = addr(r.modules[suffix])
  for k, v in m.public:
    #var isGlobal = false
    #let asNimName = extractBasename(k, isGlobal)
    #echo "LOADING ", k
    var stub = loadSym(m[], k, v, r)
    stub.setOwner module
    r.graph.strTableAdds(module, stub)

proc openSystem*(r: var RContext; modname, suffix: string) =
  openNifModule r, suffix
  #let strIdx = r.modules[modname].index.getOrDefault()
  r.graph.sysTypes[tyString] = loadType("string.0." & suffix, r)

  assert r.graph.sysTypes[tyString].kind == tyString

template bench(task, body) =
  when defined(nifBench):
    let t0 = getMonoTime()
    body
    echo task, " TOOK ", getMonoTime() - t0
  else:
    body
