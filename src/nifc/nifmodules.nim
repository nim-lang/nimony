#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## NIF set-of-modules handling.

include ".." / lib / nifprelude
import std / [assertions, tables, strutils]
import symparser, nifindexes, nifc_model, noptions

type
  NifModule = ref object
    stream: nifstreams.Stream
    index: Table[string, NifIndexEntry]  # Simple embedded index for offsets

  Definition* = object
    pos*: Cursor # points into MainModule.src
    kind*: NifcSym
    extern*: StrId
    buf: TokenBuf     # can be empty for symbols that are in the main module

  NifProgram* = object # a NIF program is a set of NIF modules
    mods: Table[string, NifModule]
    scheme: SplittedModulePath

proc setupNifProgram*(scheme: sink SplittedModulePath): NifProgram =
  result = NifProgram(scheme: scheme, mods: initTable[string, NifModule]())

proc lookupDeclaration(c: var NifProgram; s: SplittedSymName): TokenBuf =
  if s.module == "":
    raiseAssert "Cannot lookup declaration without module name: " & s.name
  else:
    var m: NifModule
    if not c.mods.hasKey(s.module):
      c.scheme.name = s.module
      var stream = nifstreams.open($c.scheme)
      let index = readEmbeddedIndex(stream)
      m = NifModule(stream: stream, index: index)
      c.mods[s.module] = m
    else:
      m = c.mods[s.module]

    let entry = m.index.getOrDefault($s)
    if entry.offset == 0:
      raiseAssert "Symbol not found in NIF module: " & $s
    else:
      result = createTokenBuf()
      m.stream.r.jumpTo entry.offset
      nifcursors.parse(m.stream, result, entry.info)

proc externName*(s: SymId; n: Cursor): StrId =
  let nn = n.firstSon
  if nn.kind == StringLit:
    result = nn.litId
  else:
    var base = pool.syms[s]
    extractBasename base
    result = pool.strings.getOrIncl(base)

proc extractExtern(n: var Cursor; pragmasAt: int): StrId =
  result = StrId(0)
  inc n
  if n.kind != SymbolDef:
    raiseAssert "Expected SymbolDef after toplevel declaration"
  else:
    let symId = n.symId
    inc n
    for i in 1..<pragmasAt: skip n
    if n.substructureKind == PragmasU:
      inc n
      while n.kind != ParRi:
        if n.pragmaKind in {ImportcP, ImportcppP, ExportcP}:
          result = externName(symId, n)
        skip n
      inc n
    elif n.kind == DotToken:
      discard "ok"
    else:
      raiseAssert "pragmas not at the correct position"
    while n.kind != ParRi:
      skip n
    inc n

type
  TypeScope* {.acyclic.} = ref object
    locals*: Table[SymId, Cursor]
    parent*: TypeScope

  MainModule* = object
    src*: TokenBuf
    types*: seq[Cursor] # points into MainModule.src
    filename*: string
    config*: ConfigRef
    mem*: seq[TokenBuf] # for intermediate results such as computed types
    builtinTypes*: Table[string, Cursor]
    current*: TypeScope
    defs: Table[SymId, Definition]
    prog: NifProgram

proc getDecl*(c: var MainModule; s: SymId): ptr Definition =
  if not c.defs.hasKey(s):
    var buf = lookupDeclaration(c.prog, splitSymName(pool.syms[s]))
    var pos = beginRead(buf)
    var n = pos.firstSon
    if n.kind == SymbolDef:
      let sk = n.symKind
      var extern = StrId(0)
      case sk
      of TypeY:
        extern = extractExtern(n, 1)
      of ProcY:
        extern = extractExtern(n, 3)
      of VarY, ConstY, GvarY, TvarY:
        extern = extractExtern(n, 1)
      else: discard
      c.defs[s] = Definition(pos: pos, kind: sk, extern: extern, buf: ensureMove(buf))
    else:
      raiseAssert "Expected SymbolDef after toplevel declaration"
  result = addr c.defs[s]

proc registerLocal*(c: var MainModule; s: SymId; typ: Cursor) =
  c.current.locals[s] = typ

proc openScope*(c: var MainModule) =
  c.current = TypeScope(locals: initTable[SymId, Cursor](), parent: c.current)

proc closeScope*(c: var MainModule) =
  c.current = c.current.parent

proc parse*(r: var Reader; m: var MainModule; parentInfo: PackedLineInfo): bool =
  var t = default(ExpandedToken)
  next(r, t)
  var currentInfo = parentInfo
  if t.filename.len == 0:
    # relative file position
    if t.pos.line != 0 or t.pos.col != 0:
      let rawInfo = unpack(pool.man, parentInfo)
      if rawInfo.file.isValid:
        currentInfo = pack(pool.man, rawInfo.file, rawInfo.line+t.pos.line, rawInfo.col+t.pos.col)
  else:
    # absolute file position:
    let fileId = pool.files.getOrIncl(decodeFilename t)
    currentInfo = pack(pool.man, fileId, t.pos.line, t.pos.col)

  result = true
  case t.tk
  of EofToken, ParRi:
    result = false
  of ParLe:
    let tag = pool.tags.getOrIncl(r.decodeStr t)
    copyInto(m.src, tag, currentInfo):
      while true:
        let progress = parse(r, m, currentInfo)
        if not progress: break
  of UnknownToken:
    copyInto m.src, ErrT, currentInfo:
      m.src.addStrLit r.decodeStr(t), currentInfo
  of DotToken:
    m.src.addDotToken()
  of Ident:
    m.src.addIdent r.decodeStr(t), currentInfo
  of Symbol:
    m.src.add symToken(pool.syms.getOrIncl(r.decodeStr t), currentInfo)
  of SymbolDef:
    m.src.add symdefToken(pool.syms.getOrIncl(r.decodeStr t), currentInfo)
  of StringLit:
    m.src.addStrLit r.decodeStr(t), currentInfo
  of CharLit:
    m.src.add charToken(decodeChar(t), currentInfo)
  of IntLit:
    m.src.addIntLit parseBiggestInt(r.decodeStr t), currentInfo
  of UIntLit:
    m.src.addUIntLit parseBiggestUInt(r.decodeStr t), currentInfo
  of FloatLit:
    m.src.add floatToken(pool.floats.getOrIncl(parseFloat(r.decodeStr t)), currentInfo)

proc processToplevelDecl(m: var MainModule; n: var Cursor; kind: NifcSym; pragmasAt: int) =
  let decl = n
  let s = decl.firstSon.symId
  let extern = extractExtern(n, pragmasAt)
  m.defs[s] = Definition(pos: decl, kind: kind, extern: extern)

proc detectToplevelDecls(m: var MainModule) =
  var n = cursorAt(m.src, 0)
  var nested = 0
  while true:
    case n.kind
    of ParLe:
      case n.stmtKind
      of TypeS:
        m.types.add n
        processToplevelDecl(m, n, TypeY, 1)
      of ProcS:
        processToplevelDecl(m, n, ProcY, 3)
      of VarS, ConstS, GvarS, TvarS:
        processToplevelDecl(m, n, n.symKind, 1)
      else:
        inc n
        inc nested
    of ParRi:
      assert nested > 0
      dec nested
      inc n
    else:
      inc n
    if nested == 0: break

proc parse(r: var Reader; filename: string): MainModule =
  # empirically, (size div 7) is a good estimate for the number of nodes
  # in the file:
  let nodeCount = r.fileSize div 7
  result = MainModule(src: createTokenBuf(nodeCount), prog: setupNifProgram(splitModulePath(filename)))
  discard parse(r, result, NoLineInfo)
  freeze(result.src)
  detectToplevelDecls(result)

proc load*(filename: string): MainModule =
  var r = nifreader.open(filename)
  case nifreader.processDirectives(r)
  of Success:
    discard
  of WrongHeader:
    quit "nif files must start with Version directive"
  of WrongMeta:
    quit "the format of meta information is wrong!"
  result = parse(r, filename)
  result.filename = filename
  r.close
