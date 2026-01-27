#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parse NIF into a packed tree representation.

import std / [hashes, tables, assertions, strutils]
include "../lib" / nifprelude
import noptions, nifmodules, symparser
import ".." / models / [nifc_tags, callconv_tags, tags]
export nifc_tags, callconv_tags

type
  Definition* = object
    pos*: Cursor # points into MainModule.src
    kind*: NifcSym
    extern*: StrId

  TypeScope* {.acyclic.} = ref object
    locals*: Table[SymId, Cursor]
    parent*: TypeScope

  MainModule* = object
    src*: TokenBuf
    types*: seq[Cursor] # points into MainModule.src
    defs*: Table[SymId, Definition]
    filename*: string
    config*: ConfigRef
    mem*: seq[TokenBuf] # for intermediate results such as computed types
    builtinTypes*: Table[string, Cursor]
    current*: TypeScope
    prog: NifProgram

proc bug*(msg: string) {.noreturn.} =
  when defined(debug):
    writeStackTrace()
  quit "BUG: " & msg

proc registerLocal*(c: var MainModule; s: SymId; typ: Cursor) =
  c.current.locals[s] = typ

proc openScope*(c: var MainModule) =
  c.current = TypeScope(locals: initTable[SymId, Cursor](), parent: c.current)

proc closeScope*(c: var MainModule) =
  c.current = c.current.parent

proc skipParRi*(n: var Cursor) =
  # XXX: Give NIFC some better error reporting.
  if n.kind == ParRi:
    inc n
  else:
    when defined(debug):
      writeStackTrace()
    quit "expected ')', but got: " & $n.kind

template tagEnum*(c: Cursor): TagEnum = cast[TagEnum](tag(c))

proc stmtKind*(c: Cursor): NifcStmt {.inline.} =
  if c.kind == ParLe and rawTagIsNifcStmt(tagEnum(c)):
    result = cast[NifcStmt](tagEnum(c))
  else:
    result = NoStmt

proc pragmaKind*(c: Cursor): NifcPragma {.inline.} =
  if c.kind == ParLe:
    let e = tagEnum(c)
    if rawTagIsNifcPragma(e):
      result = cast[NifcPragma](e)
    else:
      result = NoPragma
  elif c.kind == Ident:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if rawTagIsNifcPragma(cast[TagEnum](tagId)):
      result = cast[NifcPragma](tagId)
    else:
      result = NoPragma
  else:
    result = NoPragma

proc substructureKind*(c: Cursor): NifcOther {.inline.} =
  if c.kind == ParLe and rawTagIsNifcOther(tagEnum(c)):
    result = cast[NifcOther](tag(c))
  else:
    result = NoSub

proc typeKind*(c: Cursor): NifcType {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNifcType(tagEnum(c)):
      result = cast[NifcType](tag(c))
    else:
      result = NoType
  elif c.kind == DotToken:
    result = VoidT
  else:
    result = NoType

proc typeQual*(c: Cursor): NifcTypeQualifier {.inline.} =
  if c.kind == ParLe and rawTagIsNifcTypeQualifier(tagEnum(c)):
    result = cast[NifcTypeQualifier](tag(c))
  else:
    result = NoQualifier

proc callConvKind*(c: Cursor): CallConv {.inline.} =
  if c.kind == ParLe:
    if rawTagIsCallConv(tagEnum(c)):
      result = cast[CallConv](tag(c))
    else:
      result = NoCallConv
  elif c.kind == Ident:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if rawTagIsCallConv(cast[TagEnum](tagId)):
      result = cast[CallConv](tagId)
    else:
      result = NoCallConv
  else:
    result = NoCallConv

proc exprKind*(c: Cursor): NifcExpr {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNifcExpr(tagEnum(c)):
      result = cast[NifcExpr](tag(c))
    else:
      result = NoExpr
  else:
    result = NoExpr

proc symKind*(c: Cursor): NifcSym {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNifcSym(tagEnum(c)):
      result = cast[NifcSym](tagEnum(c))
    else:
      result = NoSym
  else:
    result = NoSym

proc tracebackTypeC*(n: Cursor): Cursor =
  assert n.typeKind in {ObjectT, UnionT, ArrayT, EnumT, ProctypeT}
  result = n
  while result.stmtKind != TypeS:
    unsafeDec result

proc firstSon*(n: Cursor): Cursor {.inline.} =
  result = n
  inc result

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

proc externName*(s: SymId; n: Cursor): StrId =
  let nn = n.firstSon
  if nn.kind == StringLit:
    result = nn.litId
  else:
    var base = pool.syms[s]
    extractBasename base
    result = pool.strings.getOrIncl(base)

proc processToplevelDecl(m: var MainModule; n: var Cursor; kind: NifcSym; pragmasAt: int) =
  let decl = n
  inc n
  if n.kind != SymbolDef:
    raiseAssert "Expected SymbolDef after toplevel declaration"
  else:
    let symId = n.symId
    inc n
    for i in 1..<pragmasAt: skip n
    var extern = StrId(0)
    if n.substructureKind == PragmasU:
      inc n
      while n.kind != ParRi:
        if n.pragmaKind in {ImportcP, ImportcppP, ExportcP}:
          extern = externName(symId, n)
        skip n
      inc n
    elif n.kind == DotToken:
      discard "ok"
    else:
      raiseAssert "pragmas not at the correct position"
    while n.kind != ParRi:
      skip n
    inc n
    m.defs[symId] = Definition(pos: decl, kind: kind, extern: extern)

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

proc parse*(r: var Reader): MainModule =
  # empirically, (size div 7) is a good estimate for the number of nodes
  # in the file:
  let nodeCount = r.fileSize div 7
  result = MainModule(src: createTokenBuf(nodeCount))
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
  result = parse(r)
  result.filename = filename
  r.close

proc parLeToken*(t: NifcType; info = NoLineInfo): PackedToken =
  result = parLeToken(TagId(t), info)

# Read helpers:

template elementType*(n: Cursor): Cursor = n.firstSon

type
  TypeDecl* = object
    name*, pragmas*, body*: Cursor

proc asTypeDeclImpl(n: var Cursor): TypeDecl =
  assert n.stmtKind == TypeS
  inc n
  result = TypeDecl(name: n)
  skip n
  result.pragmas = n
  skip n
  result.body = n

proc asTypeDecl*(n: Cursor): TypeDecl =
  var n = n
  asTypeDeclImpl(n)

proc takeTypeDecl*(n: var Cursor): TypeDecl =
  result = asTypeDeclImpl(n)
  skip n # skip body
  skipParRi n

type
  FieldDecl* = object
    name*, pragmas*, typ*: Cursor

proc takeFieldDecl*(n: var Cursor): FieldDecl =
  assert n.substructureKind == FldU
  inc n
  result = FieldDecl(name: n)
  skip n
  result.pragmas = n
  skip n
  result.typ = n
  skip n
  skipParRi n

type
  ParamDecl* = object
    name*, pragmas*, typ*: Cursor

proc takeParamDecl*(n: var Cursor): ParamDecl =
  assert n.substructureKind == ParamU
  inc n
  result = ParamDecl(name: n)
  skip n
  result.pragmas = n
  skip n
  result.typ = n
  skip n
  skipParRi n


type
  ProcType* = object
    params*, returnType*, pragmas*: Cursor

proc takeProcType*(n: var Cursor): ProcType =
  if n.typeKind == ParamsT:
    discard
  else:
    assert n.stmtKind == ProcS or n.typeKind == ProctypeT
    inc n # into (proctype ...)
    skip n # skip the name
  assert n.typeKind == ParamsT or n.kind == DotToken
  result = ProcType(params: n)
  skip n
  result.returnType = n
  skip n
  result.pragmas = n
  skip n
  if n.kind == DotToken:
    inc n
  skipParRi n

type
  ProcDecl* = object
    name*, params*, returnType*, pragmas*, body*: Cursor

proc takeProcDecl*(n: var Cursor): ProcDecl =
  assert n.stmtKind == ProcS
  inc n
  result = ProcDecl(name: n)
  skip n
  result.params = n
  skip n
  result.returnType = n
  skip n
  result.pragmas = n
  skip n
  result.body = n
  skip n
  skipParRi n

type
  VarDecl* = object
    name*, pragmas*, typ*, value*: Cursor

proc takeVarDecl*(n: var Cursor): VarDecl =
  assert n.stmtKind in {GvarS, TvarS, VarS, ConstS}
  inc n
  result = VarDecl(name: n)
  skip n
  result.pragmas = n
  skip n
  result.typ = n
  skip n
  result.value = n
  skip n
  skipParRi n
