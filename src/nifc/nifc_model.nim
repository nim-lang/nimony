#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parse NIF into a packed tree representation.

import std / [hashes, tables, assertions, strutils]
include "../lib" / nifprelude
import noptions
import ".." / models / [nifc_tags, callconv_tags, tags]
export nifc_tags, callconv_tags

type
  Definition* = object
    pos*: int
    kind*: NifcSym

  TypeScope* {.acyclic.} = ref object
    locals*: Table[SymId, Cursor]
    parent*: TypeScope

  Module* = object
    src*: TokenBuf
    types*: seq[int]
    defs*: Table[SymId, Definition]
    filename*: string
    config*: ConfigRef
    mem*: seq[TokenBuf] # for intermediate results such as computed types
    builtinTypes*: Table[string, Cursor]
    current*: TypeScope

proc bug*(msg: string) {.noreturn.} =
  when defined(debug):
    writeStackTrace()
  quit "BUG: " & msg

proc registerLocal*(c: var Module; s: SymId; typ: Cursor) =
  c.current.locals[s] = typ

proc openScope*(c: var Module) =
  c.current = TypeScope(locals: initTable[SymId, Cursor](), parent: c.current)

proc closeScope*(c: var Module) =
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

proc parse*(r: var Reader; m: var Module; parentInfo: PackedLineInfo): bool =
  let t = next(r)
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
    let tag = pool.tags.getOrIncl(decodeStr t)
    if cast[TagEnum](tag) == TypeTagId:
      m.types.add m.src.len
    copyInto(m.src, tag, currentInfo):
      while true:
        let progress = parse(r, m, currentInfo)
        if not progress: break
  of UnknownToken:
    copyInto m.src, ErrT, currentInfo:
      m.src.addStrLit decodeStr(t), currentInfo
  of DotToken:
    m.src.addDotToken()
  of Ident:
    m.src.addIdent decodeStr(t), currentInfo
  of Symbol:
    m.src.add symToken(pool.syms.getOrIncl(decodeStr t), currentInfo)
  of SymbolDef:
    # Remember where to find this symbol:
    let litId = pool.syms.getOrIncl(decodeStr t)
    let pos = m.src.len - 1
    let n = cursorAt(m.src, pos)
    m.defs[litId] = Definition(pos: pos, kind: n.symKind)
    endRead(m.src)
    m.src.add symdefToken(litId, currentInfo)
  of StringLit:
    m.src.addStrLit decodeStr(t), currentInfo
  of CharLit:
    m.src.add charToken(decodeChar(t), currentInfo)
  of IntLit:
    m.src.addIntLit parseBiggestInt(decodeStr t), currentInfo
  of UIntLit:
    m.src.addUIntLit parseBiggestUInt(decodeStr t), currentInfo
  of FloatLit:
    m.src.add floatToken(pool.floats.getOrIncl(parseFloat(decodeStr t)), currentInfo)

proc parse*(r: var Reader): Module =
  # empirically, (size div 7) is a good estimate for the number of nodes
  # in the file:
  let nodeCount = r.fileSize div 7
  result = Module(src: createTokenBuf(nodeCount))
  discard parse(r, result, NoLineInfo)
  freeze(result.src)

proc load*(filename: string): Module =
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

proc firstSon*(n: Cursor): Cursor {.inline.} =
  result = n
  inc result

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
