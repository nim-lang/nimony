#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parse NIF into a packed tree representation.

import std / [hashes, tables, assertions]
include "../lib" / nifprelude
import noptions
import ".." / models / [nifc_tags, callconv_tags, tags]

proc stmtKind*(c: Cursor): NifcStmt {.inline.} =
  if c.kind == ParLe and rawTagIsNifcStmt(tag(c).uint32):
    result = cast[NifcStmt](tag(c))
  else:
    result = NoStmt

proc pragmaKind*(c: Cursor): NifcPragma {.inline.} =
  if c.kind == ParLe:
    let tagId = c.tagId.uint32
    if rawTagIsNifcPragma(tagId):
      result = cast[NifcPragma](tagId)
    else:
      result = NoPragma
  elif c.kind == Ident:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if rawTagIsNifcPragma(tagId.uint32):
      result = cast[NifcPragma](tagId)
    else:
      result = NoPragma
  else:
    result = NoPragma

proc substructureKind*(c: Cursor): NifcOther {.inline.} =
  if c.kind == ParLe and rawTagIsNifcOther(tag(c).uint32):
    result = cast[NifcOther](tag(c))
  else:
    result = NoSub

proc typeKind*(c: Cursor): NifcType {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNifcType(tag(c).uint32):
      result = cast[NifcType](tag(c))
    else:
      result = NoType
  elif c.kind == DotToken:
    result = VoidT
  else:
    result = NoType

proc callConvKind*(c: Cursor): CallConv {.inline.} =
  if c.kind == ParLe:
    if rawTagIsCallConv(tag(c).uint32):
      result = cast[CallConv](tag(c))
    else:
      result = NoCallConv
  elif c.kind == Ident:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if rawTagIsCallConv(tagId.uint32):
      result = cast[CallConv](tagId)
    else:
      result = NoCallConv
  else:
    result = NoCallConv

proc exprKind*(c: Cursor): NifcExpr {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNifcExpr(tag(c).uint32):
      result = cast[NifcExpr](tag(c))
    else:
      result = NoExpr
  else:
    result = NoExpr

proc symKind*(c: Cursor): NifcSym {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNifcSym(tag(c).uint32):
      result = cast[NifcSym](tag(c))
    else:
      result = NoSym
  else:
    result = NoSym


type
  Definition* = object
    pos*: int
    kind*: NifcSym

  Module* = object
    code*: TokenBuf
    types*: seq[int]
    defs*: Table[SymId, Definition]
    filename*: string
    config*: ConfigRef

proc tracebackTypeC*(n: Cursor): Cursor =
  assert n.typeKind in {ObjectT, UnionT, ArrayT}
  result = n
  while result.stmtKind != TypeS:
    unsafeDec result

proc parse*(r: var Reader; m: var Module; parentInfo: PackedLineInfo): bool =
  let t = next(r)
  var currentInfo = parentInfo
  if t.filename.len == 0:
    # relative file position
    if t.pos.line != 0 or t.pos.col != 0:
      let (file, line, col) = unpack(pool.man, parentInfo)
      currentInfo = pack(pool.man, file, line+t.pos.line, col+t.pos.col)
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
    if tag.uint32 == TypeTagId:
      m.types.add m.code.len
    copyInto(m.code, tag, currentInfo):
      while true:
        let progress = parse(r, m, currentInfo)
        if not progress: break
  of UnknownToken:
    copyInto m.code, ErrT, currentInfo:
      m.code.addStrLit decodeStr(t), currentInfo
  of DotToken:
    m.code.addDotToken()
  of Ident:
    m.code.addIdent decodeStr(t), currentInfo
  of Symbol:
    m.code.add symToken(pool.syms.getOrIncl(decodeStr t), currentInfo)
  of SymbolDef:
    # Remember where to find this symbol:
    let litId = pool.syms.getOrIncl(decodeStr t)
    let pos = m.code.len - 1
    let n = cursorAt(m.code, pos)
    m.defs[litId] = Definition(pos: pos, kind: n.symKind)
    endRead(m.code)
    m.code.add symdefToken(litId, currentInfo)
  of StringLit:
    m.code.addStrLit decodeStr(t), currentInfo
  of CharLit:
    m.code.addAtom CharLit, uint32 decodeChar(t), currentInfo
  of IntLit:
    # we keep numbers as strings because we typically don't do anything with them
    # but to pass them as they are to the C code.
    m.code.addAtom IntLit, pool.strings.getOrIncl(decodeStr t), currentInfo
  of UIntLit:
    m.code.addAtom UIntLit, pool.strings.getOrIncl(decodeStr t), currentInfo
  of FloatLit:
    m.code.addAtom FloatLit, pool.strings.getOrIncl(decodeStr t), currentInfo

proc parse*(r: var Reader): Module =
  # empirically, (size div 7) is a good estimate for the number of nodes
  # in the file:
  let nodeCount = r.fileSize div 7
  result = Module(code: createTokenBuf(nodeCount))
  discard parse(r, result, NoLineInfo)

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

# Read helpers:

template elementType*(types: TypeGraph; n: NodePos): NodePos = n.firstSon

type
  TypeDecl* = object
    name*, pragmas*, body*: NodePos

proc asTypeDecl*(types: TypeGraph; n: NodePos): TypeDecl =
  assert types[n].kind == TypeC
  let (a, b, c) = sons3(types, n)
  TypeDecl(name: a, pragmas: b, body: c)

type
  FieldDecl* = object
    name*, pragmas*, typ*: NodePos

proc asFieldDecl*(types: TypeGraph; n: NodePos): FieldDecl =
  assert types[n].kind == FldC
  let (a, b, c) = sons3(types, n)
  FieldDecl(name: a, pragmas: b, typ: c)

type
  ParamDecl* = object
    name*, pragmas*, typ*: NodePos

proc asParamDecl*(types: TypeGraph; n: NodePos): ParamDecl =
  assert types[n].kind == ParamC
  let (a, b, c) = sons3(types, n)
  ParamDecl(name: a, pragmas: b, typ: c)

type
  ProcType* = object
    params*, returnType*, pragmas*: NodePos

proc asProcType*(types: TypeGraph; n: NodePos): ProcType =
  assert types[n].kind in {ProctypeC, ProcC}
  let (_, a, b, c) = sons4(types, n)
  ProcType(params: a, returnType: b, pragmas: c)

type
  ProcDecl* = object
    name*, params*, returnType*, pragmas*, body*: NodePos

proc asProcDecl*(t: Tree; n: NodePos): ProcDecl =
  assert t[n].kind == ProcC
  let (a, b, c, d, e) = sons5(t, n)
  ProcDecl(name: a, params: b, returnType: c, pragmas: d, body: e)

type
  VarDecl* = object
    name*, pragmas*, typ*, value*: NodePos

proc asVarDecl*(t: Tree; n: NodePos): VarDecl =
  assert t[n].kind in {GvarC, TvarC, VarC, ConstC}
  let (a, b, c, d) = sons4(t, n)
  VarDecl(name: a, pragmas: b, typ: c, value: d)
