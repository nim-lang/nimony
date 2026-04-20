#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parse NIF into a packed tree representation.

import std / [assertions]
include "../lib" / nifprelude
import ".." / models / [nifc_tags, callconv_tags, tags]
export nifc_tags, callconv_tags

proc bug*(msg: string) {.noreturn.} =
  when defined(debug):
    writeStackTrace()
  quit "BUG: " & msg

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
