#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parse NIF into a packed tree representation.

import std / [assertions, syncio]
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
    consumeParRi n
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


proc tagToken*(t: NifcType; info = NoLineInfo): PackedToken =
  result = tagToken(TagId(t), info)

# Backwards-compat alias for callers that still use the old name; the body
# now goes through `tagToken` so the new (kind|tag|jump) layout is used.
template parLeToken*(t: NifcType; info = NoLineInfo): PackedToken =
  tagToken(t, info)

# Read helpers:

template elementType*(n: Cursor): Cursor = n.firstSon

type
  TypeDecl* = object
    name*, pragmas*, body*: Cursor

proc asTypeDeclImpl(n: var Cursor): TypeDecl =
  ## Reads child positions; advances `n` past the (type) scope close.
  assert n.stmtKind == TypeS
  n.into:
    result = TypeDecl(name: n)
    skip n
    result.pragmas = n
    skip n
    result.body = n
    skip n  # consume body so the into-epilogue closes the scope cleanly

proc asTypeDecl*(n: Cursor): TypeDecl =
  var n = n
  asTypeDeclImpl(n)  # local cursor is dropped on return

proc takeTypeDecl*(n: var Cursor): TypeDecl =
  asTypeDeclImpl(n)  # advances `n` past the close

type
  FieldDecl* = object
    name*, pragmas*, typ*: Cursor

proc takeFieldDecl*(n: var Cursor): FieldDecl =
  assert n.substructureKind == FldU
  n.into:
    result = FieldDecl(name: n)
    skip n
    result.pragmas = n
    skip n
    result.typ = n
    skip n

type
  ParamDecl* = object
    name*, pragmas*, typ*: Cursor

proc takeParamDecl*(n: var Cursor): ParamDecl =
  assert n.substructureKind == ParamU
  n.into:
    result = ParamDecl(name: n)
    skip n
    result.pragmas = n
    skip n
    result.typ = n
    skip n


type
  ProcType* = object
    params*, returnType*, pragmas*: Cursor

proc takeProcType*(n: var Cursor): ProcType =
  if n.typeKind == ParamsT:
    # Already inside a (proc/proctype) scope; the caller entered. Read the
    # remaining sibling cursors without entering a new scope.
    assert n.typeKind == ParamsT or n.kind == DotToken
    result = ProcType(params: n)
    skip n
    result.returnType = n
    skip n
    result.pragmas = n
    skip n
    if n.kind == DotToken:
      inc n
  else:
    assert n.stmtKind == ProcS or n.typeKind == ProctypeT
    n.into:  # enter (proc ...) / (proctype ...)
      skip n # name
      assert n.typeKind == ParamsT or n.kind == DotToken
      result = ProcType(params: n)
      skip n
      result.returnType = n
      skip n
      result.pragmas = n
      skip n
      if n.hasMore and n.kind == DotToken:
        inc n
      while n.hasMore: skip n

type
  ProcDecl* = object
    name*, params*, returnType*, pragmas*, body*: Cursor

proc takeProcDecl*(n: var Cursor): ProcDecl =
  assert n.stmtKind == ProcS
  n.into:
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

type
  VarDecl* = object
    name*, pragmas*, typ*, value*: Cursor

proc takeVarDecl*(n: var Cursor): VarDecl =
  assert n.stmtKind in {GvarS, TvarS, VarS, ConstS}
  n.into:
    result = VarDecl(name: n)
    skip n
    result.pragmas = n
    skip n
    result.typ = n
    skip n
    result.value = n
    skip n

# ── Tag-typed intent overloads (mirrors nimony_model's pattern) ─────────────

type NifcTagKind* =
  NifcStmt | NifcExpr | NifcType | NifcOther | NifcPragma | NifcSym

# See `nimony_model.nim`'s `tagDispatch` for the rationale: tag-class
# templates dispatch via `when expected is X` and use `==`/`$` operators
# that don't exist on the union itself, so we mark them `.untyped` under
# nimony so the body isn't typechecked at definition time.
when defined(nimony):
  {.pragma: tagDispatch, untyped.}
else:
  {.pragma: tagDispatch.}

template kindMatches(c: Cursor; expected: NifcTagKind): bool {.tagDispatch.} =
  when expected is NifcStmt:    c.stmtKind == expected
  elif expected is NifcExpr:    c.exprKind == expected
  elif expected is NifcType:    c.typeKind == expected
  elif expected is NifcOther:   c.substructureKind == expected
  elif expected is NifcPragma:  c.pragmaKind == expected
  elif expected is NifcSym:     c.symKind == expected
  else:                         false

template skip*(c: var Cursor; expected: NifcTagKind) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "skip " & $expected & ": cursor at kind=" & $c.kind
  skip c

template inc*(c: var Cursor; expected: NifcTagKind) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "inc " & $expected & ": cursor at kind=" & $c.kind
  inc c

template into*(c: var Cursor; expected: NifcTagKind; body: untyped) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "into " & $expected & ": cursor at kind=" & $c.kind
  into c:
    body

template loopInto*(c: var Cursor; expected: NifcTagKind; body: untyped) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "loopInto " & $expected & ": cursor at kind=" & $c.kind
  loopInto c:
    body
