#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / assertions
include nifprelude
import stringviews

import ".." / models / [tags, nimony_tags, callconv_tags]
export nimony_tags, callconv_tags

template tagEnum*(c: Cursor): TagEnum = cast[TagEnum](tag(c))

proc stmtKind*(c: Cursor): NimonyStmt {.inline.} =
  if c.kind == ParLe and rawTagIsNimonyStmt(tagEnum(c)):
    result = cast[NimonyStmt](tagEnum(c))
  else:
    result = NoStmt

proc pragmaKind*(c: Cursor): NimonyPragma {.inline.} =
  if c.kind == ParLe:
    let e = tagEnum(c)
    if rawTagIsNimonyPragma(e):
      result = cast[NimonyPragma](e)
    else:
      result = NoPragma
  elif c.kind == Ident:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if rawTagIsNimonyPragma(cast[TagEnum](tagId)):
      result = cast[NimonyPragma](tagId)
    else:
      result = NoPragma
  else:
    result = NoPragma

proc substructureKind*(c: Cursor): NimonyOther {.inline.} =
  if c.kind == ParLe and rawTagIsNimonyOther(tagEnum(c)):
    result = cast[NimonyOther](tag(c))
  else:
    result = NoSub

proc typeKind*(c: Cursor): NimonyType {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNimonyType(tagEnum(c)):
      result = cast[NimonyType](tagEnum(c))
    else:
      result = NoType
  elif c.kind == DotToken:
    result = VoidT
  else:
    result = NoType

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

proc exprKind*(c: Cursor): NimonyExpr {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNimonyExpr(tagEnum(c)):
      result = cast[NimonyExpr](tagEnum(c))
    else:
      result = NoExpr
  else:
    result = NoExpr

proc symKind*(c: Cursor): NimonySym {.inline.} =
  if c.kind == ParLe:
    if rawTagIsNimonySym(tagEnum(c)):
      result = cast[NimonySym](tagEnum(c))
    else:
      result = NoSym
  else:
    result = NoSym

proc cfKind*(c: Cursor): ControlFlowKind {.inline.} =
  if c.kind == ParLe:
    if rawTagIsControlFlowKind(tagEnum(c)):
      result = cast[ControlFlowKind](tagEnum(c))
    else:
      result = NoControlFlow
  else:
    result = NoControlFlow

template isParamsTag*(c: Cursor): bool = c.tagEnum == ParamsTagId

# Outdated aliases:
type
  SymKind* = NimonySym
  ExprKind* = NimonyExpr
  StmtKind* = NimonyStmt
  SubstructureKind* = NimonyOther
  PragmaKind* = NimonyPragma
  TypeKind* = NimonyType

const
  IntT* = IT
  UIntT* = UT
  FloatT* = FT
  CharT* = CT
  HoleyEnumT* = OnumT
  InvokeT* = AtT

const
  RoutineKinds* = {ProcY, FuncY, IteratorY, TemplateY, MacroY, ConverterY, MethodY}
  CallKinds* = {CallX, CallstrlitX, CmdX, PrefixX, InfixX, HcallX}
  ConvKinds* = {HconvX, ConvX, OconvX, DconvX, CastX}
  TypeclassKinds* = {ConceptT, TypeKindT, OrdinalT, OrT, AndT, NotT}

proc addParLe*(dest: var TokenBuf; kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind|ControlFlowKind|CallConv;
               info = NoLineInfo) =
  dest.add parLeToken(cast[TagId](kind), info)

proc addParPair*(dest: var TokenBuf; kind: TypeKind|PragmaKind|ExprKind|StmtKind; info = NoLineInfo) =
  dest.add parLeToken(cast[TagId](kind), info)
  dest.addParRi()

proc parLeToken*(kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind|PragmaKind; info = NoLineInfo): PackedToken =
  parLeToken(cast[TagId](kind), info)

template copyIntoKind*(dest: var TokenBuf; kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind|PragmaKind;
                       info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(kind, info)
  body
  dest.addParRi()

template copyIntoKinds*(dest: var TokenBuf; kinds: array[2, StmtKind]; info: PackedLineInfo; body: untyped) =
  dest.add parLeToken(kinds[0], info)
  dest.add parLeToken(kinds[1], info)
  body
  dest.addParRi()
  dest.addParRi()

template copyInto*(dest: var TokenBuf; n: var Cursor; body: untyped) =
  assert n.kind == ParLe
  dest.add n
  inc n
  body
  dest.addParRi()
  skipParRi n

proc isAtom*(n: Cursor): bool {.inline.} = n.kind < ParLe

proc copyIntoSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symToken(s, info)

proc copyTree*(dest: var TokenBuf; src: TokenBuf) {.inline.} =
  dest.add src

proc copyTree*(dest: var TokenBuf; src: Cursor) {.inline.} =
  dest.addSubtree src

proc addSymDef*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.add symdefToken(s, info)

proc addEmpty*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.add dotToken(info)

proc addEmpty2*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.add dotToken(info)
  dest.add dotToken(info)

proc addEmpty3*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.add dotToken(info)
  dest.add dotToken(info)
  dest.add dotToken(info)

proc takeTree*(dest: var TokenBuf; n: var Cursor) =
  if n.kind != ParLe:
    dest.add n
    inc n
  else:
    var nested = 0
    while true:
      dest.add n
      case n.kind
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == 0:
          inc n
          break
      of EofToken:
        raiseAssert "expected ')', but EOF reached"
      else: discard
      inc n

proc sameTrees*(a, b: Cursor): bool =
  var a = a
  var b = b
  var nested = 0
  let isAtom = a.kind != ParLe
  while true:
    if a.kind != b.kind: return false
    case a.kind
    of ParLe:
      if a.tagId != b.tagId: return false
      inc nested
    of ParRi:
      dec nested
      if nested == 0: return true
    of Symbol, SymbolDef:
      if a.symId != b.symId: return false
    of IntLit:
      if a.intId != b.intId: return false
    of UIntLit:
      if a.uintId != b.uintId: return false
    of FloatLit:
      if a.floatId != b.floatId: return false
    of StringLit, Ident:
      if a.litId != b.litId: return false
    of CharLit, UnknownToken:
      if a.uoperand != b.uoperand: return false
    of DotToken, EofToken: discard "nothing else to compare"
    if isAtom: return true
    inc a
    inc b
  return false

proc isDeclarative*(n: Cursor): bool =
  case n.stmtKind
  of FromimportS, ImportS, ExportS, IncludeS, ImportExceptS, TypeS, CommentS, TemplateS:
    result = true
  else:
    case n.substructureKind
    of PragmasU, TypevarsU:
      result = true
    else:
      case n.exprKind
      of TypeofX:
        result = true
      else:
        result = false

proc isCompileTimeType*(n: Cursor): bool {.inline.} =
  n.typeKind in {TypeKindT, TypedescT, SymKindT, OrT, AndT, NotT, ConceptT, StaticT}

proc firstSon*(n: Cursor): Cursor {.inline.} =
  result = n
  inc result

proc hookName*(op: HookKind): string =
  case op
  of DestroyH: "destroy"
  of WasmovedH: "wasMoved"
  of DupH: "dup"
  of CopyH: "copy"
  of SinkhH: "sink"
  of TraceH: "trace"
  of NoHook: "(NoHook)"

const
  NoSymId* = SymId(0)

proc extractPragma*(n: Cursor; kind: PragmaKind): Cursor =
  var n = n
  if n.kind != DotToken:
    inc n
    while n.kind != ParRi:
      if pragmaKind(n) == kind:
        inc n
        return n
      skip n
  result = default(Cursor)

proc hasPragma*(n: Cursor; kind: PragmaKind): bool =
  result = not cursorIsNil(extractPragma(n, kind))

proc hasPragmaOfValue*(n: Cursor; kind: PragmaKind; val: string): bool =
  let p = extractPragma(n, kind)
  result = not cursorIsNil(p) and p.kind == StringLit and pool.strings[p.litId] == val

proc addSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) =
  dest.add symToken(s, info)

const
  TypeModifiers = {MutT, OutT, LentT, SinkT, StaticT}

proc removeModifier*(a: var Cursor) =
  if a.kind == ParLe and a.typeKind in TypeModifiers:
    inc a

proc skipModifier*(a: Cursor): Cursor =
  result = a
  removeModifier(result)

const
  LocalDecls* = {VarS, LetS, ConstS, ResultS, CursorS, GvarS, TvarS, GletS, TletS}
