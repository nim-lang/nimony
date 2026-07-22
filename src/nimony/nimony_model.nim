#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / assertions
include ".." / lib / nifprelude
import ".." / lib / [stringviews, symparser]

import ".." / models / [tags, nimony_tags, callconv_tags]
export nimony_tags, callconv_tags

template tagEnum*(c: Cursor): TagEnum =
  ## Safe on any cursor position: atoms and scope ends yield `InvalidTagId`
  ## (same contract as nifcdecl.tagEnumOf).
  (if c.isTagLit: cast[TagEnum](tag(c)) else: InvalidTagId)

template tagEnum*(c: PackedToken): TagEnum = cast[TagEnum](tag(c))

proc stmtKind*(c: PackedToken): NimonyStmt {.inline.} =
  if c.isTagLit and rawTagIsNimonyStmt(tagEnum(c)):
    result = cast[NimonyStmt](tagEnum(c))
  else:
    result = NoStmt

proc stmtKind*(c: Cursor): NimonyStmt {.inline.} =
  # Routed through `c.kind` so a bounded cursor at its scope's end (virtual
  # ParRi under `-d:virtualParRi`) yields the none-value instead of asserting.
  if c.isTagLit:
    result = stmtKind(c.load())
  else:
    result = NoStmt

proc pragmaKind*(c: Cursor): NimonyPragma {.inline.} =
  if c.isTagLit:
    let e = tagEnum(c)
    if rawTagIsNimonyPragma(e):
      result = cast[NimonyPragma](e)
    else:
      result = NoPragma
  elif c.isIdent:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if tagId.int >= 0 and tagId.int <= high(TagEnum).int and rawTagIsNimonyPragma(cast[TagEnum](tagId)):
      result = cast[NimonyPragma](tagId)
    else:
      result = NoPragma
  else:
    result = NoPragma

proc substructureKind*(c: PackedToken): NimonyOther {.inline.} =
  if c.isTagLit and rawTagIsNimonyOther(tagEnum(c)):
    result = cast[NimonyOther](tag(c))
  else:
    result = NoSub

proc substructureKind*(c: Cursor): NimonyOther {.inline.} =
  if c.isTagLit:
    result = substructureKind(c.load())
  else:
    result = NoSub

proc typeKind*(c: Cursor): NimonyType {.inline.} =
  if c.isTagLit:
    if rawTagIsNimonyType(tagEnum(c)):
      result = cast[NimonyType](tagEnum(c))
    else:
      result = NoType
  elif c.isDotToken:
    result = VoidT
  else:
    result = NoType

proc callConvKind*(c: Cursor): CallConv {.inline.} =
  if c.isTagLit:
    if rawTagIsCallConv(tagEnum(c)):
      result = cast[CallConv](tag(c))
    else:
      result = NoCallConv
  elif c.isIdent:
    let tagId = pool.tags.getOrIncl(pool.strings[c.litId])
    if rawTagIsCallConv(cast[TagEnum](tagId)):
      result = cast[CallConv](tagId)
    else:
      result = NoCallConv
  else:
    result = NoCallConv

proc exprKind*(c: PackedToken): NimonyExpr {.inline.} =
  if c.isTagLit:
    if rawTagIsNimonyExpr(tagEnum(c)):
      result = cast[NimonyExpr](tagEnum(c))
    else:
      result = NoExpr
  else:
    result = NoExpr

proc exprKind*(c: Cursor): NimonyExpr {.inline.} =
  if c.isTagLit:
    result = exprKind(c.load())
  else:
    result = NoExpr

proc symKind*(c: Cursor): NimonySym {.inline.} =
  if c.isTagLit:
    if rawTagIsNimonySym(tagEnum(c)):
      result = cast[NimonySym](tagEnum(c))
    else:
      result = NoSym
  else:
    result = NoSym

proc cfKind*(c: Cursor): ControlFlowKind {.inline.} =
  if c.isTagLit:
    if rawTagIsControlFlowKind(tagEnum(c)):
      result = cast[ControlFlowKind](tagEnum(c))
    else:
      result = NoControlFlow
  else:
    result = NoControlFlow

proc hookKind*(x: TagId): HookKind {.inline.} =
  if rawTagIsHookKind(cast[TagEnum](x)):
    result = cast[HookKind](x)
  else:
    result = NoHook

template isParamsTag*(c: Cursor): bool = c.tagEnum == ParamsTagId

# Outdated aliases:
type
  SymKind* = NimonySym
  ExprKind* = NimonyExpr
  StmtKind* = NimonyStmt
  SubstructureKind* = NimonyOther
  PragmaKind* = NimonyPragma
  TypeKind* = NimonyType

# ── Tag-typed intent overloads for inc/skip/into/loopInto ───────────────────
# Concrete-tag intents document the expected node kind and fail loudly at
# runtime when reality differs. Use these when the tag is statically known
# (`n.into IfS: …`); use `TagClass` (Anything/AnyExpr/AnyStmt/AnyType) for
# the looser categorical intent; fall back to `SkipIntent` for role-style
# annotations (SkipName, SkipPragmas, …).

type NimonyTagKind* =
  NimonyStmt | NimonyExpr | NimonyType | NimonyOther | NimonyPragma | NimonySym

# Tag-typed `skip`/`inc`/`into`/`loopInto` overloads (and the `kindMatches`
# helper they share) take a tag-class argument. The body uses
# `when expected is X` to dispatch on which tag-class the caller passed.
# Nimony typechecks generic code, so a *typed* template here would force
# `==` and `$` to exist on the union type at definition time — they don't.
# Mark the templates `untyped` (under nimony) so the body is only checked
# at instantiation, when `expected` has a concrete tag-class type. Host
# Nim is fine with the typed form, so we keep the `NimonyTagKind` typing
# there for sharper sigs and IDE help.
when defined(nimony):
  {.pragma: tagDispatch, untyped.}
else:
  {.pragma: tagDispatch.}

template kindMatches(c: Cursor; expected: NimonyTagKind): bool {.tagDispatch.} =
  when expected is NimonyStmt:    c.stmtKind == expected
  elif expected is NimonyExpr:    c.exprKind == expected
  elif expected is NimonyType:    c.typeKind == expected
  elif expected is NimonyOther:   c.substructureKind == expected
  elif expected is NimonyPragma:  c.pragmaKind == expected
  elif expected is NimonySym:     c.symKind == expected
  else:                           false

template skip*(c: var Cursor; expected: NimonyTagKind) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "skip " & $expected & ": cursor at kind=" & $c.kind &
    " (stmt=" & $c.stmtKind & " expr=" & $c.exprKind & " type=" & $c.typeKind & ")"
  skip c

template inc*(c: var Cursor; expected: NimonyTagKind) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "inc " & $expected & ": cursor at kind=" & $c.kind &
    " (stmt=" & $c.stmtKind & " expr=" & $c.exprKind & " type=" & $c.typeKind & ")"
  inc c

template into*(c: var Cursor; expected: NimonyTagKind; body: untyped) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "into " & $expected & ": cursor at kind=" & $c.kind &
    " (stmt=" & $c.stmtKind & " expr=" & $c.exprKind & " type=" & $c.typeKind & ")"
  into c:
    body

template loopInto*(c: var Cursor; expected: NimonyTagKind; body: untyped) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "loopInto " & $expected & ": cursor at kind=" & $c.kind &
    " (stmt=" & $c.stmtKind & " expr=" & $c.exprKind & " type=" & $c.typeKind & ")"
  loopInto c:
    body

const
  IntT* = IT
  UIntT* = UT
  FloatT* = FT
  CharT* = CT
  HoleyEnumT* = OnumT
  InvokeT* = AtT

const
  RoutineKinds* = {ProcY, FuncY, IteratorY, TemplateY, MacroY, ConverterY, MethodY}
  CallKinds* = {CallX, CallstrlitX, CmdX, PrefixX, InfixX, HcallX, ProccallX, DelayX}
  CallKindsS* = {CallS, CallstrlitS, CmdS, PrefixS, InfixS, HcallS}
  ConvKinds* = {HconvX, ConvX, DconvX, CastX}
  TypeclassKinds* = {ConceptT, TypekindT, OrdinalT, OrT, AndT, NotT}
  RoutineTypes* = {ProcT, FuncT, IteratorT, TemplateT, MacroT, ConverterT, MethodT, ProctypeT, ItertypeT}

proc addParLe*[T: enum](dest: var TokenBuf; kind: T; info = NoLineInfo) =
  ## Open a tag from a kind enum (TypeKind/StmtKind/… all share TagEnum
  ## ordinals). A plain `enum` constraint so `ord` type-checks (a union
  ## constraint does not admit `ord`/`uint32` under nimony's Nim-2 generics).
  dest.addParLe(cast[TagId](uint32(ord(kind))), info)

proc addParPair*[T: enum](dest: var TokenBuf; kind: T; info = NoLineInfo) =
  dest.addParLe(cast[TagId](uint32(ord(kind))), info)
  dest.addParRi()

template setTagAt*(dest: var TokenBuf; pos: int; tag: TagId) =
  ## In-place retag of the token at `pos` (nifcore's `dest[pos]` yields a value,
  ## so it must be read, mutated, and written back).
  var t = dest[pos]
  setTag(t, tag)
  dest[pos] = t
template setSymIdAt*(dest: var TokenBuf; pos: int; sym: SymId) =
  var t = dest[pos]
  setSymId(t, sym)
  dest[pos] = t
template copyKeepLineInfoAt*(dest: var TokenBuf; pos: int; src: untyped) =
  var t = dest[pos]
  copyKeepLineInfo(t, src)
  dest[pos] = t
proc lastValueStart*(b: TokenBuf): int =
  ## Index of the head of the last complete value in `b`: skips the trailing
  ## suffix tokens (line info / extended bits) that follow the value's head.
  result = b.len - 1
  while result > 0 and readonlyCursorAt(b, result).kind in {ExtendedSuffix, LineInfoLit}:
    dec result

proc retagAt*[T: enum](dest: var TokenBuf; pos: int; kind: T; info = NoLineInfo) =
  ## Rewrite the tag of an already-emitted open token at `pos`, preserving its
  ## sealed jump/body (the classic `dest[pos] = parLeToken(kind, info)` idiom).
  var t = dest[pos]
  setTag(t, cast[TagId](uint32(ord(kind))))
  dest[pos] = t
proc addParLe*(dest: var TokenBuf; tag: string; info = NoLineInfo) {.inline.} =
  ## Open a tag by name (replaces the classic `dest.add tagToken(tag)`).
  dest.addParLe(pool.tags.getOrIncl(tag), info)

template copyIntoKind*(dest: var TokenBuf; kind: TypeKind|SymKind|ExprKind|StmtKind|SubstructureKind|PragmaKind;
                       info: PackedLineInfo; body: untyped) =
  dest.addParLe(kind, info)
  body
  dest.addParRi()

template copyIntoKinds*(dest: var TokenBuf; kinds: array[2, StmtKind]; info: PackedLineInfo; body: untyped) =
  dest.addParLe(kinds[0], info)
  dest.addParLe(kinds[1], info)
  body
  dest.addParRi()
  dest.addParRi()

proc skipParRi(n: var Cursor) =
  assert not n.hasMore, "expected ')'"
  consumeParRi n

template copyInto*(dest: var TokenBuf; n: var Cursor; body: untyped) =
  assert n.isTagLit
  dest.addParLe(n.tag, n.info)
  n.into:
    body
  dest.addParRi()

template takeInto*(dest: var TokenBuf; n: var Cursor; body: untyped) =
  ## Like `copyInto`, but the emitted closing `)` keeps the input close's
  ## line info — the exact behavior of the classic `takeToken`/`takeParRi`
  ## pair. Downstream sem phases read close infos (e.g. `addReturnResult`),
  ## so rewrites of `takeParRi` must use this, not `copyInto`. Under
  ## `-d:virtualParRi` an elided close yields NoLineInfo.
  assert n.isTagLit
  dest.addParLe(n.tag, n.info)
  n.into:
    body
    dest.addParRi(n.endInfo)

proc isAtom*(n: Cursor): bool {.inline.} = n.hasMore and not n.isTagLit

proc copyIntoSymUse*(dest: var TokenBuf; s: SymId; info: PackedLineInfo) {.inline.} =
  dest.addSymUse(s, info)

proc copyTree*(dest: var TokenBuf; src: TokenBuf) {.inline.} =
  dest.add src

proc copyTree*(dest: var TokenBuf; src: Cursor) {.inline.} =
  dest.addSubtree src

proc addEmpty*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.addDotToken(info)

proc addEmpty2*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.addDotToken(info)
  dest.addDotToken(info)

proc addEmpty3*(dest: var TokenBuf; info: PackedLineInfo = NoLineInfo) =
  dest.addDotToken(info)
  dest.addDotToken(info)
  dest.addDotToken(info)

proc symNameId(s: SymId): StrId =
  var name = pool.syms[s]
  extractBasename name
  pool.strings.getOrIncl(name)

proc sameTreesNC(a, b: Cursor): bool =
  ## Structural equality on nifcore cursors, ignoring sparse line-info
  ## suffixes (they are not iterated as children).
  if a.hasMore != b.hasMore: return false
  if not a.hasMore: return true
  let ka = a.load.kind
  if ka != b.load.kind: return false
  case ka
  of TagLit:
    if cursorTagId(a) != cursorTagId(b): return false
    var ca = childCursor(a)
    var cb = childCursor(b)
    while ca.hasMore and cb.hasMore:
      if not sameTreesNC(ca, cb): return false
      skip ca
      skip cb
    result = ca.hasMore == cb.hasMore
  of Symbol, SymbolDef: result = symId(a) == symId(b)
  of IntLit:            result = intVal(a) == intVal(b)
  of UIntLit:           result = uintVal(a) == uintVal(b)
  of FloatLit:          result = floatVal(a) == floatVal(b)
  of StrLit, Ident:     result = strId(a) == strId(b)
  of CharLit:           result = charLit(a) == charLit(b)
  of DotToken:          result = true
  of ExtendedSuffix, LineInfoLit: result = true

proc sameTreesIgnoreSymIdsNC(a, b: Cursor): bool =
  let aName = a.hasMore and a.load.kind in {nifcore.Symbol, nifcore.SymbolDef, nifcore.Ident}
  let bName = b.hasMore and b.load.kind in {nifcore.Symbol, nifcore.SymbolDef, nifcore.Ident}
  if aName or bName:
    if not (aName and bName): return false
    let an = if a.load.kind == nifcore.Ident: a.litId else: symNameId(a.symId)
    let bn = if b.load.kind == nifcore.Ident: b.litId else: symNameId(b.symId)
    return an == bn
  if a.hasMore != b.hasMore: return false
  if not a.hasMore: return true
  let ka = a.load.kind
  if ka != b.load.kind: return false
  case ka
  of TagLit:
    if cursorTagId(a) != cursorTagId(b): return false
    var ca = childCursor(a)
    var cb = childCursor(b)
    while ca.hasMore and cb.hasMore:
      if not sameTreesIgnoreSymIdsNC(ca, cb): return false
      skip ca
      skip cb
    result = ca.hasMore == cb.hasMore
  of IntLit:            result = intVal(a) == intVal(b)
  of UIntLit:           result = uintVal(a) == uintVal(b)
  of FloatLit:          result = floatVal(a) == floatVal(b)
  of StrLit:            result = strId(a) == strId(b)
  of CharLit:           result = charLit(a) == charLit(b)
  of DotToken:          result = true
  of Symbol, SymbolDef, Ident, ExtendedSuffix, LineInfoLit: result = true

proc sameTrees*(a, b: Cursor): bool =
  return sameTreesNC(a, b)
proc sameTreesButIgnoreSymIds*(a, b: Cursor): bool =
  ## Like `sameTrees` but maps symbols back to their base identifier names.
  ## Used for forward declaration matching and concept requirement comparison.
  return sameTreesIgnoreSymIdsNC(a, b)
proc isDeclarative*(n: Cursor): bool =
  case n.stmtKind
  of FromimportS, ImportS, ExportS, IncludeS, ImportexceptS, TypeS, CommentS, TemplateS:
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
  n.typeKind in {TypekindT, TypedescT, SymkindT, OrT, AndT, NotT, ConceptT, StaticT}

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
  if not n.isDotToken:
    n.into:  # (pragmas …)
      while n.hasMore:
        if pragmaKind(n) == kind:
          inc n, SkipTag  # past the matched pragma's open
          return n  # caller reads the pragma's value at this position
        skip n
  result = default(Cursor)

proc hasPragma*(n: Cursor; kind: PragmaKind): bool =
  result = not cursorIsNil(extractPragma(n, kind))

proc hasPragmaOfValue*(n: Cursor; kind: PragmaKind; val: string): bool =
  let p = extractPragma(n, kind)
  result = not cursorIsNil(p) and p.isStringLit and pool.strings[p.litId] == val

const
  TypeModifiers* = {MutT, OutT, LentT, SinkT, StaticT}

proc removeModifier*(a: var Cursor) =
  if a.isTagLit and a.typeKind in TypeModifiers:
    inc a

proc skipModifier*(a: Cursor): Cursor =
  result = a
  removeModifier(result)

const
  LocalDecls* = {VarS, LetS, ConstS, ResultS, CursorS, PatternvarS, GvarS, TvarS, GletS, TletS}

template skipToLocalType*(n) =
  inc n # skip ParLe
  inc n # skip name
  skip n # skip export marker
  skip n # skip pragmas

proc skipToReturnType*(n: var Cursor) =
  ## Advances `n` past the prefix slots so it points at the return type.
  ## Handles Nimony's compact proctype/itertype layout (`(<tag> <NilTag> (params) RetType ...)`)
  ## and the proc-decl-shaped layout (`(proc Name Export Pattern Typevars (params) RetType ...)`).
  let skipKind = n.typeKind
  inc n # skip ParLe
  if skipKind in {ProctypeT, ItertypeT}:
    skip n # nilability tag
    skip n # params
  else:
    skip n # name
    skip n # export marker
    skip n # pattern
    skip n # generics
    skip n # params

proc procHasPragma*(typ: Cursor; kind: PragmaKind): bool =
  var typ = typ
  if typ.typeKind in RoutineTypes:
    skipToReturnType typ
    skip typ, SkipType # return type
    result = hasPragma(typ, kind)
  else:
    result = false

type
  Effect* = enum
    HasNoSideEffect
    HasSideEffect

proc whichEffect*(k: StmtKind; pragmas: Cursor): Effect =
  if k in {FuncS, IteratorS, ConverterS}:
    result = HasNoSideEffect
    if hasPragma(pragmas, SideEffectP):
      # explict override?
      result = HasSideEffect
  elif hasPragma(pragmas, NoSideEffectP):
    result = HasNoSideEffect
  else:
    result = HasSideEffect

proc isNilAnnotation*(n: Cursor): bool {.inline.} =
  ## Returns true if `n` is a `(notnil)`, `(nil)`, or `(unchecked)` annotation.
  n.isTagLit and n.substructureKind in {NotnilU, NilU, UncheckedU}

proc skipNilAnnotation*(n: var Cursor) {.inline.} =
  ## Skip a trailing nil annotation `(notnil)`, `(nil)`, or `(unchecked)`
  ## plus any further attributes (importc/header/...) that `fitTypeToPragmas`
  ## may have appended when an importc'd pointer alias was inlined.
  while n.hasMore:
    skip n
