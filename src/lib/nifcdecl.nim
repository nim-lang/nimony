#
#           Leng tag decoding for `nifcore` cursors
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution, for
#    details about the copyright.
#

## The `nifcore` counterpart of `lengc/leng_model`: tag decoders and declaration
## readers over **nifcore** cursors (no separate ParLe/ParRi; subtrees are
## bounded by jump fields). Originally `arkham/nifcdecl` in the `nativenif`
## repo; moved here so the shoggoth Leng optimizer and arkham's code generators
## share a single source (arkham already builds against nimony's `src/lib`).
##
## It reuses the canonical, NIF-API-independent enums from `models/leng_tags`
## (`LengStmt`, `LengExpr`, `LengType`, `LengPragma`, `LengOther`, `LengSym`, …)
## whose ordinals are the master tag ordinals. To make a parsed `nifcore`
## buffer's `cursorTagId` line up with those ordinals, seed the buffer's
## `TagPool` from the same master `TagData` (just like `nifstreams` seeds its
## global pool). Then `cast[LengStmt](tagId)` works exactly as in `leng_model`
## — but over `nifcore` cursors, keeping the enums clear of any NIF-API
## coupling.

import std / assertions
import nifcore
import ".." / models / [leng_tags, callconv_tags, tags]
export leng_tags, callconv_tags

proc createLengTagPool*(): TagPool =
  ## A `nifcore` tag pool seeded so each Leng tag's `TagId` equals its master
  ## `TagEnum` ordinal. Pass to `parseFromFile`/`parseFromBuffer` as
  ## `sharedTags` so `tagEnum`/`stmtKind`/… can decode by ordinal.
  result = newTagPool()
  for e in TagEnum:
    if e == InvalidTagId: continue
    let id = result.registerTag(TagData[e][0])
    assert uint32(id) == uint32(TagData[e][1]),
      "nifc tag pool misalignment for " & TagData[e][0]

template tagEnumOf*(c: Cursor): TagEnum =
  (if c.kind == TagLit: cast[TagEnum](uint32(c.cursorTagId)) else: InvalidTagId)

template tagEnum*(c: Cursor): TagEnum =
  ## `leng_model`-compatible spelling of `tagEnumOf`.
  tagEnumOf(c)

# ── tag-class decoders ──────────────────────────────────────────────────────

proc stmtKind*(c: Cursor): LengStmt {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsLengStmt(e): cast[LengStmt](e) else: NoStmt

proc exprKind*(c: Cursor): LengExpr {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsLengExpr(e): cast[LengExpr](e) else: NoExpr

proc pragmaKind*(c: Cursor): LengPragma {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsLengPragma(e): cast[LengPragma](e) else: NoPragma

proc substructureKind*(c: Cursor): LengOther {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsLengOther(e): cast[LengOther](e) else: NoSub

proc typeKind*(c: Cursor): LengType {.inline.} =
  if c.kind == DotToken: return VoidT       # an empty type slot reads as void
  let e = tagEnumOf(c)
  if rawTagIsLengType(e): cast[LengType](e) else: NoType

proc symKind*(c: Cursor): LengSym {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsLengSym(e): cast[LengSym](e) else: NoSym

proc typeQual*(c: Cursor): LengTypeQualifier {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsLengTypeQualifier(e): cast[LengTypeQualifier](e) else: NoQualifier

proc callConvKind*(c: Cursor): CallConv {.inline.} =
  let e = tagEnumOf(c)
  if rawTagIsCallConv(e): cast[CallConv](e) else: NoCallConv

# Read helpers:

proc elementType*(n: Cursor): Cursor {.inline.} =
  ## First child of a (single-element) type node, e.g. the `T` of `(ptr T)`.
  ## In `nifcore`, `inc` past a `TagLit` head lands on the first body token.
  result = n
  inc result

# NB: `leng_model.tracebackTypeC` is intentionally not ported — it walks
# *backwards* via `unsafeDec`, which `nifcore` has no primitive for, and no
# current consumer needs it. Add a backward primitive to `nifcore` first if a
# caller appears.

# ── declaration readers (mirror leng_model; already in into/skip/hasMore idiom)

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

# ── Tag-typed intent overloads (mirrors leng_model / nimony_model) ───────────

type LengTagKind* =
  LengStmt | LengExpr | LengType | LengOther | LengPragma | LengSym

# See `nimony_model.nim`'s `tagDispatch` for the rationale: tag-class
# templates dispatch via `when expected is X` and use `==`/`$` operators that
# don't exist on the union itself, so we mark them `.untyped` under nimony so
# the body isn't typechecked at definition time.
when defined(nimony):
  {.pragma: tagDispatch, untyped.}
else:
  {.pragma: tagDispatch.}

template kindMatches(c: Cursor; expected: LengTagKind): bool {.tagDispatch.} =
  when expected is LengStmt:    c.stmtKind == expected
  elif expected is LengExpr:    c.exprKind == expected
  elif expected is LengType:    c.typeKind == expected
  elif expected is LengOther:   c.substructureKind == expected
  elif expected is LengPragma:  c.pragmaKind == expected
  elif expected is LengSym:     c.symKind == expected
  else:                         false

template skip*(c: var Cursor; expected: LengTagKind) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "skip " & $expected & ": cursor at kind=" & $c.kind
  skip c

template inc*(c: var Cursor; expected: LengTagKind) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "inc " & $expected & ": cursor at kind=" & $c.kind
  inc c

template into*(c: var Cursor; expected: LengTagKind; body: untyped) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "into " & $expected & ": cursor at kind=" & $c.kind
  into c:
    body

template loopInto*(c: var Cursor; expected: LengTagKind; body: untyped) {.tagDispatch.} =
  assert kindMatches(c, expected),
    "loopInto " & $expected & ": cursor at kind=" & $c.kind
  loopInto c:
    body
