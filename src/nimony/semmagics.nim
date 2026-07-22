#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Sem-checking of builtin "magic" expressions (defined/declared/compiles,
## bindSym, low/high, set ops, instanceof/is, deref/addr/sizeof, …).
##
## These handlers sit at the leaves of the sem recursion: they are dispatched
## to from `semExpr`'s big case in sem.nim, and the only way they re-enter the
## core is through three callbacks on `SemContext` (`semExprCB`, `commonTypeCB`,
## `semLocalTypeImplCB`). That lets this module compile separately from sem.nim
## instead of being textually `include`d, breaking the otherwise mutual
## recursion. The thin shims below restore the familiar `semExpr` / `commonType`
## / `semLocalType(Impl)` names so the handler bodies read exactly as they did
## inside sem.nim.

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std / [tables, sets, assertions]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import nimony_model, builtintypes, decls, asthelpers, programs, sigmatch,
  nifconfig, typeprops, semdata, sembasics, semchecks, derefs,
  renderer, langmodes

# --- thin shims forwarding into the sem core via SemContext callbacks ---

proc semExpr(c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag] = {}) =
  c.semExprCB(c, dest, it, flags)

proc commonType(c: var SemContext; dest: var TokenBuf; it: var Item; argBegin: int; expected: TypeCursor) =
  c.commonTypeCB(c, dest, it, argBegin, expected)

proc semLocalTypeImpl(c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext) =
  c.semLocalTypeImplCB(c, dest, n, context, false, SymId(0))

proc semLocalType(c: var SemContext; dest: var TokenBuf; n: var Cursor; context = InLocalDecl): TypeCursor =
  let insertPos = dest.len
  semLocalTypeImpl c, dest, n, context
  result = typeToCursor(c, dest, insertPos)

# --- handlers (moved verbatim from sem.nim) ---

proc getDottedIdent(n: var Cursor): string

proc getDottedIdentAux(n: var Cursor): string =
  if n.isTagLit and n.exprKind == DotX:
    n.into:
      result = getDottedIdent(n)
      let s = takeIdent(n)
      if s == StrId(0) or result == "":
        result = ""
      else:
        result.add(".")
        result.add(pool.strings[s])
  else:
    # treat as atom
    let s = takeIdent(n)
    if s == StrId(0):
      result = ""
    else:
      result = pool.strings[s]

proc getDottedIdent(n: var Cursor): string =
  if n.isTagLit and n.tagId == nifstreams.ErrT:
    n.peekInto:
      result = getDottedIdentAux(n)
  else:
    result = getDottedIdentAux(n)

proc semDefined*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  var info = NoLineInfo
  var orig = default(Cursor)
  var name = ""
  it.n.into:
    info = it.n.info
    orig = it.n
    name = getDottedIdent(it.n)
  if name == "":
    c.buildErr dest, info, "invalid expression for defined: " & asNimCode(orig), orig
  else:
    let isDefined = c.g.config.isDefined(name)
    let beforeExpr = dest.len
    dest.addParLe(if isDefined: TrueX else: FalseX, info)
    dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, dest, it, beforeExpr, expected

proc semDeclared*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  var info = NoLineInfo
  var orig = default(Cursor)
  var nameId = StrId(0)
  it.n.into:
    info = it.n.info
    orig = it.n
    # XXX maybe always type the argument and check for Symbol/errored Ident instead
    let isError = it.n.isTagLit and it.n.tagId == nifstreams.ErrT
    if isError:
      # does not consider module quoted symbols for now
      it.n.peekInto:
        nameId = takeIdent(it.n)
    else:
      nameId = takeIdent(it.n)
  if nameId == StrId(0):
    c.buildErr dest, info, "invalid expression for declared: " & asNimCode(orig), orig
  else:
    let isDeclared = isDeclared(c, nameId)
    let beforeExpr = dest.len
    dest.addParLe(if isDeclared: TrueX else: FalseX, info)
    dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, dest, it, beforeExpr, expected

proc hasError(dest: TokenBuf): bool =
  let errTag = pool.tags.getOrIncl("err")
  var i = 0
  result = false
  while i < dest.len:
    if dest[i].kind == OpenTagKind and dest[i].tagId == errTag:
      result = true
      break
    else:
      inc i

proc semCompiles*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  var info = NoLineInfo
  var hadError = false
  let beforeExpr = dest.len
  it.n.into:
    info = it.n.info

    let oldScope = c.currentScope
    let oldProcRequestsLen = c.procRequests.len
    let oldTypeInstDeclsLen = c.typeInstDecls.len
    let oldInstantiatedFrom = c.instantiatedFrom.len
    let oldInWhen = c.inWhen
    let oldTemplateInstCounter = c.templateInstCounter
    let oldExpanded = c.expanded.len
    let oldIncludeStackLen = c.includeStack.len
    let oldDebugAllowErrors = c.debugAllowErrors
    c.debugAllowErrors = true

    c.openScope()
    var arg = Item(n: it.n, typ: c.types.autoType)
    semExpr(c, dest, arg)

    c.currentScope = oldScope

    hadError = hasError(dest)
    shrink dest, beforeExpr
    c.currentScope = oldScope
    c.procRequests.setLen(oldProcRequestsLen)
    c.typeInstDecls.setLen(oldTypeInstDeclsLen)
    c.instantiatedFrom.setLen(oldInstantiatedFrom)
    c.includeStack.setLen(oldIncludeStackLen)

    c.inWhen = oldInWhen
    c.templateInstCounter = oldTemplateInstCounter
    c.expanded.shrink(oldExpanded)
    c.debugAllowErrors = oldDebugAllowErrors

    skip it.n

  let expected = it.typ
  dest.addParLe(if hadError: FalseX else: TrueX, info)
  dest.addParRi()
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected


proc semAstToStr*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  var info = NoLineInfo
  var astStr = ""
  it.n.into:
    info = it.n.info
    astStr = asNimCode(it.n)
    skip it.n

  let beforeExpr = dest.len
  dest.addStrLit(astStr, info)

  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, dest, it, beforeExpr, expected

proc readBindSymRule(arg: Cursor): string =
  ## Best-effort extraction of the second `bindSym` arg as a rule name.
  ## Accepts an Ident, Symbol, or IntLit (0=brClosed, 1=brOpen, 2=brForceOpen)
  ## form; falls back to "" if it can't be classified, which the caller
  ## treats as `brClosed`.
  case arg.kind
  of Ident:
    result = pool.strings[arg.litId]
  of Symbol:
    var s = pool.syms[arg.symId]
    extractBasename s
    result = s
  of IntLit:
    case pool.integers[arg.intId]
    of 0: result = "brClosed"
    of 1: result = "brOpen"
    of 2: result = "brForceOpen"
    else: result = ""
  else:
    result = ""

proc semBindSymName*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  ## `bindSym(t: var NifBuilder; name: string; rule: BindSymRule = brClosed)` —
  ## sem-time magic for plugins that resolves `name` in the *current*
  ## (plugin module's def-site) scope and rewrites the call to a runtime
  ## helper that appends the resolved symbol(s) into `t`:
  ##
  ##   bindSymHelper(<t expr>, "<NIF text>")
  ##
  ## where the NIF text is either a single fully-qualified symbol (one match)
  ## or a `(cchoice …)` / `(ochoice …)` sym-choice subtree (multiple matches).
  ## `brForceOpen` always wraps in `(ochoice …)`, even for a single match, so
  ## call-site sem augments the candidate set.
  ##
  ## Round-tripping the symbol(s) through text keeps the magic simple — we
  ## only need to splice the `t` argument into the synthesized call; the
  ## actual symbol-token reconstruction happens at plugin runtime via
  ## `parseNifBuffer`.
  var tInfo = NoLineInfo
  var tBuf = createTokenBuf(8)
  var info = NoLineInfo
  var orig = default(Cursor)
  var nameStr = ""
  var ruleName = "brClosed"
  var failed = false
  it.n.into:                              # enter (bindSym
    # First arg: the builder. Capture its tokens so we can re-emit it inside
    # the synthesized call.
    tInfo = it.n.info
    tBuf.addSubtree it.n
    skip it.n

    # Second arg: the name string literal.
    info = it.n.info
    orig = it.n
    if it.n.kind != StrLitKind:
      c.buildErr dest, info,
        "bindSym expects a string literal as the name, got: " & asNimCode(orig), orig
      while it.n.hasMore: skip it.n
      failed = true
    else:
      nameStr = pool.strings[it.n.litId]
      skip it.n                           # consume the StrLitKind

      # Optional third arg: rule (default brClosed).
      if it.n.hasMore:
        let r = readBindSymRule(it.n)
        if r.len > 0:
          ruleName = r
        skip it.n
  if failed: return

  let identifier = pool.strings.getOrIncl(nameStr)
  var choiceBuf = createTokenBuf(8)
  let n = buildSymChoice(c, choiceBuf, identifier, info, FindAll)
  if n == 0:
    c.buildErr dest, info, "bindSym: undeclared identifier: '" & nameStr & "'", orig
    return

  var resolved: seq[SymId] = @[]
  var choice = beginRead(choiceBuf)
  if choice.isSymbol:
    resolved.add choice.symId
  elif choice.isTagLit:
    choice.peekInto:
      while choice.hasMore and choice.isSymbol:
        resolved.add choice.symId
        inc choice
  endRead(choiceBuf)
  if resolved.len == 0:
    c.buildErr dest, info, "bindSym: cannot resolve '" & nameStr & "' to a symbol", orig
    return

  let forceOpen = ruleName == "brForceOpen"
  var nifText = ""
  if resolved.len == 1 and not forceOpen:
    nifText = pool.syms[resolved[0]]
  else:
    let tag = if ruleName == "brClosed": "cchoice" else: "ochoice"
    nifText.add "("
    nifText.add tag
    for s in resolved:
      nifText.add " "
      nifText.add pool.syms[s]
    nifText.add ")"

  # Synthesize: bindSymHelper(<t expr>, "<nifText>") and re-sem so the helper
  # ident binds to `lib/plugins.bindSymHelper` and the call type-checks.
  var synthBuf = createTokenBuf(16 + tBuf.len)
  synthBuf.copyIntoKind CallS, tInfo:
    synthBuf.addIdent "bindSymHelper", tInfo
    synthBuf.addSubtree(beginRead(tBuf))
    synthBuf.addStrLit nifText, info
  synthBuf.addParRi()  # extra closer so the final `inc` after sem doesn't run off
  var inner = Item(n: cursorAt(synthBuf, 0), typ: it.typ)
  semExpr c, dest, inner
  it.typ = inner.typ

proc semBindSym*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  ## `bindSym(name: string; rule: BindSymRule = brClosed): NimNode` — resolve
  ## `name` in the *current* (macro-definition) scope at sem time and replace
  ## the call with either
  ##   newSymNode("<full-sym-name>")           (single match)
  ## or
  ##   newSymChoiceNode(<rule>, ["<sym1>", …]) (multiple matches)
  ## The plugin emits the result as `(sym …)` / `(cchoice …)` / `(ochoice …)`,
  ## which sem at the macro call site treats as already-resolved (closed) or
  ## as an open-overload set (open).
  var info = NoLineInfo
  var orig = default(Cursor)
  var nameStr = ""
  var ruleName = "brClosed"
  var failed = false
  it.n.into:                              # enter (bindSym
    info = it.n.info
    orig = it.n
    if it.n.kind != StrLitKind:
      c.buildErr dest, info,
        "bindSym expects a string literal, got: " & asNimCode(orig), orig
      skip it.n
      while it.n.hasMore: skip it.n
      failed = true
    else:
      nameStr = pool.strings[it.n.litId]
      skip it.n                           # consume the StrLitKind

      # Optional second arg: rule (default brClosed).
      if it.n.hasMore:
        let r = readBindSymRule(it.n)
        if r.len > 0:
          ruleName = r
        skip it.n
  if failed: return

  # Look up `nameStr` in the macro's def scope (== current scope here).
  let identifier = pool.strings.getOrIncl(nameStr)
  var choiceBuf = createTokenBuf(8)
  let n = buildSymChoice(c, choiceBuf, identifier, info, FindAll)
  if n == 0:
    c.buildErr dest, info, "bindSym: undeclared identifier: '" & nameStr & "'", orig
    return

  # Collect every matching symbol from the choice buffer. `buildSymChoice`
  # writes either a single Symbol token or `(ochoice sym1 sym2 …)`.
  var resolved: seq[SymId] = @[]
  var choice = beginRead(choiceBuf)
  if choice.isSymbol:
    resolved.add choice.symId
  elif choice.isTagLit:
    choice.peekInto:
      while choice.hasMore and choice.isSymbol:
        resolved.add choice.symId
        inc choice
  endRead(choiceBuf)
  if resolved.len == 0:
    c.buildErr dest, info, "bindSym: cannot resolve '" & nameStr & "' to a symbol", orig
    return

  # Build the synthesized call in a scratch buffer and re-run it through
  # `semExpr` so the helper Ident binds to `lib/std/macros.newSymNode` /
  # `newSymChoiceNode` and the call is properly typed.
  #
  # Emission rules:
  #   `brClosed`     single → newSymNode; multi → newSymChoiceNode(brClosed, …)
  #   `brOpen`       single → newSymNode; multi → newSymChoiceNode(brOpen, …)
  #   `brForceOpen`  always → newSymChoiceNode(brOpen, …) — even with one match,
  #                  so sem at the call site augments the choice with
  #                  call-site-visible overloads instead of treating it as
  #                  already-resolved.
  let forceOpen = ruleName == "brForceOpen"
  var synthBuf = createTokenBuf(16)
  if resolved.len == 1 and not forceOpen:
    synthBuf.copyIntoKind CallS, info:
      synthBuf.addIdent "newSymNode", info
      synthBuf.addStrLit pool.syms[resolved[0]], info
  else:
    synthBuf.copyIntoKind CallS, info:
      synthBuf.addIdent "newSymChoiceNode", info
      synthBuf.addIdent ruleName, info
      synthBuf.copyIntoKind BracketX, info:
        for s in resolved:
          synthBuf.addStrLit pool.syms[s], info
  synthBuf.addParRi()  # extra closer so the final `inc` after sem doesn't run off
  var inner = Item(n: cursorAt(synthBuf, 0), typ: it.typ)
  semExpr c, dest, inner
  it.typ = inner.typ

proc semIsMainModule*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  skip it.n
  let isMainModule = IsMain in c.moduleFlags
  let beforeExpr = dest.len
  dest.addParLe(if isMainModule: TrueX else: FalseX, info)
  dest.addParRi()
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semEnumToStr*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  copyInto dest, it.n:
    var x = Item(n: it.n, typ: c.types.autoType)

    var exprTokenBuf = createTokenBuf()
    semExpr c, exprTokenBuf, x
    it.n = x.n
    if containsGenericParams(x.typ):
      discard
    else:
      # Only nominal enum types have a per-type `dollar`.TypeName compiler proc.
      # If we don't have a nominal symbol (e.g. the operand is a type class like
      # `ExprKind|StmtKind|TypeKind`), there is no single `$` to call — report
      # instead of dereferencing a non-symbol token.
      let typ = x.typ.skipModifier
      if typ.kind != Symbol:
        shrink dest, beforeExpr
        c.buildErr dest, info,
          "'$' is not available for type <" & typeToString(x.typ) & ">"
        return
      let typeSymId = typ.symId
      let typeName = pool.syms[typeSymId]
      let dollorName = "dollar`." & typeName
      let dollorSymId = pool.syms.getOrIncl(dollorName)
      shrink dest, beforeExpr
      dest.addParLe(CallX, info)
      dest.addSymUse(dollorSymId, info)
    dest.add exprTokenBuf
  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, dest, it, beforeExpr, expected

proc buildLowValue(c: var SemContext; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr dest, typ.info, "cannot get low value of non-type"
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT, AnumT:
      # first field
      let edecl = asEnumDecl(decl.body)
      var field = edecl.body
      inc field, SkipTag
      skip field, SkipType
      if edecl.kind == AnumT:
        skip field, AnyType
      let first = asLocal(field)
      dest.addSymUse(first.name.symId, info)
    of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
       TemplateT, ObjectT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, ArrayT, VarargsT,
       StaticT, TupleT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
       DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, AutoT, SymkindT, TypekindT, TypedescT,
       UntypedT, TypedT, CstringT, PointerT, OrdinalT:
      c.buildErr dest, info, "invalid type for low: " & typeToString(typ)
  of OpenTagKind:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let bits = typebits(c.g.config, bitsCursor.load)
      dest.addParLe(SufX, info)
      let value =
        case bits
        of 8: low(int8).int64
        of 16: low(int16).int64
        of 32: low(int32).int64
        else: low(int64)
      dest.addIntLit(value, info)
      dest.addStrLit("i" & $bits, info)
      dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let bits = typebits(c.g.config, bitsCursor.load)
      dest.addParLe(SufX, info)
      let value = 0'u64
      dest.addUIntLit(value, info)
      dest.addStrLit("u" & $bits, info)
      dest.addParRi()
    of CharT:
      dest.addCharLit('\0', info)
    of RangetypeT:
      var first = typ
      inc first
      let base = first
      skip first
      dest.addParLe(ConvX, info)
      dest.addSubtree base
      dest.addSubtree first
      dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildLowValue(c, dest, index, info)
    of BoolT:
      dest.addParLe(FalseX, info)
      dest.addParRi()
    of FloatT:
      dest.addParLe(NeginfX, info)
      dest.addParRi()
    of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
       TemplateT, ObjectT, EnumT, ProctypeT, VoidT, PtrT, VarargsT,
       StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
       DistinctT, ItertypeT, UarrayT, SetT, AutoT, SymkindT, TypekindT, TypedescT,
       UntypedT, TypedT, CstringT, PointerT, OrdinalT:
      c.buildErr dest, info, "invalid type for low: " & typeToString(typ)
  else:
    c.buildErr dest, info, "invalid type for low: " & typeToString(typ)

proc buildHighValue(c: var SemContext; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr dest, typ.info, "cannot get high value of non-type"
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT, AnumT:
      # last field
      let edecl = asEnumDecl(decl.body)
      var field = edecl.body
      var lastField = field
      field.into:
        skip field, SkipType
        if edecl.kind == AnumT:
          skip field, AnyType
        lastField = field
        while field.hasMore:
          lastField = field
          skip field
      let last = asLocal(lastField)
      dest.addSymUse(last.name.symId, info)
    of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
       TemplateT, ObjectT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, PtrT, ArrayT, VarargsT,
       StaticT, TupleT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
       DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, AutoT, SymkindT, TypekindT, TypedescT,
       UntypedT, TypedT, CstringT, PointerT, OrdinalT:
      c.buildErr dest, info, "invalid type for high: " & typeToString(typ)
  of OpenTagKind:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let bits = typebits(c.g.config, bitsCursor.load)
      dest.addParLe(SufX, info)
      let value =
        case bits
        of 8: high(int8).int64
        of 16: high(int16).int64
        of 32: high(int32).int64
        else: high(int64)
      dest.addIntLit(value, info)
      dest.addStrLit("i" & $bits, info)
      dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let bits = typebits(c.g.config, bitsCursor.load)
      dest.addParLe(SufX, info)
      let value =
        case bits
        of 8: high(uint8).uint64
        of 16: high(uint16).uint64
        of 32: high(uint32).uint64
        else: high(uint64)
      dest.addUIntLit(value, info)
      dest.addStrLit("u" & $bits, info)
      dest.addParRi()
    of CharT:
      dest.addCharLit(high(char), info)
    of RangetypeT:
      var last = typ
      inc last
      let base = last
      skip last
      skip last
      dest.addParLe(ConvX, info)
      dest.addSubtree base
      dest.addSubtree last
      dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildHighValue(c, dest, index, info)
    of BoolT:
      dest.addParLe(TrueX, info)
      dest.addParRi()
    of FloatT:
      dest.addParLe(InfX, info)
      dest.addParRi()
    of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
       TemplateT, ObjectT, EnumT, ProctypeT, VoidT, PtrT, VarargsT,
       StaticT, TupleT, OnumT, AnumT, RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
       DistinctT, ItertypeT, UarrayT, SetT, AutoT, SymkindT, TypekindT, TypedescT,
       UntypedT, TypedT, CstringT, PointerT, OrdinalT:
      c.buildErr dest, info, "invalid type for high: " & typeToString(typ)
  else:
    c.buildErr dest, info, "invalid type for high: " & typeToString(typ)

proc semLow*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  var typ = default(TypeCursor)
  copyInto dest, it.n:
    typ = semLocalType(c, dest, it.n)
  if containsGenericParams(typ):
    discard
  else:
    dest.shrink beforeExpr
    buildLowValue(c, dest, typ, info)
  let expected = it.typ
  var resultType = typ
  if resultType.typeKind == ArrayT:
    inc resultType # skip tag, get to range type
    skip resultType # skip element type, get to range type
    if resultType.typeKind == RangetypeT:
      inc resultType # skip range tag, get to base type
  elif resultType.typeKind == RangetypeT:
    inc resultType # skip tag, get to base type
  it.typ = resultType
  commonType c, dest, it, beforeExpr, expected

proc semHigh*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  var typ = default(TypeCursor)
  copyInto dest, it.n:
    typ = semLocalType(c, dest, it.n)
  if containsGenericParams(typ):
    discard
  else:
    dest.shrink beforeExpr
    buildHighValue(c, dest, typ, info)
  let expected = it.typ
  var resultType = typ
  if resultType.typeKind == ArrayT:
    inc resultType # skip tag
    skip resultType # skip element type, get to range type
    if resultType.typeKind == RangetypeT:
      inc resultType # skip range tag, get to base type
  elif resultType.typeKind == RangetypeT:
    inc resultType # skip tag, get to base type
  it.typ = resultType
  commonType c, dest, it, beforeExpr, expected

proc semVoidHook*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  copyInto dest, it.n:
    it.typ = c.types.autoType
    semExpr c, dest, it
    if it.n.hasMore:
      # hook has 2nd argument:
      it.typ = c.types.autoType
      semExpr c, dest, it
  it.typ = c.types.voidType
  commonType c, dest, it, beforeExpr, expected

proc semDupHook*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  copyInto dest, it.n:
    it.typ = c.types.autoType
    semExpr c, dest, it
  it.typ = skipModifier(it.typ)
  commonType c, dest, it, beforeExpr, expected

proc semDeref*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  let expected = it.typ
  var arg = Item(typ: c.types.autoType)
  copyInto dest, it.n:
    arg.n = it.n
    semExpr c, dest, arg
    it.n = arg.n
  let t = skipModifier(arg.typ)
  case t.typeKind
  of RefT, PtrT:
    it.typ = t
    inc it.typ # get to base type
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT,
     TemplateT, ObjectT, EnumT, ProctypeT, IT, UT, FT, CT, BoolT, VoidT, ArrayT, VarargsT,
     StaticT, TupleT, OnumT, AnumT, MutT, OutT, LentT, SinkT, NiltT, ConceptT,
     DistinctT, ItertypeT, RangetypeT, UarrayT, SetT, AutoT, SymkindT, TypekindT, TypedescT,
     UntypedT, TypedT, CstringT, PointerT, OrdinalT:
    c.buildErr dest, info, "invalid type for deref: " & typeToString(t)
  commonType c, dest, it, beforeExpr, expected

proc semFailed*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  # It is not yet clear how this should work.
  let beforeExpr = dest.len
  let expected = it.typ
  copyInto dest, it.n:
    var arg = Item(n: it.n, typ: c.types.autoType)
    semExpr c, dest, arg
    it.n = arg.n
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semAddr*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  var arg = Item(typ: c.types.autoType)
  copyInto dest, it.n:
    let info = it.n.info
    let beforeArg = dest.len
    arg.n = it.n
    semExpr c, dest, arg
    it.n = arg.n
    let a = cursorAt(dest, beforeArg)
    # `UarrayT` admits internal-only aconstr literals such as
    # `(aconstr (uarray T) e1 …)` emitted by exprexec's ptr-to-nif rule;
    # `addr` then wraps them into a pointer that hexer's nifcgen hoists to
    # an anonymous module-level static. Users can't normally produce a
    # `UarrayT`-typed value (it's a size-unknown internal type), so this
    # arm doesn't widen the addr-of-literal surface for hand-written code.
    if isAddressable(a) or arg.typ.typeKind in {MutT, LentT, UarrayT}:
      endRead dest
    else:
      let asStr = asNimCode(a)
      endRead dest
      dest.shrink beforeArg
      c.buildErr dest, info, "invalid expression for `addr` operation: " & asStr

  it.typ = ptrTypeOf(c, dest, skipModifier(arg.typ))
  commonType c, dest, it, beforeExpr, expected

proc semSizeof*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let expected = it.typ
  copyInto dest, it.n:
    # handle types
    semLocalTypeImpl c, dest, it.n, InLocalDecl
  it.typ = c.types.intType
  commonType c, dest, it, beforeExpr, expected

proc semInclExcl*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  copyInto dest, it.n:
    let typeStart = dest.len
    semLocalTypeImpl c, dest, it.n, InLocalDecl
    let typ = typeToCursor(c, dest, typeStart)
    var op = Item(n: it.n, typ: typ)
    semExpr c, dest, op
    if op.typ.typeKind == SetT:
      inc op.typ
    else:
      c.buildErr dest, info, "expected set type"
    semExpr c, dest, op
    it.n = op.n
  producesVoid c, dest, info, it.typ

proc semInSet*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let info = it.n.info
  let beforeExpr = dest.len
  copyInto dest, it.n:
    let typeStart = dest.len
    semLocalTypeImpl c, dest, it.n, InLocalDecl
    let typ = typeToCursor(c, dest, typeStart)
    var op = Item(n: it.n, typ: typ)
    semExpr c, dest, op
    if op.typ.typeKind == SetT:
      inc op.typ
    else:
      c.buildErr dest, info, "expected set type"
    semExpr c, dest, op
    it.n = op.n
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semCardSet*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  copyInto dest, it.n:
    let typeStart = dest.len
    semLocalTypeImpl c, dest, it.n, InLocalDecl
    let typ = typeToCursor(c, dest, typeStart)
    var op = Item(n: it.n, typ: typ)
    semExpr c, dest, op
    it.n = op.n
  let expected = it.typ
  it.typ = c.types.intType
  commonType c, dest, it, beforeExpr, expected

proc semInstanceof*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  type
    State = enum
      NoSubtype
      LacksRtti
      MaybeSubtype
      AlwaysSubtype

  let info = it.n.info
  let beforeExpr = dest.len
  let expected = it.typ
  var arg = Item(typ: c.types.autoType)
  var beforeType = 0
  var ok = MaybeSubtype
  copyInto dest, it.n:
    arg.n = it.n
    semExpr c, dest, arg
    it.n = arg.n
    # handle types
    beforeType = dest.len
    semLocalTypeImpl c, dest, it.n, InLocalDecl
    if c.routine.inGeneric == 0:
      let t = cursorAt(dest, beforeType)
      if t.isSymbol and arg.typ.isSymbol:
        let xtyp = arg.typ.symId
        let targetSym = t.symId
        ok = NoSubtype
        if xtyp == targetSym:
          # XXX report "always true" here
          ok = AlwaysSubtype
        else:
          let targetBase = skipTypeInstSym(targetSym)
          for xsubtype in inheritanceChain(xtyp):
            let subBase = skipTypeInstSym(xsubtype)
            if subBase == targetBase:
              ok = AlwaysSubtype
              break
        if ok == NoSubtype:
          let xBase = skipTypeInstSym(xtyp)
          for subtype in inheritanceChain(targetSym):
            let subBase = skipTypeInstSym(subtype)
            if xBase == subBase:
              ok = MaybeSubtype
              break
          if not hasRtti(xtyp):
            ok = LacksRtti
      dest.endRead()
  case ok
  of MaybeSubtype, AlwaysSubtype:
    discard
  of NoSubtype, LacksRtti:
    let tstr = asNimCode(cursorAt(dest, beforeType))
    dest.endRead()
    dest.shrink beforeExpr
    if ok == NoSubtype:
      c.buildErr dest, info, "type of " & asNimCode(arg.n) & " is never a subtype of " & tstr
    else:
      c.buildErr dest, info, "base type of " & asNimCode(arg.n) & " is " & tstr & " which lacks RTTI and cannot be used in an `of` check"
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected

proc semInternalTypeName*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  copyInto dest, it.n:
    let typ = semLocalType(c, dest, it.n)
    if containsGenericParams(typ):
      discard
    else:
      let typeName = pool.syms[typ.symId]
      dest.shrink beforeExpr
      dest.addStrLit typeName, info
  let expected = it.typ
  it.typ = c.types.stringType
  commonType c, dest, it, beforeExpr, expected

proc semIs*(c: var SemContext; dest: var TokenBuf; it: var Item) =
  let beforeExpr = dest.len
  let info = it.n.info
  let orig = it.n
  var lhsExpr = orig
  inc lhsExpr
  var rhsExpr = lhsExpr
  skip rhsExpr
  var lhs = Item(typ: c.types.autoType)
  var rhs = default(TypeCursor)
  it.n.into:
    lhs.n = it.n
    semExpr c, dest, lhs
    it.n = lhs.n
    if lhs.typ.typeKind == TypedescT:
      inc lhs.typ

    if it.n.exprKind == TupconstrX:
      it.n.peekInto:
        rhs = semLocalType(c, dest, it.n)
    else:
      rhs = semLocalType(c, dest, it.n)
  dest.shrink beforeExpr # delete LHS and RHS
  if containsGenericParams(lhs.typ) or containsGenericParams(rhs):
    # Keep the unpreprocessed operand expressions so template/generic
    # instantiation can substitute formals and re-run `semIs`. Defer whenever
    # `inGeneric > 0` too: in template bodies operands can still be `untyped`,
    # which would make `containsGenericParams` false and wrongly fold to `false`.
    dest.addParLe(orig.tag, orig.info)
    dest.addSubtree lhsExpr
    dest.addSubtree rhsExpr
    dest.addParRi()
  else:
    var m = createMatch(addr c)
    let matched =
      if isTypeclassConstraint(rhs):
        var formal = rhs
        matchesConstraint(m, formal, lhs.typ)
      else:
        typematch m, rhs, lhs
        isMatchForIs(m, rhs)
    if matched:
      dest.addParPair(TrueX, info)
    else:
      dest.addParPair(FalseX, info)
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, dest, it, beforeExpr, expected
