#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nim 2 compatibility shims.
##
## Lowering pass that rewrites Nim source-level constructs to forms the
## later pipeline understands without per-construct knowledge.
##
## Current responsibilities:
##
## - `varargs[T]` parameter types lower to the corresponding
##   `openArray[T]` instance Symbol.
## - Call sites with a `varargs[T]` formal slot have their trailing
##   flat args bundled into `(hcall toOpenArray.0[I,T] (aconstr (array
##   T (rangetype int 0 N-1)) e₁ … eₙ))` — the same shape sem already
##   produces for explicit `f([1, 2, 3])` against an `openArray[int]`
##   formal, so derefs/hexer/NIFC need no new handling. Empty case
##   yields an empty range `0..-1`. The `toOpenArray.0` generic
##   converter is instantiated via `requestRoutineInstance` so the
##   hcall references the resulting instance SymId (matching sem's
##   normal converter-insertion pattern).
## - Bare `(varargs)` (the `{.varargs.}` pragma form for C importc
##   procs) is left untouched — NIFC's `...` ellipsis is reserved for
##   that case.
##
## XXX cross-module: this rewrites the param's *published* signature,
## so a module importing a `proc f*(xs: varargs[int])` sees an
## `openArray[int]` formal and its sigmatch refuses flat int lists.
## Single-module varargs procs work end-to-end. The cross-module fix
## (defer the type rewrite to a hexer pass so `.s.nif` keeps the
## varargs slot for cross-module sigmatch) needs a way to find or
## synthesize the openArray-shaped struct from hexer — punted for a
## follow-up. See conversation notes in `feedback_varargs_cross_module.md`.
##
## Future responsibilities:
##
## - Implicit generics (`proc f(x: T)` → `proc f[T](x: T)`). Same
##   walking + rewriting machinery applies, so it lives here next to
##   the varargs rewrite.
##
## Included from sem.nim because we need access to `semLocalType` and
## the existing instantiation machinery (`c.procRequests`,
## `c.typeInstDecls`).

# included from sem.nim

const OpenArrayHeadName = "openArray.0." & SystemModuleSuffix

proc compatOpenArrayInstance(c: var SemContext; elemType: Cursor;
                             info: PackedLineInfo): TypeCursor =
  ## Build `(invoke openArray elemType)` and instantiate it. Returns the
  ## resolved Symbol cursor for `openArray[T]`. Idempotent for repeated
  ## calls with the same `elemType` — `semLocalType` consults the type
  ## instantiation cache via the canonical-form key, so duplicate calls
  ## hand back the same instance Symbol.
  var invokeBuf = createTokenBuf(8)
  invokeBuf.addParLe(InvokeT, info)
  invokeBuf.add symToken(pool.syms.getOrIncl(OpenArrayHeadName), info)
  invokeBuf.addSubtree elemType
  invokeBuf.addParRi()
  var scratch = createTokenBuf(16)
  var src = cursorAt(invokeBuf, 0)
  result = semLocalType(c, scratch, src)

proc compatRewriteImpl(c: var SemContext; dest: var TokenBuf; src: var Cursor)

proc compatVarargsElem(paramType: Cursor): Cursor =
  ## If `paramType` is a typed `(varargs T)`, return a cursor at T.
  ## Returns a nil cursor for the bare `(varargs)` pragma form so callers
  ## can leave that case alone.
  if paramType.typeKind != VarargsT: return default(Cursor)
  var elem = paramType
  inc elem
  if elem.kind == ParRi:
    return default(Cursor)
  result = elem

proc compatVarargsHasHint(paramType: Cursor): bool =
  ## True if `(varargs T conv? "hint")` already carries the openArray
  ## mangle hint that `compatRewriteParam` injects. Makes the rewrite
  ## idempotent for template re-instantiation.
  if paramType.typeKind != VarargsT: return false
  var c = paramType
  inc c
  while c.hasMore:
    if c.kind == StringLit: return true
    skip c
  result = false

proc compatIsAlreadyWrapped(src: Cursor): bool =
  ## True if the varargs slot's first arg is a previously-emitted
  ## bundle — a single `(hcall <toOpenArray.0[I,T]-instance> …)`
  ## followed by the call's closing paren. Lets `compatRewriteCall`
  ## short-circuit on re-sem of an already-rewritten template body.
  result = false
  if src.exprKind != HcallX: return
  var c = src
  inc c
  if c.kind != Symbol: return
  let name = pool.syms[c.symId]
  if not name.startsWith("toOpenArray.0."): return
  var probe = src
  skip probe
  result = probe.kind == ParRi

proc compatToOpenArrayTypevars(): (SymId, SymId) =
  ## Return the SymIds of `toOpenArray.0[I, T]`'s typevars in
  ## declaration order. Mirrors `openarrays.nim`'s
  ## `converter toOpenArray*[I, T]…`.
  result = (SymId(0), SymId(0))
  let origin = pool.syms.getOrIncl("toOpenArray.0." & SystemModuleSuffix)
  let res = tryLoadSym(origin)
  if res.status != LacksNothing: return
  let routine = asRoutine(res.decl)
  var tv = routine.typevars
  if tv.substructureKind != TypevarsU: return
  inc tv
  if tv.kind == ParLe and tv.symKind == TypevarY:
    var inner = tv
    inc inner
    result[0] = inner.symId
    skip tv
  if tv.kind == ParLe and tv.symKind == TypevarY:
    var inner = tv
    inc inner
    result[1] = inner.symId

proc compatBundleVarargs(c: var SemContext; dest: var TokenBuf;
                         src: var Cursor; elemType: Cursor;
                         info: PackedLineInfo) =
  ## See module-level docstring.
  var argBufs: seq[TokenBuf] = @[]
  while src.hasCurrentToken and src.kind != ParRi:
    var ab = createTokenBuf(8)
    compatRewriteImpl(c, ab, src)
    argBufs.add ab
  let n = argBufs.len

  var rangeBuf = createTokenBuf(8)
  rangeBuf.addParLe(RangetypeT, info)
  rangeBuf.addSubtree c.types.intType
  rangeBuf.addIntLit(0, info)
  rangeBuf.addIntLit(n - 1, info)
  rangeBuf.addParRi()
  let rangeType = typeToCursor(c, rangeBuf, 0)

  var elemBuf = createTokenBuf(8)
  elemBuf.addSubtree elemType
  let elemTypeC = typeToCursor(c, elemBuf, 0)

  let (iSym, tSym) = compatToOpenArrayTypevars()
  let origin = pool.syms.getOrIncl("toOpenArray.0." & SystemModuleSuffix)
  var inferred = initTable[SymId, Cursor]()
  inferred[iSym] = rangeType
  inferred[tSym] = elemTypeC
  var typeArgs = createTokenBuf(8)
  typeArgs.addSubtree rangeType
  typeArgs.addSubtree elemTypeC
  let inst = c.requestRoutineInstance(origin, typeArgs, inferred, info)

  dest.addParLe(HcallX, info)
  dest.add symToken(inst.targetSym, info)
  dest.addParLe(AconstrX, info)
  dest.addParLe(ArrayT, info)
  dest.addSubtree elemTypeC
  dest.addSubtree rangeType
  dest.addParRi()
  for ab in argBufs:
    dest.add ab
  dest.addParRi()
  dest.addParRi()

proc compatRewriteCall(c: var SemContext; dest: var TokenBuf; src: var Cursor) =
  let info = src.info
  takeToken dest, src
  if not src.hasCurrentToken or src.kind != Symbol:
    while src.hasCurrentToken and src.kind != ParRi:
      compatRewriteImpl(c, dest, src)
    if src.hasCurrentToken and src.kind == ParRi:
      takeParRi dest, src
    return
  let calleeSym = src.symId
  takeToken dest, src
  let res = tryLoadSym(calleeSym)
  var fnParams = default(Cursor)
  if res.status == LacksNothing and isRoutine(res.decl.symKind):
    # Walk to the `params` slot manually rather than via `asRoutine` —
    # `skip` can run off partially-published decls (e.g. proc declared
    # inside an untyped template body). The 4 slots before params are
    # name / export / pattern / typevars.
    var probe = res.decl
    inc probe
    var slotsConsumed = 0
    var depth = 0
    while probe.hasCurrentToken and slotsConsumed < 4:
      case probe.kind
      of ParLe:
        inc depth
        inc probe
      of ParRi:
        if depth == 0: break
        dec depth
        inc probe
        if depth == 0: inc slotsConsumed
      of EofToken:
        break
      else:
        inc probe
        if depth == 0: inc slotsConsumed
    if probe.hasCurrentToken and slotsConsumed == 4:
      fnParams = probe
  if not cursorIsNil(fnParams) and fnParams.kind == ParLe and
      fnParams.substructureKind == ParamsU:
    inc fnParams
    while src.hasCurrentToken and src.kind != ParRi:
      if fnParams.kind == ParRi:
        compatRewriteImpl(c, dest, src)
        continue
      let param = takeLocal(fnParams, SkipFinalParRi)
      let elem = compatVarargsElem(param.typ)
      if not cursorIsNil(elem):
        if compatIsAlreadyWrapped(src):
          compatRewriteImpl(c, dest, src)
        else:
          compatBundleVarargs(c, dest, src, elem, info)
        break
      compatRewriteImpl(c, dest, src)
    while fnParams.kind != ParRi:
      let param = takeLocal(fnParams, SkipFinalParRi)
      let elem = compatVarargsElem(param.typ)
      if not cursorIsNil(elem):
        var dummy = src
        compatBundleVarargs(c, dest, dummy, elem, info)
  else:
    while src.hasCurrentToken and src.kind != ParRi:
      compatRewriteImpl(c, dest, src)
  if src.hasCurrentToken and src.kind == ParRi:
    takeParRi dest, src

proc compatRewriteParam(c: var SemContext; dest: var TokenBuf; src: var Cursor) =
  takeToken dest, src
  takeTree dest, src    # name
  takeTree dest, src    # export marker
  takeTree dest, src    # pragmas

  if src.typeKind == VarargsT and not compatVarargsHasHint(src):
    let info = src.info
    var elem = src
    inc elem
    if elem.kind != ParRi:
      # Keep `(varargs T conv?)` as-is, but append the openArray instance
      # Sym name as a string-literal "mangle hint" so hexer can rewrite
      # `(varargs T … "hint")` → the openArray Sym at codegen time without
      # duplicating sem's `instToString`/`uhashBase36` mangling. The hint
      # is invisible to sigmatch (which only reads T) and to derefs (which
      # only checks the tag), so `.s.nif` stays varargs-shaped — fixes the
      # cross-module sigmatch gap noted in this file's docstring.
      let inst = compatOpenArrayInstance(c, elem, info)
      let hintStr =
        if inst.kind == Symbol: pool.syms[inst.symId]
        else: ""
      dest.addParLe(VarargsT, info)
      var inner = src
      inc inner
      while inner.hasMore:
        dest.addSubtree inner
        skip inner
      dest.addStrLit hintStr, info
      dest.addParRi()
      skip src
    else:
      # Bare `(varargs)` — `{.varargs.}` pragma form on C importc procs;
      # leave it untouched. NIFC handles it via `...` ellipsis.
      takeTree dest, src
  else:
    takeTree dest, src
  if src.kind == ParRi:
    takeParRi dest, src
  else:
    takeTree dest, src
    takeParRi dest, src

proc compatRewriteRoutine(c: var SemContext; dest: var TokenBuf; src: var Cursor) =
  takeToken dest, src
  takeTree dest, src    # name
  takeTree dest, src    # export marker
  takeTree dest, src    # pattern
  takeTree dest, src    # typevars
  if src.substructureKind == ParamsU:
    takeToken dest, src
    while src.kind != ParRi:
      if src.symKind == ParamY:
        compatRewriteParam(c, dest, src)
      else:
        takeTree dest, src
    takeParRi dest, src
  else:
    takeTree dest, src
  takeTree dest, src    # return type
  takeTree dest, src    # pragmas
  takeTree dest, src    # effects
  while src.kind != ParRi:
    compatRewriteImpl(c, dest, src)
  takeParRi dest, src

proc compatRewriteImpl(c: var SemContext; dest: var TokenBuf; src: var Cursor) =
  case src.kind
  of ParLe:
    case src.stmtKind
    of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS:
      compatRewriteRoutine(c, dest, src)
      return
    else: discard
    if src.exprKind in CallKinds or src.stmtKind in CallKindsS:
      compatRewriteCall(c, dest, src)
      return
    takeToken dest, src
    while src.hasMore:
      compatRewriteImpl(c, dest, src)
    if src.kind == ParRi:
      takeParRi dest, src
  of ParRi:
    bug "compatRewriteImpl: unexpected ParRi"
  else:
    takeToken dest, src

proc compatRewrite*(c: var SemContext; buf: var TokenBuf) =
  if buf.len == 0: return
  var src = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  if src.hasCurrentToken and src.kind == ParLe:
    takeToken dest, src
    while src.hasCurrentToken and src.kind != ParRi:
      compatRewriteImpl(c, dest, src)
    if src.hasCurrentToken and src.kind == ParRi:
      takeParRi dest, src
  endRead buf
  buf = ensureMove(dest)
