#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std / [hashes, os, tables, sets, assertions, syncio]
when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
else:
  {.pragma: untyped.}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [symparser, intrinsics]
import ".." / models / tags
import ".." / nimony / [nimony_model, programs, typenav, expreval, xints, decls, builtintypes, sizeof, typeprops, langmodes, typekeys, nifconfig]
import hexer_context, pipeline, dce1, lifter
import  ".." / lib / [stringtrees]

proc skipExportMarker(c: var EContext; n: var Cursor) =
  if n.isDotToken:
    inc n
  elif n.isIdent and pool.strings[n.strId] == "x":
    inc n
  elif n.isTagLit:
    # can now also be `(tag)` or `(tag <bits>)`:
    skip n
  else:
    error c, "expected '.' or 'x' for an export marker: ", n

proc expectSymdef(c: var EContext; n: var Cursor) =
  if not n.isSymbolDef:
    error c, "expected symbol definition, but got: ", n

proc getSymDef(c: var EContext; n: var Cursor): (SymId, NifLineInfo) =
  expectSymdef(c, n)
  result = (n.symId, n.info)
  inc n

proc expectSym(c: var EContext; n: var Cursor) =
  if not n.isSymbol:
    error c, "expected symbol, but got: ", n

proc getSym(c: var EContext; n: var Cursor): (SymId, NifLineInfo) =
  expectSym(c, n)
  result = (n.symId, n.info)
  inc n

proc expectStrLit(c: var EContext; n: var Cursor) =
  if not n.isStringLit:
    error c, "expected string literal, but got: ", n

proc expectIntLit(c: var EContext; n: var Cursor) =
  if not n.isIntLit:
    error c, "expected int literal, but got: ", n

proc add(dest: var TokenBuf; tag: string; info: NifLineInfo) =
  dest.addParLe(tag, info)

proc takeParRi(dest: var TokenBuf; n: var Cursor; start: Cursor) {.inline.} =
  ## Bounded counterpart of the classic `takeParRi dest, n`: emits the close
  ## with the input close's info (`NoLineInfo` if it was elided) and advances
  ## `n` past the whole subtree opened at `start` (Recipe-B leave: `sub` on
  ## descent, `start; skip` to restore the outer bound).
  dest.addParRi(n.endInfo)
  n = start
  skip n

type
  GenPragmas = object
    opened: bool

proc openGenPragmas(): GenPragmas = GenPragmas(opened: false)

proc maybeOpen(dest: var TokenBuf; g: var GenPragmas; info: NifLineInfo) {.inline.} =
  if not g.opened:
    g.opened = true
    dest.addParLe("pragmas", info)

proc addKey(dest: var TokenBuf; g: var GenPragmas; key: string; info: NifLineInfo) =
  maybeOpen dest, g, info
  dest.addParLe(key, info)
  dest.addParRi()

proc addKeyVal(dest: var TokenBuf; g: var GenPragmas; key: string; val: StrId; info: NifLineInfo) =
  maybeOpen dest, g, info
  dest.addParLe(globalTags.registerTag(key), info)
  dest.addStrLit(val, info)
  dest.addParRi()

proc addKeyVal(dest: var TokenBuf; g: var GenPragmas; key: string; val: int64; info: NifLineInfo) =
  maybeOpen dest, g, info
  dest.addParLe(globalTags.registerTag(key), info)
  dest.addIntLit(val, info)
  dest.addParRi()

proc closeGenPragmas(dest: var TokenBuf; g: GenPragmas) =
  if g.opened:
    dest.addParRi()
  else:
    dest.addDotToken()

type
  TraverseMode = enum
    TraverseAll, TraverseInner, TraverseSig, TraverseTopLevel

proc trExpr(c: var EContext; dest: var TokenBuf; n: var Cursor)
proc trStmt(c: var EContext; dest: var TokenBuf; n: var Cursor; mode = TraverseInner)
proc trLocal(c: var EContext; dest: var TokenBuf; n: var Cursor; tag: SymKind; mode: TraverseMode; renameTo: SymId; constRef = false)
proc getCompilerProc(c: var EContext; name: string; isInline=false): string

type
  TypeFlag = enum
    IsTypeBody
    IsPointerOf
    IsNodecl
    IsInheritable
    IsUnion
    IsImportExternal

proc trType(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {})

type
  CollectedPragmas = object
    extern: StrId
    flags: set[PragmaKind]
    align, bits: int64
    header: StrId
    dynlib: StrId
    callConv: CallConv
    intrinsic: IntrinsicOp   ## the `{.instruction: "X".}` / `{.intrinsic: "X".}` row
                             ## (`NoIntrinsicOp` if neither); sem already checked
                             ## it against the signature, so hexer only forwards
    register: StrId          ## `{.register: "rdi".}` — the pinned machine register of a
                             ## param / result / local. Which names exist is arkham's
                             ## question; hexer only carries the string across.
    interrupt: StrId         ## `{.interrupt: "SysTick".}` — the exception/interrupt
                             ## vector this proc handles. Same arrangement as
                             ## `register`: which vectors a part HAS is arkham's
                             ## question, so hexer only carries the name across.

proc parsePragmas(c: var EContext; dest: var TokenBuf; n: var Cursor): CollectedPragmas

proc applicationTag(n: Cursor): string =
  ## `"call"` or `"instr"` for a call-shaped node. Whether something costs an
  ## ABI call must be answerable downstream from the TAG ALONE — with no symbol
  ## resolution and no cross-module load — so the callee is resolved once, here,
  ## and the answer is baked into the tag.
  result = "call"
  if n.isTagLit:
    let callee = sub(n)
    if callee.kind == Symbol:
      let res = tryLoadSym(callee.symId)
      if res.status == LacksNothing and res.decl.symKind.isRoutine:
        let pragmas = asRoutine(res.decl, SkipExclBody).pragmas
        if hasPragma(pragmas, InstructionP) or hasPragma(pragmas, IntrinsicP):
          result = "instr"

proc externKind(p: CollectedPragmas): string =
  if ImportcP in p.flags:
    result = "importc"
  elif ImportcppP in p.flags:
    result = "importcpp"
  elif ExportcP in p.flags:
    result = "exportc"
  else:
    result = ""

proc externPragmas(c: var EContext; dest: var TokenBuf; genPragmas: var GenPragmas;
                   prag: CollectedPragmas; pinfo: NifLineInfo) =
  let extKind = externKind(prag)
  if extKind.len != 0:
    if prag.extern != StrId(0):
      dest.addKeyVal genPragmas, extKind, prag.extern, pinfo
    else:
      dest.addKey genPragmas, extKind, pinfo
  if NodeclP in prag.flags:
    dest.addKey genPragmas, "nodecl", pinfo
  if prag.header != StrId(0):
    dest.addKeyVal genPragmas, "header", prag.header, pinfo
  if prag.dynlib != StrId(0) and prag.flags * {ImportcP, ImportcppP} != {} and
      c.dynlibIsStaticImport:
    # The library name goes into Leng wherever a `dynlib` means a STATIC import
    # — see `dynlibIsStaticImport` for the whole matrix. Only the native backend
    # reads it (it builds the import table itself); `lengc`'s C and LLVM code
    # generators skip the pragma, because their linker resolves the symbol.
    #
    # Deliberately NOT `and c.nativeBackend`, which is what it used to say: a
    # non-main module's `.x.nif` is cached ONCE per nimcache and shared by every
    # project built there, and `nimony n` builds its compile-time plugins with
    # `nimony c` in that same nimcache. Gating on the backend made the file's
    # CONTENT depend on which of the two hexer runs got there first, and when the
    # C one did, arkham refused `system`'s externs in a build that had been green
    # a moment earlier ("`GetStdHandle` names no import library"). Whatever a
    # shared artifact holds has to be the same for both backends.
    dest.addKeyVal genPragmas, "dynlib", prag.dynlib, pinfo

proc trField(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {}) =
  # Translate gfld to fld for NIFC (NIFC only knows fld):
  dest.addParLe(globalTags.registerTag("fld"), n.info)
  n.into:

    expectSymdef(c, n)
    let (s, sinfo) = getSymDef(c, n)
    dest.addSymDef(s, sinfo)

    skipExportMarker c, n

    let pinfo = n.info
    let prag = parsePragmas(c, dest, n)

    var genPragmas = openGenPragmas()
    externPragmas c, dest, genPragmas, prag, pinfo

    if prag.align != 0:
      dest.addKeyVal genPragmas, "align", prag.align, pinfo
    if prag.bits != 0:
      dest.addKeyVal genPragmas, "bits", prag.bits, pinfo
    closeGenPragmas dest, genPragmas

    trType c, dest, n, flags

    skip n # skips value
    dest.addParRi(n.endInfo)

proc ithTupleField(c: var EContext; counter: int, typ: Cursor): SymId {.inline.} =
  #var typ = typ
  pool.syms.getOrIncl("fld." & $counter)
  # & "." & takeMangle(typ, Backend, c.bits))

proc genTupleField(c: var EContext; dest: var TokenBuf; typ: var Cursor; counter: int) =
  dest.addParLe("fld", typ.info)
  let name = ithTupleField(c, counter, typ)
  dest.addSymDef(name, typ.info)
  dest.addDotToken() # pragmas
  c.trType(dest, typ, {})
  dest.addParRi() # "fld"

proc trEnumField(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {}) =
  dest.addParLe(n.cursorTagId, n.info) # efld
  n.into:

    expectSymdef(c, n)
    let (s, sinfo) = getSymDef(c, n)
    dest.addSymDef(s, sinfo)

    skipExportMarker c, n

    skip n # pragmas: must be empty

    skip n # type: must be the enum itself

    n.into: # TupleConstr
      trExpr c, dest, n
      skip n

    dest.addParRi(n.endInfo)

proc genStringType(c: var EContext; dest: var TokenBuf; info: NifLineInfo) =
  # now unused
  let s = pool.syms.getOrIncl(StringName)
  dest.addParLe("type", info)
  dest.addSymDef(s, info)

  dest.addDotToken()
  dest.addParLe("object", info)
  dest.addDotToken()

  when sso:
    dest.addParLe("fld", info)
    let bytesField = pool.syms.getOrIncl(StringBytesField)
    dest.addSymDef(bytesField, info)
    dest.addDotToken()
    dest.addParLe("u", info)
    dest.addIntLit(-1, info)
    dest.addParRi() # "u"
    dest.addParRi() # "fld"

    dest.addParLe("fld", info)
    let moreField = pool.syms.getOrIncl(StringMoreField)
    dest.addSymDef(moreField, info)
    dest.addDotToken()
    dest.addParLe("ptr", info)
    dest.addSymUse(pool.syms.getOrIncl(LongStringName), info)
    dest.addParRi() # "ptr"
    dest.addParRi() # "fld"
  else:
    dest.addParLe("fld", info)
    let strField = pool.syms.getOrIncl(StringAField)
    dest.addSymDef(strField, info)
    dest.addDotToken()
    dest.addParLe("ptr", info)
    dest.addParLe("c", info)
    dest.addIntLit(8, info)
    dest.addParRi() # "c"
    dest.addParRi() # "ptr"
    dest.addParRi() # "fld"

    dest.addParLe("fld", info)
    let lenField = pool.syms.getOrIncl(StringIField)
    dest.addSymDef(lenField, info)
    dest.addDotToken()
    dest.addParLe("i", info)
    dest.addIntLit(-1, info)
    dest.addParRi() # "i"
    dest.addParRi() # "fld"

  dest.addParRi() # "object"
  dest.addParRi() # "type"

proc useStringType(c: var EContext; dest: var TokenBuf; info: NifLineInfo) =
  let s = pool.syms.getOrIncl(StringName)
  dest.addSymUse(s, info)

proc trTupleBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  n.into:
    dest.addParLe("object", info)
    dest.addDotToken()
    var counter = 0
    while n.hasMore:
      if n.substructureKind == KvU:
        n.into: # kv
          skip n # skip name
          genTupleField(c, dest, n, counter)
      else:
        genTupleField(c, dest, n, counter)
      inc counter
    dest.addParRi(n.endInfo)

proc trArrayBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.addParLe(n.cursorTagId, n.info)
  n.into:
    trType c, dest, n
    if n.typeKind == RangetypeT:
      var first, last: int64
      n.into:
        skip n
        expectIntLit c,  n
        first = n.intVal
        inc n
        expectIntLit c, n
        last = n.intVal
        inc n
      dest.addIntLit(last - first + 1, n.endInfo)
    else:
      # should not be possible, but assume length anyway
      trExpr c, dest, n
    dest.addParRi(n.endInfo)

proc trParams(c: var EContext; dest: var TokenBuf; n: var Cursor;
              rewriteRaises = false)

proc trProcTypeBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.addParLe("proctype", n.info)
  # NIFC proctype keeps the proc-decl shape with empty name/export/pattern/typevars
  # slots, so we emit `.` for slot 0 ourselves.
  dest.addDotToken() # name
  # `(proctype <NilTag> ...)` and `(itertype <NilTag> ...)` share the same
  # canonical shape — both project down to NIFC's proctype. The legacy
  # 4-leading-dot proc-decl-shaped layouts (`(proc Name Export Pattern ...)`)
  # have trailing effects/body slots we still need to consume.
  let isCompactTypeForm = n.typeKind in {ProctypeT, ItertypeT}
  # inlined `skipToParams` so the scope handle survives for the close below:
  n.into:
    if isCompactTypeForm:
      skip n # nilability tag
    else:
      skip n # name
      skip n # export marker
      skip n # pattern
      skip n # generics
    trParams c, dest, n, rewriteRaises = true

    let pinfo = n.info
    let prag = parsePragmas(c, dest, n)
    var genPragmas = openGenPragmas()
    if prag.callConv != NoCallConv:
      let name = $prag.callConv
      addKey dest, genPragmas, name, pinfo
    closeGenPragmas dest, genPragmas

    # ignore effects and body slots only present in proc-decl-shaped layouts.
    if not isCompactTypeForm:
      if n.hasMore:
        skip n
        if n.hasMore:
          skip n
    dest.addParRi(n.endInfo)

proc trRefBody(c: var EContext; dest: var TokenBuf; n: var Cursor; key: string) =
  # We translate `ref T` to:
  # ptr OuterT;
  # OuterT = object
  #  r: int
  #  d: T
  # This means `deref x` becomes `x->d` and `x.field` becomes `x->d.field`
  # `cast` must also be adjusted by the offset of `d` within `OuterT` but this seems
  # to be optional.

  let info = n.info
  inc n
  dest.addParLe("object", info)
  dest.addDotToken()

  dest.addParLe("fld", info)
  let rcField = pool.syms.getOrIncl(RcField)
  dest.addSymDef(rcField, info)
  dest.addDotToken() # pragmas
  dest.addParLe("i", info)
  dest.addIntLit(-1, info)
  dest.addParRi() # "i"
  dest.addParRi() # "fld"

  let dataField = pool.syms.getOrIncl(DataField)
  dest.addParLe("fld", info)
  dest.addSymDef(dataField, info)
  dest.addDotToken() # pragmas
  trType c, dest, n, {}
  dest.addParRi() # "fld"

  dest.addParRi() # "object"

proc trAsNamedType(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var body = n
  let k = body.typeKind
  let key: string
  key = takeMangle(n, Backend, c.bits)

  var val = c.newTypes.getOrDefault(key)
  if val == SymId(0):
    val = pool.syms.getOrIncl(genericTypeName(key, c.main))
    c.newTypes[key] = val

    var buf = createTokenBuf(30)
    swap dest, buf

    dest.addParLe("type", info)
    dest.addSymDef(val, info)

    dest.addDotToken()
    case k
    of TupleT, ClosureTupleT:
      trTupleBody c, dest, body
    of ArrayT:
      trArrayBody c, dest, body
    of RoutineTypes:
      trProcTypeBody c, dest, body
    of RefT:
      trRefBody c, dest, body, key
    else:
      error c, "expected tuple or array, but got: ", body
    dest.addParRi() # "type"

    swap dest, buf
    c.pending.add buf
    # No `programs.publish` here: nifcgen is the last hexer stage, so
    # nothing downstream queries these synthesized type decls via
    # `tryLoadSym`. nifcgen's own `trType` only consults the decl to
    # detect `distinct` types, which synthesized object decls are never.
  # regardless of what we had to do, we still need to add the typename:
  if k == RefT:
    dest.addParLe("ptr", info)
    dest.addSymUse(val, info)
    dest.addParRi()
  else:
    dest.addSymUse(val, info)

proc addRttiField(c: var EContext; dest: var TokenBuf; info: NifLineInfo) =
  dest.addParLe("fld", info)
  dest.addSymDef(pool.syms.getOrIncl(VTableField), info)
  dest.addEmpty() # pragmas
  dest.addParLe PtrT, info
  let rttiSym = pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix)
  dest.addSymUse rttiSym, info
  dest.addParRi() # "ptr"
  dest.addParRi() # "fld"

proc trObjFields(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag])

proc trBranchRanges(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  ## Copy the `(ranges ...)` selector list of an `of` branch. Shared by the
  ## `case` statement (`trCase`) and the case-object lowering in `trObjFields`,
  ## so both spell branch values identically. Note `trExpr` folds an enum
  ## symbol to its integer value, so the output carries numbers, not names.
  if n.isTagLit and n.substructureKind == RangesU:
    takeInto dest, n:            # (ranges ...)
      while n.hasMore:
        if n.isTagLit and n.substructureKind == RangeU:
          takeInto dest, n:      # (range lo hi)
            while n.hasMore:
              trExpr c, dest, n
        else:
          trExpr c, dest, n
  else:
    trExpr c, dest, n

proc trBranchBody(c: var EContext; dest: var TokenBuf; n: var Cursor;
                  flags: set[TypeFlag]) =
  ## Emit a branch's fields as an anonymous object, or `.` when the branch
  ## declares none (`of x: nil`). The empty form still records that the
  ## discriminant values of this branch are legal.
  assert n.stmtKind == StmtsS
  n.into:
    if n.exprKind == NilX:
      skip n
      dest.addDotToken
    else:
      dest.addParLe("object", n.endInfo)
      dest.addDotToken  # base type
      trObjFields(c, dest, n, flags)
      dest.addParRi # end of object

proc trObjFields(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag]) =
  while n.hasMore:
    case n.substructureKind
    of FldU, GfldU:
      trField(c, dest, n, flags)
    of CaseU:
      # A case object becomes a *discriminated* union: the selector is emitted
      # as an ordinary field and the branches keep the `(ranges ...)` that
      # select them, so debug info can map a discriminant value to its branch
      # (see `doc/leng-spec.md`, UnionBranch). The discriminator is found
      # positionally by consumers - it is the `fld` emitted immediately before
      # the `union`, which the two statements below establish.
      n.into:
        trField(c, dest, n, flags)
        dest.addParLe("union", n.info)
        while n.hasMore:
          case n.substructureKind
          of OfU:
            takeInto dest, n:      # (of ...)
              trBranchRanges c, dest, n
              trBranchBody c, dest, n, flags
          of ElseU:
            takeInto dest, n:      # (else ...)
              trBranchBody c, dest, n, flags
          of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
              TypevarU, StaticTypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU,
              CaseU, StmtsU, ParamsU, PragmasU, EitherU, JoinU,
              UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU,
              GfldU, CallargsU, ForcallU, DeferexpansionU, NeedtypesU, DependencyU, NoSub:
            error "expected `of` or `else` inside `case`"
        dest.addParRi # end of union
    of NilU:
      skip n, SkipFull
    of NotnilU, KvU, VvU, RangeU, RangesU, ParamU, TypevarU,
        StaticTypevarU, EfldU, WhenU, ElifU, ElseU, TypevarsU, OfU, StmtsU,
        ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
        UnpacktupU, ExceptU, FinU, UncheckedU, CallargsU,
        ForcallU, DeferexpansionU, NeedtypesU, DependencyU, NoSub:
      error "illformed AST inside object: ", n

proc pointerTag(n: Cursor): string =
  ## Which Leng pointer a `ptr`/`var`/`lent` node becomes: `aptr` when it points at
  ## an `UncheckedArray[T]`, `ptr` otherwise.
  ##
  ## Leng distinguishes the two — `ptr` addresses ONE object, `aptr` an array of
  ## them — and only `aptr` may be indexed or have an offset added to it. That is
  ## exactly the distinction Nim already draws between `ptr T` and
  ## `ptr UncheckedArray[T]`, so the tag is a rename, not a judgement call.
  ##
  ## Without this every pointer left here as `ptr`, and since the `UarrayT` arm of
  ## `trType` erases the `UncheckedArray` layer under `IsPointerOf`, a
  ## `ptr UncheckedArray[T]` came out indistinguishable from a `ptr T`. Arithmetic
  ## on one then produced `(add (ptr T) …)`, which no Leng backend can lower: nifasm
  ## rejects arithmetic on a single-object pointer outright, and lengc's C output
  ## reads it as scaled C pointer arithmetic. `aptr` was never emitted anywhere in
  ## hexer, so the well-typed form was unreachable.
  var probe = n
  probe = sub(probe)
  result = (if probe.typeKind == UarrayT: "aptr" else: "ptr")

proc trType(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {}) =
  case n.kind
  of DotToken:
    dest.addSubtree n
    inc n
  of Symbol:
    let s = n.symId
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      var typeDecl = asTypeDecl(res.decl)
      var body = typeDecl.body
      if body.typeKind == DistinctT: # skips DistinctT
        let prag = parsePragmas(c, dest, typeDecl.pragmas)

        if prag.flags * {ImportcP, ImportcppP} == {}:
          inc body
          trType(c, dest, body, flags)
          inc n
        else:
          dest.addSubtree n
          inc n
      else:
        dest.addSubtree n
        inc n
    else:
      # No decl found means this is a synthesized named type from
      # `trAsNamedType` (e.g. `(ref T)` lowered to a generated object
      # decl). Those are never `distinct`, so we don't need the decl —
      # just emit the symbol; nifc resolves the reference against the
      # type decl that `c.pending` appends to the module's output.
      dest.addSubtree n
      inc n
  of TagLit:
    case n.typeKind
    of NoType, ErrT, OrT, AndT, NotT, TypedescT, UntypedT, TypedT, TypekindT, OrdinalT:
      error c, "type expected but got: ", n
    of IntT, UIntT, FloatT, CharT, BoolT, AutoT, SymkindT:
      takeTree dest, n
    of VarargsT:
      # `(varargs T conv? "openArray.0.I<key>.<mod>")` — Nim 2 typed
      # varargs. The trailing string literal is a mangle hint planted by
      # `semcompat.compatRewriteParam` naming the openArray instance Sym
      # for T. Emit that Sym directly so NIFC sees an openArray-shaped
      # value in the param's type slot; sem instantiates the instance,
      # so the hint resolves to a real decl in `.s.nif`.
      #
      # Bare `(varargs)` — `{.varargs.}` proc pragma form on C importc
      # procs — passes through unchanged so NIFC's `...` ellipsis fires.
      let info = n.info
      var probe = n
      probe = sub(probe)  # throwaway copy; bounds the walk under vpr
      var hint = default(Cursor)
      while probe.hasMore:
        if probe.isStringLit:
          hint = probe
          break
        skip probe
      if cursorIsNil(hint):
        takeTree dest, n
      else:
        let hintSym = pool.syms.getOrIncl(pool.strings[hint.strId])
        dest.addSymUse(hintSym, info)
        skip n
    of MutT, LentT:
      let ptrPos = dest.len
      dest.addParLe(pointerTag(n), n.info)
      let ptrStart = n
      n = sub(n)
      if isViewType(n):
        dest.shrink ptrPos # remove the "ptr" again
        trType c, dest, n, {}
        n = ptrStart; skip n
      else:
        while n.hasMore:
          trType c, dest, n, {IsPointerOf}
        takeParRi dest, n, ptrStart
    of PtrT, OutT:
      dest.addParLe(pointerTag(n), n.info)
      let ptrStart = n
      n = sub(n)
      trType c, dest, n, {IsPointerOf}
      skipNilAnnotation n
      takeParRi dest, n, ptrStart
    of RefT:
      trAsNamedType c, dest, n
    of ArrayT, RoutineTypes:
      if IsNodecl in flags:
        trArrayBody c, dest, n
      else:
        trAsNamedType c, dest, n
    of RangetypeT:
      # skip to base type
      n.into:
        trType c, dest, n
        skip n
        skip n
    of UarrayT:
      if IsPointerOf in flags:
        n.into:
          trType c, dest, n
      else:
        dest.addParLe("flexarray", n.info)
        n.into:
          trType c, dest, n
          dest.addParRi(n.endInfo)
    of PointerT:
      dest.addParLe("ptr", n.info)
      dest.addParLe("void", n.info)
      dest.addParRi()
      let ptrStart = n
      n = sub(n)
      skipNilAnnotation n
      takeParRi dest, n, ptrStart
    of CstringT:
      # `(aptr (c 8))`, not `(ptr (c 8))`: a cstring is indexed (`s[i]`, and every
      # scan in std/strings walks it), which is what Leng's `aptr` means — a pointer
      # to an array of elements. `ptr` addresses ONE object and may not be indexed.
      dest.addParLe("aptr", n.info)
      dest.addParLe($CharT, n.info)
      dest.addIntLit(8, n.info)
      dest.addParRi()
      let ptrStart = n
      n = sub(n)
      skipNilAnnotation n
      takeParRi dest, n, ptrStart
    of StaticT, SinkT, DistinctT:
      n.into:
        trType c, dest, n, flags
    of TupleT, ClosureTupleT:
      # `(closureTuple fn env)` is laid out exactly like the plain tuple it
      # replaced; only the mangled key differs, so it gets its own generated
      # struct decl.
      trAsNamedType c, dest, n
    of ObjectT:
      let isUnion = IsUnion in flags
      if isUnion:
        dest.addParLe("union", n.info)
      else:
        dest.addParLe(n.cursorTagId, n.info)
      n.into:
        if isUnion:
          # Union types don't inherit any types.
          assert n.isDotToken
          inc n
        else:
          if n.isDotToken:
            dest.addSubtree n
            inc n
          else:
            # inherited symbol
            let isPtr = n.typeKind in {RefT, PtrT}
            var s = SymId(0)
            var sinfo = NoLineInfo
            if isPtr:
              n.into:
                (s, sinfo) = getSym(c, n)
                skipNilAnnotation n
            else:
              (s, sinfo) = getSym(c, n)
            dest.addSymUse(s, sinfo)

          if IsInheritable in flags:
            addRttiField c, dest, n.endInfo

        if n.isDotToken:
          dest.addSubtree n
          inc n
        else:
          trObjFields(c, dest, n, flags)

        dest.addParRi(n.endInfo)
    of EnumT, HoleyEnumT, AnumT:
      let enumKind = n.typeKind
      dest.addParLe("enum", n.info)
      n.into:
        trType c, dest, n, flags # base type
        if enumKind == AnumT:
          skip n # owner object type sym

        while n.substructureKind == EfldU:
          trEnumField(c, dest, n, flags)

        dest.addParRi(n.endInfo)
    of SetT:
      let info = n.info
      n.into:
        let sizeOrig = bitsetSizeInBytes(n)
        var err = false
        let size = asSigned(sizeOrig, err)
        if err:
          error c, "invalid set element type: ", n
        else:
          case size
          of 1, 2, 4, 8:
            dest.addParLe("u", info)
            dest.addIntLit(size * 8, info)
            dest.addParRi()
          else:
            var arrBuf = createTokenBuf(16)
            arrBuf.addParLe("array", info)
            arrBuf.addParLe("u", info)
            arrBuf.addIntLit(8, info)
            arrBuf.addParRi()
            arrBuf.addIntLit(size, info)
            arrBuf.addParRi()
            var arrCursor = cursorAt(arrBuf, 0)
            trAsNamedType(c, dest, arrCursor)
        skip n
    of VoidT, NiltT, ConceptT, InvokeT:
      error c, "unimplemented type: ", n
  else:
    error c, "type expected but got: ", n

proc maybeByConstRef(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let param = asLocal(n)
  if param.typ.typeKind in {TypedescT, StaticT}:
    # do not produce any code for this as it's a compile-time parameter
    skip n, SkipFull
  elif passByConstRef(param.typ, param.pragmas, c.bits div 8, c.sizeofCache) or typeprops.isInheritable(param.typ, false):
    var paramBuf = createTokenBuf()
    paramBuf.addParLe("param", n.info)
    paramBuf.addSubtree param.name
    paramBuf.addSubtree param.exported
    paramBuf.addSubtree param.pragmas
    copyIntoKind paramBuf, PtrT, param.typ.info:
      paramBuf.addSubtree param.typ
    paramBuf.addDotToken()
    paramBuf.addParRi()
    var paramCursor = beginRead(paramBuf)
    # `constRef = true`: this pointer exists only because the parameter is
    # passed by reference for efficiency — the SOURCE parameter is by-value, so
    # nothing may write through it. `passByConstRef` already excluded
    # `sink`/`var`/`out`, so that is the precondition which licensed the
    # pointer, not something anyone has to analyse. `funcsummary` reads the
    # `(constref)` pragma back — see `paramMayWrite`.
    trLocal(c, dest, paramCursor, ParamY, TraverseSig, SymId(0), constRef = true)
    skip n
  else:
    trLocal(c, dest, n, ParamY, TraverseSig, SymId(0))

proc trParams(c: var EContext; dest: var TokenBuf; n: var Cursor;
              rewriteRaises = false) =
  if n.isDotToken:
    dest.addSubtree n
    inc n
  elif n.isTagLit and n.substructureKind == ParamsU:
    dest.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        if n.symKind != ParamY:
          error c, "expected (param) but got: ", n
        maybeByConstRef(c, dest, n)
      dest.addParRi(n.endInfo)
  else:
    error c, "expected (params) but got: ", n
  # the result type
  var retType = n
  skip n
  # n is now at the pragmas position:
  if rewriteRaises and hasPragma(n, RaisesP):
    # PROCTYPES only. A raising routine's DECLARATION gets its success tuple
    # from the `eraiser`, which has to run before `cps` — a coroutine's frame
    # and result slot are built out of the return type, and that cannot wait
    # for codegen. Imported routines included: `transformInlineRoutines` runs
    # the whole pipeline over those too.
    #
    # A proctype is different. `(type :Fn . . . (proctype ... (raises)))` in
    # another module is pulled in as a type DECLARATION, not as code, so this
    # is the only pass that ever looks at one. Hence the mapping lives in
    # `builtintypes.addLengReturnType`: several sites, one definition.
    var ret = createTokenBuf(6)
    addLengReturnType(ret, retType, n, NoLineInfo)
    retType = cursorAt(ret, 0)
    trType c, dest, retType
  else:
    trType c, dest, retType

proc parsePragmas(c: var EContext; dest: var TokenBuf; n: var Cursor): CollectedPragmas =
  result = default(CollectedPragmas)
  if n.isDotToken:
    inc n
  elif n.isTagLit and globalTags.tags[n.cursorTagId] == $PragmasS:
    n.into:
      while n.hasMore:
        if n.isTagLit:
          let pk = n.pragmaKind
          case pk
          of NoPragma:
            let cc = n.callConvKind
            if cc == NoCallConv:
              if hookKind(n.cursorTagId) != NoHook:
                skip n
              elif isNilAnnotation(n):
                skip n
              else:
                error c, "unknown pragma: ", n
            else:
              result.callConv = cc
              skip n
          of MagicP:
            n.into:
              if not (n.isStringLit or n.isIdent):
                error c, "expected string literal or ident, but got: ", n
              result.flags.incl MagicP
              inc n
          of ImportcP, ImportcppP, ExportcP:
            n.into:
              expectStrLit c, n
              result.extern = n.strId
              result.flags.incl pk
              inc n
          of AssemblerP, NakedP, StackP:
            # `(assembler)`/`(naked)` on a proc, `(stack)` on a local: bare
            # markers, forwarded as-is.
            result.flags.incl pk
            skip n
          of RegisterP:
            n.into:
              expectStrLit c, n
              result.register = n.strId
              result.flags.incl pk
              inc n
          of InterruptP:
            n.into:
              expectStrLit c, n
              result.interrupt = n.strId
              result.flags.incl pk
              inc n
          of InstructionP, IntrinsicP:
            # sem already resolved and checked the opcode; re-resolve the NAME
            # here so hexer carries the enum rather than a string.
            n.into:
              expectStrLit c, n
              let cls = if pk == InstructionP: icPinned else: icPortable
              result.intrinsic = intrinsicOpByName(pool.strings[n.strId], cls)
              result.flags.incl pk
              inc n
          of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoreturnP,
             VarargsP, NoSideEffectP, NodestroyP, BycopyP, ByrefP,
             InlineP, NoinlineP, AlwaysInlineP, NoinitP, InjectP, GensymP, DirtyP,
             UntypedP, ViewP,
             InheritableP, PureP, AcyclicP, ClosureP, PackedP, UnionP, IncompleteStructP,
             EstablishesBorrowP:
            result.flags.incl pk
            skip n
          of BorrowP:
            result.flags.incl InlineP
            result.flags.incl pk
            skip n
          of HeaderP:
            n.into:
              expectStrLit c, n
              result.header = n.strId
              inc n
          of DynlibP:
            n.into:
              expectStrLit c, n
              result.dynlib = n.strId
              result.flags.incl DynlibP
              inc n
          of AlignP:
            n.into:
              expectIntLit c, n
              result.align = n.intVal
              inc n
          of BitsP:
            n.into:
              expectIntLit c, n
              result.bits = n.intVal
              inc n
          of RequiresP, EnsuresP, StringP, RaisesP, ErrorP, AssumeP, AssertP, ReportP,
             TagsP, DeprecatedP, SideEffectP, KeepOverflowFlagP, SemanticsP,
             BaseP, FinalP, PragmaP, CursorP, PassiveP, PluginP, MethodsP, CastP, SizeP,
             FeatureP, UncheckedAssignP, UncheckedAccessP,
             ProfilerP, StacktraceP, GcsafeP, UsedP:
            skip n
          of BuildP, BundleP, CompileP, EmitP, PushP, PopP, PassLP, PassCP, CallConvP:
            bug "unreachable"
        else:
          error c, "unknown pragma: ", n
  else:
    error c, "(pragmas) or '.' expected, but got: ", n

proc trProcBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  if n.stmtKind == StmtsS:
    dest.addParLe(n.cursorTagId, n.info)
    n.into:
      var prevStmt = NoStmt
      while n.hasMore:
        prevStmt = n.stmtKind
        trStmt c, dest, n, TraverseInner
      if prevStmt == RetS or c.resultSym == SymId(0):
        discard "ok, do not add another return"
      else:
        dest.addParLe(RetS, n.endInfo)
        dest.addSymUse(c.resultSym, n.endInfo)
        dest.addParRi()
      dest.addParRi(n.endInfo)
  else:
    trStmt c, dest, n, TraverseInner

template moveToTopLevel(c: var EContext; dest: var TokenBuf; mode: TraverseMode; body: typed) {.untyped.} =
  if mode in {TraverseAll, TraverseInner}:
    var temp = createTokenBuf()
    swap dest, temp
    body
    swap dest, temp
    c.pending.add temp
  else:
    body

proc makeLocalDeclName(c: var EContext; s: SymId): string =
  # for proc and type decls
  result = pool.syms[s]
  extractBasename(result)
  result.add "."
  result.addInt c.localDeclCounters
  inc c.localDeclCounters
  result.add "."
  result.add c.main

proc makeLocalSymId(c: var EContext; s: SymId): SymId =
  let newName = makeLocalDeclName(c, s)
  result = pool.syms.getOrIncl(newName)

proc trHoistedConst(c: var EContext; dest: var TokenBuf; n: var Cursor; mode: TraverseMode) =
  ## A const that still exists inside a proc body HERE has an aggregate value
  ## — the simple-literal ones were inlined at their use sites by `trExpr`.
  ## Leng keeps no proc-level consts: hoist the decl to the top level (the
  ## `c.pending` tail, like synthesized type decls) under a module-suffixed
  ## name, so the embedded index can serve it — single-dot locals are never
  ## indexed, and whole-program consumers (arkham's foreign loading, ithaqua)
  ## resolve foreign declarations through the index.
  var peek = n
  inc peek                                  # into (const, at the SymbolDef
  let oldSym = peek.symId
  let newSym = makeLocalSymId(c, oldSym)
  c.hoistedConsts[oldSym] = newSym          # decl precedes every use in Nim
  var temp = createTokenBuf(30)
  trLocal c, temp, n, ConstY, mode, newSym
  c.pending.add temp

proc buildProcType(c: var EContext; dest: var TokenBuf; thisProc: Cursor): SymId =
  var thisProc = asRoutine(thisProc)
  var procTypeBuf = createTokenBuf()
  # Build a Nimony-shape proctype (4 slots) since the only consumer here is
  # `trAsNamedType` → `takeMangle` → `mangleProctype`, which expects the
  # compact Nimony layout.
  procTypeBuf.addParLe ProctypeT
  procTypeBuf.addDotToken() # nilability tag
  procTypeBuf.addSubtree thisProc.params
  procTypeBuf.addSubtree thisProc.retType
  procTypeBuf.addSubtree thisProc.pragmas
  procTypeBuf.addParRi() # end of proctype

  var procTypeCursor = beginRead(procTypeBuf)
  var beforeProcPos = dest.len
  trAsNamedType c, dest, procTypeCursor
  var lastTok = dest.len - 1
  while readonlyCursorAt(dest, lastTok).kind in {ExtendedSuffix, LineInfoLit}:
    dec lastTok
  result = readonlyCursorAt(dest, lastTok).symId
  dest.shrink beforeProcPos

proc trProc(c: var EContext; dest: var TokenBuf; n: var Cursor; mode: TraverseMode) =
  let thisProc = n
  c.typeCache.openScope()
  var dst = createTokenBuf(50)
  swap dest, dst
  #let toPatch = c.dest.len
  let oldResultSym = c.resultSym
  c.resultSym = SymId(0)

  let vinfo = n.info
  dest.addParLe("proc", vinfo)
  let procStart = n
  n = sub(n)
  let (s, sinfo) = getSymDef(c, n)
  let newSym = s
  dest.addSymDef(s, sinfo)

  var isGeneric = false
  if n.isTagLit:
    isGeneric = true
  skipExportMarker c, n

  skip n # patterns

  if n.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    n.into:                                     # (typevars ...)
      while n.hasMore:
        assert n.symKind in {TypevarY, StaticTypevarY}
        n.into:                                 # (typevar ...)
          let (typevar, _) = getSymDef(c, n)
          while n.hasMore: skip n
  else:
    skip n, SkipGenParams

  if isGeneric:
    # count each param as used:
    n.into:                                     # (params ...)
      while n.hasMore:
        assert n.symKind == ParamY
        n.into:                                 # (param ...)
          let (param, _) = getSymDef(c, n)
          while n.hasMore: skip n
    skip n, SkipType # return type
  else:
    trParams c, dest, n

  let pinfo = n.info
  let procRaises = hasPragma(n, RaisesP)
  let prag = parsePragmas(c, dest, n)

  var genPragmas = openGenPragmas()

  externPragmas c, dest, genPragmas, prag, pinfo
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    dest.addKey genPragmas, name, pinfo
  if InlineP in prag.flags:
    dest.addKey genPragmas, "inline", pinfo

  if AlwaysInlineP in prag.flags:
    dest.addKey genPragmas, "alwaysInline", pinfo

  if NoinlineP in prag.flags:
    # Must reach the `.c.nif`: `intramodinliner.computeInlineInfo` derives the
    # whole policy from the NIFC proc decl, so a `.noinline` that stops here is
    # a silent no-op — and worse than a no-op, because the cold half of a
    # deliberate hot/cold split gets spliced back into the hot wrapper, leaving
    # a body too big for the wrapper itself to inline.
    dest.addKey genPragmas, "noinline", pinfo

  if SelectanyP in prag.flags:
    dest.addKey genPragmas, "selectany", pinfo

  if prag.intrinsic != NoIntrinsicOp:
    let key = if InstructionP in prag.flags: "instruction" else: "intrinsic"
    dest.addKeyVal genPragmas, key,
      pool.strings.getOrIncl(IntrinsicNames[prag.intrinsic]), pinfo

  if AssemblerP in prag.flags:
    dest.addKey genPragmas, "assembler", pinfo

  if NakedP in prag.flags:
    dest.addKey genPragmas, "naked", pinfo

  if InterruptP in prag.flags and prag.interrupt != StrId(0):
    dest.addKeyVal genPragmas, "interrupt", prag.interrupt, pinfo

  if NoreturnP in prag.flags and not procRaises:
    # Leng has no noreturn pragma of its own; carry the fact as the existing
    # `(attr "noreturn")`. The C backend renders it `__attribute__((noreturn))`
    # (a codegen win in its own right), arkham skips unknown pragmas, and the
    # optimizer's condition-elimination pass reads it to learn facts from the
    # fall-through of assert/panic guards.
    #
    # NOT for `.raises` procs: under goto exceptions a raising "noreturn" proc
    # (raiseOSError) RETURNS at the Leng level — it hands back an error code
    # for the caller to propagate. Telling C it never returns made gcc delete
    # the callers' error paths (a stage-2 boot miscompile), and it would
    # mislead the fall-through learning the same way. Only a proc that
    # genuinely diverges — exits or aborts — may carry the attribute.
    dest.addKeyVal genPragmas, "attr", pool.strings.getOrIncl("noreturn"), pinfo

  closeGenPragmas dest, genPragmas

  skip n # miscPos

  # body:
  if isGeneric:
    skip n, SkipBody
  elif mode != TraverseSig or InlineP in prag.flags:
    trProcBody c, dest, n
  else:
    dest.addDotToken()
    skip n
  takeParRi dest, n, procStart
  swap dst, dest
  # A `dynlib` importc proc IS declared unless the runtime loader is taking it
  # over: that lowering replaces the declaration by a function-pointer global
  # *of the same symbol*, so emitting both would collide. Where the symbol is a
  # static import instead — the native image's own import table, or a linker
  # resolving it from the import library — the declaration is what carries it.
  if MagicP in prag.flags or isGeneric or (c.usesRuntimeDynlibLoader and DynlibP in prag.flags):
    discard "do not add to dest"
  else:
    dest.add dst

  if c.usesRuntimeDynlibLoader and prag.dynlib != StrId(0) and prag.flags * {ImportcP, ImportcppP} != {}:
    # `{.push dynlib: ...}` applies the pragma to *every* proc in scope,
    # including inline helpers that have bodies. Those don't need dynamic
    # symbol loading, and worse, their `prag.extern` is `StrId(0)` which
    # later crashes `initDynlib`'s `pool.strings[val]` lookup. Only emit
    # the dynlib loader stub for procs that actually pull a symbol out of
    # the shared library, i.e. importc/importcpp-marked ones.
    let typeSym = buildProcType(c, dest, thisProc)

    c.dynlibs.mgetOrPut(prag.dynlib, @[]).add (newSym, prag.extern, typeSym)

  c.typeCache.closeScope()
  c.resultSym = oldResultSym

proc trTypeDecl(c: var EContext; dest: var TokenBuf; n: var Cursor; mode: TraverseMode) =
  var dst = createTokenBuf(50)
  swap dest, dst
  #let toPatch = c.dest.len
  let decl = asTypeDecl(n)
  let isDistinct = decl.body.typeKind == DistinctT
  let vinfo = n.info
  dest.addParLe("type", vinfo)
  let typeStart = n
  n = sub(n)
  let (s, sinfo) = getSymDef(c, n)

  let newSym = s
  dest.addSymDef(s, sinfo)

  var isGeneric = n.isTagLit
  skipExportMarker c, n
  if n.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    n.into:                                     # (typevars ...)
      while n.hasMore:
        assert n.symKind in {TypevarY, StaticTypevarY}
        n.into:                                 # (typevar ...)
          let (typevar, _) = getSymDef(c, n)
          while n.hasMore: skip n               # consume rest of body (skipToEnd would eat the parri too)
  else:
    skip n, SkipGenParams

  let pinfo = n.info
  let prag = parsePragmas(c, dest, n)
  var genPragmas = openGenPragmas()

  externPragmas c, dest, genPragmas, prag, pinfo
  if PackedP in prag.flags:
    dest.addKey genPragmas, "packed", pinfo
  closeGenPragmas dest, genPragmas

  if n.typeKind in TypeclassKinds:
    isGeneric = true
  if isGeneric:
    skip n, SkipType
  else:
    var flags = {IsTypeBody}
    if NodeclP in prag.flags: flags.incl IsNodecl
    if InheritableP in prag.flags and PureP notin prag.flags:
      flags.incl IsInheritable
    if UnionP in prag.flags:
      flags.incl IsUnion
    if {ImportcP, ImportcppP} * prag.flags != {}:
      flags.incl IsImportExternal
    trType c, dest, n, flags
  takeParRi dest, n, typeStart
  swap dst, dest
  if isGeneric:
    discard "do not add to dest"
  else:
    dest.add dst

proc genStringLit(c: var EContext; dest: var TokenBuf; s: string; info: NifLineInfo) =
  when sso:
    ## Generate an SSO string literal as an oconstr expression.
    ## Short strings (len <= AlwaysAvail) pack all data inline in `bytes`.
    ## Long strings (len > AlwaysAvail) use StaticSlen sentinel in `bytes`
    ## and emit a static LongString const to strLitBuf, referencing it via addr.
    let alwaysAvail = c.bits div 8 - 1 # 7 on 64-bit, 3 on 32-bit
    let staticSlen = 254'u # StaticSlen sentinel

    let bytesField = pool.syms.getOrIncl(StringBytesField)
    let moreField  = pool.syms.getOrIncl(StringMoreField)

    # Pack up to alwaysAvail chars into the `bytes` uint alongside slen.
    # LE layout: slen at bits 0..7 (LSB/byte0), chars at bits 8, 16, ...
    # BE layout: slen at bits (bits-8)..(bits-1) (MSB/byte0), chars at bits (bits-16), ...
    var bytesVal: uint = 0
    if c.bigEndian:
      if s.len <= alwaysAvail:
        bytesVal = uint(s.len) shl uint(alwaysAvail * 8)
        for i in 0 ..< s.len:
          bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((alwaysAvail - 1 - i) * 8))
      else:
        bytesVal = staticSlen shl uint(alwaysAvail * 8)
        for i in 0 ..< alwaysAvail:
          if i < s.len:
            bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((alwaysAvail - 1 - i) * 8))
    else:
      if s.len <= alwaysAvail:
        # Short string: slen in byte 0, chars in bytes 1..slen
        bytesVal = uint(s.len)
        for i in 0 ..< s.len:
          bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((i + 1) * 8))
      else:
        # Long string: StaticSlen in byte 0, first alwaysAvail chars in bytes 1..
        bytesVal = staticSlen
        for i in 0 ..< alwaysAvail:
          if i < s.len:
            bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((i + 1) * 8))

    dest.addParLe("oconstr", info)
    useStringType c, dest, info

    # (kv bytes <bytesVal>)
    dest.addParLe(KvU, info)
    dest.addSymUse(bytesField, info)
    dest.addUIntLit(bytesVal, info)
    dest.addParRi() # "kv"

    # (kv more nil-or-addr)
    dest.addParLe(KvU, info)
    dest.addSymUse(moreField, info)
    if s.len <= alwaysAvail:
      dest.addParPair(NilX, info)
    else:
      # Reference a static LongString const, named by *content* via the
      # `strlit.0.I<hash>.<module>` instantiation form. Two properties matter:
      #
      # 1. The disambiguator is a hash of the string, NOT a per-module counter.
      #    A counter makes the symbol's number depend on every other literal in
      #    the module, so editing an unrelated literal (or recompiling a module
      #    on its own) renumbers consts that *other* modules' inline-proc copies
      #    already reference by name — a stale cross-module reference that fails
      #    to link. A content hash is stable: the name changes only when the
      #    string does.
      #
      # 2. The instantiation form (`isInstantiation` → true) lets DCE's
      #    `resolveSymbolConflicts` collapse identical strings program-wide: one
      #    module keeps the definition, the rest drop their copies and reference
      #    the winner. This holds in partial builds too (e.g. the compile-time-
      #    eval sub-compile) because the winner is always chosen from the modules
      #    actually in that build, and the cross-module `extern` for it is emitted
      #    by nifc into the `protos` section ahead of any const that takes its
      #    address (see `genForeignDataDecl` in nifc/codegen.nim).
      #
      # `strLits` keeps emission to one const per distinct string per module.
      var litName = c.strLits.getOrDefault(s)
      if litName == SymId(0):
        litName = pool.syms.getOrIncl("strlit.0.I" & $uint64(hash(s)) & "." & c.main)
        c.strLits[s] = litName

        c.strLitBuf.addParLe("const", info)
        c.strLitBuf.addSymDef(litName, info)
        c.strLitBuf.addDotToken() # no pragmas
        # type: LongString
        c.strLitBuf.addSymUse(pool.syms.getOrIncl(LongStringName), info)
        # value: (oconstr LongStringName (kv fullLen len) (kv rc 0) (kv capImpl 0) (kv data "s"))
        c.strLitBuf.addParLe("oconstr", info)
        c.strLitBuf.addSymUse(pool.syms.getOrIncl(LongStringName), info)

        c.strLitBuf.addParLe(KvU, info)
        c.strLitBuf.addSymUse(pool.syms.getOrIncl(LongStringFullLenField), info)
        c.strLitBuf.addIntLit(s.len, info)
        c.strLitBuf.addParRi() # "kv"

        c.strLitBuf.addParLe(KvU, info)
        c.strLitBuf.addSymUse(pool.syms.getOrIncl(LongStringRcField), info)
        c.strLitBuf.addIntLit(0, info)
        c.strLitBuf.addParRi() # "kv"

        c.strLitBuf.addParLe(KvU, info)
        c.strLitBuf.addSymUse(pool.syms.getOrIncl(LongStringCapImplField), info)
        c.strLitBuf.addIntLit(0, info)
        c.strLitBuf.addParRi() # "kv"

        c.strLitBuf.addParLe(KvU, info)
        c.strLitBuf.addSymUse(pool.syms.getOrIncl(LongStringDataField), info)
        c.strLitBuf.addStrLit(s)
        c.strLitBuf.addParRi() # "kv"

        c.strLitBuf.addParRi() # "oconstr"
        c.strLitBuf.addParRi() # "const"

      # Reference the LongString via addr
      dest.addParLe("addr", info)
      dest.addSymUse(litName, info)
      dest.addParRi() # "addr"
    dest.addParRi() # "kv" (more)

    dest.addParRi() # "oconstr"
  else:
    dest.addParLe("oconstr", info)
    useStringType c, dest, info

    dest.addParLe(KvU, info)
    let strField = pool.syms.getOrIncl(StringAField)
    dest.addSymUse(strField, info)
    dest.addStrLit(s)
    dest.addParRi() # "kv"

    dest.addParLe(KvU, info)
    let lenField = pool.syms.getOrIncl(StringIField)
    dest.addSymUse(lenField, info)
    # length also contains the "isConst" flag:
    dest.addIntLit(s.len * 2, info)
    dest.addParRi() # "kv"

    dest.addParRi() # "oconstr"

proc genStringLit(c: var EContext; dest: var TokenBuf; n: Cursor) =
  assert n.isStringLit
  let info = n.info
  let s {.cursor.} = pool.strings[n.strId]
  genStringLit(c, dest, s, info)

proc trStmtsExpr(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let exprStart = n
  n = sub(n)
  if isLastSon(n):
    trExpr c, dest, n
    n = exprStart; skip n, SkipFull
  else:
    dest.addParLe(exprStart.cursorTagId, exprStart.info)
    while n.hasMore:
      if not isLastSon(n):
        trStmt c, dest, n
      else:
        trExpr c, dest, n
    takeParRi dest, n, exprStart

proc trTupleConstr(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.addParLe("oconstr", n.info)
  n.into:
    var tupleType = n
    c.trType(dest, n, {})

    tupleType = sub(tupleType) # parallel walk over the tuple type's fields
    var counter = 0
    while n.hasMore:
      dest.addParLe("kv", n.info)
      if tupleType.substructureKind == KvU:
        tupleType.into:
          skip tupleType # skip key
          dest.addSymUse(ithTupleField(c, counter, tupleType), n.info)
          skip tupleType
      else:
        dest.addSymUse(ithTupleField(c, counter, tupleType), n.info)
        skip tupleType

      inc counter
      if n.substructureKind == KvU:
        n.into:
          skip n # skip key
          trExpr c, dest, n
      else:
        trExpr c, dest, n
      dest.addParRi() # "kv"
    dest.addParRi(n.endInfo)

proc trConv(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let beforeConv = dest.len
  dest.addParLe("conv", info)
  let convStart = n
  n = sub(n)
  let destType = n
  trType(c, dest, n)
  let srcType = getType(c.typeCache, n)
  if destType.typeKind == CstringT and isStringType(srcType):
    var lit = n
    if lit.exprKind == SufX: inc lit
    if lit.isStringLit:
      # evaluate the conversion at compile time:
      dest.shrink beforeConv
      dest.addStrLit pool.strings[lit.strId]
      skip n # the literal, or its whole suffix wrapper
      n = convStart; skip n
    else:
      when sso:
        bug "cannot convert a string to cstring at runtime"
      else:
        let strField = pool.syms.getOrIncl(StringAField)
        dest.addParLe("dot", info)
        trExpr(c, dest, n)
        dest.addSymUse(strField, info)
        dest.addIntLit(0, info)
        dest.addParRi()
        takeParRi dest, n, convStart
  else:
    trExpr(c, dest, n)
    takeParRi dest, n, convStart

proc isSimpleLiteral(nb: var Cursor): bool =
  case nb.kind
  of IntLit, UIntLit, FloatLit, CharLit, StrLit, DotToken:
    result = true
    inc nb
  else:
    case nb.exprKind
    of FalseX, TrueX, InfX, NeginfX, NanX, NilX:
      result = true
      skip nb
    of SufX:
      result = false
      nb.into:
        result = isSimpleLiteral(nb)
        skip nb # type suffix
    of CastX, ConvX:
      result = true
      nb.into:
        skip nb # type
        while nb.hasMore:
          if not isSimpleLiteral(nb): return false
    of ErrX, AtX, DerefX, DotX, PatX, ParX, AddrX, AndX, OrX,
        XorX, NotX, NegX, SizeofX, AlignofX, OffsetofX,
        OconstrX, AconstrX, BracketX, CurlyX, CurlyatX, OvfX,
        AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX,
        BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CallX,
        CmdX, CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX,
        DdotX, HaddrX, NewrefX, NewobjX, TupX, TupconstrX,
        SetconstrX, TabconstrX, AshrX, BaseobjX, HconvX,
        DconvX, CallstrlitX, InfixX, PrefixX, HcallX,
        CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX,
        InstanceofX, ProccallX, HighX, LowX, TypeofX, UnpackX,
        FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
        DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX,
        Delay0X, SuspendX, ExprX, DoX, ArratX, TupatX,
        PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX,
        LtsetX, InsetX, CardX, EmoveX, DestroyX, DupX, CopyX,
        WasmovedX, SinkhX, TraceX, InternalTypeNameX,
        InternalFieldPairsX, FailedX, IsX, EnvpX, KvX, ToClosureX, NoExpr:
      result = false

proc getCompilerProc(c: var EContext; name: string; isInline=false): string =
  result = name & ".0." & SystemModuleSuffix

proc trArrAt(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  # The array-index bound check (and the zero-base `i - lo` adjustment) is
  # normally lowered earlier, in `hexer/desugar.trArrAt`, so the check call
  # can be hoisted by `xelim` and inlined — desugar strips the bound children,
  # so the common path reaches here as a bare `(arrat arr index)` and falls
  # straight through the `n.hasMore` guard below.
  #
  # The fallback that follows is still required: closure-iterator lowering
  # (lambdalifting / coro_transform) splices iterator bodies that never pass
  # through desugar, so a *bounded* `(arrat arr index hi [lo])` can still
  # arrive here. Those get checked inline as before — correct, just not
  # inline-spliceable (rare enough not to matter).
  dest.addParLe(AtX, n.info) # NIFC uses the `at` token for array indexing
  n.into:
    trExpr(c, dest, n)
    let beforeIndex = dest.len
    let info = n.info
    let isUnsigned = getType(c.typeCache, n).typeKind in {UIntT, CharT}
    trExpr(c, dest, n)
    if n.hasMore:
      var indexDest = createTokenBuf(dest.len - beforeIndex)
      # balanced span: raw copy keeps its seals
      for i in beforeIndex..<dest.len:
        indexDest.add dest[i]
      dest.shrink beforeIndex
      let indexB = n
      skip n
      if n.hasMore:
        # we have `low(T)`:
        let indexA = n
        skip n
        if BoundCheck in c.activeChecks:
          let abProcName = getCompilerProc(c, if isUnsigned: "nimUcheckAB" else: "nimIcheckAB", true)
          dest.copyIntoUnchecked "call", info:
            dest.addSymUse(pool.syms.getOrIncl(abProcName), info)
            dest.add indexDest
            dest.addSubtree indexA
            dest.addSubtree indexB
        else:
          let indexType = if isUnsigned: c.typeCache.builtins.uintType else: c.typeCache.builtins.intType
          # we need the substraction regardless:
          dest.addParLe SubX, info
          dest.addSubtree indexType
          dest.add indexDest
          dest.addSubtree indexA
          dest.addParRi()
      else:
        # we only have to care about the upper bound:
        if BoundCheck in c.activeChecks:
          let abProcName = getCompilerProc(c, if isUnsigned: "nimUcheckB" else: "nimIcheckB", true)
          dest.copyIntoUnchecked "call", info:
            dest.addSymUse(pool.syms.getOrIncl(abProcName), info)
            dest.add indexDest
            dest.addSubtree indexB
        else:
          dest.add indexDest
    dest.addParRi(n.endInfo)

proc trFieldname(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  if n.isSymbol:
    dest.addSubtree n
    inc n
  else:
    trExpr c, dest, n

proc trAddrAconstrUarray(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  ## Lowers `(addr (aconstr (uarray T) e1 ... eN))` — the shape exprexec's
  ## ptr-to-nif rule emits for a `ptr UncheckedArray[T]` const-init value.
  ## The aconstr-uarray is a synthetic "inline array literal of unknown
  ## length"; wrapped in `addr` it forms the seq's `data` pointer. Two
  ## things happen here:
  ##   1. The elements are hoisted to an anonymous module-level static
  ##      array (NIFC requires named array types; we go through
  ##      `trAsNamedType` for both the const's type slot and the
  ##      aconstr's). The const decl lands in `strLitBuf` so it
  ##      precedes the user's const-init (which references it).
  ##   2. The expression site emits `(cast (ptr T) (addr <anon>))`. The
  ##      explicit cast is needed because `addr` of an array gives
  ##      `ptr (array T N)` and the receiving field is `ptr T`; the
  ##      `(ptr T)` slot goes through `trType` so a nominal element
  ##      type (tuple, named object) gets lifted too.
  let info = n.info
  var aconstr = n
  aconstr = sub(aconstr) # past `addr` tag
  aconstr = sub(aconstr) # past `aconstr` tag — now at the `(uarray T)`
                              # type slot, bounded to the aconstr's children
  var elemType = aconstr
  inc elemType # past `uarray` tag → element type
  var elemTypeBuf = createTokenBuf(8)
  elemTypeBuf.addSubtree elemType

  # Skip past the uarray type tree to reach the element values, and count them.
  var scan = aconstr
  skip scan
  var elemCount = 0
  block:
    var s = scan
    while s.hasMore:
      inc elemCount
      skip s

  # Build `(array T N)` once and reuse via cursors for both the const's
  # declared type and the inner aconstr's type slot.
  var arrTypeBuf = createTokenBuf(8)
  arrTypeBuf.addParLe("array", info)
  arrTypeBuf.add elemTypeBuf
  arrTypeBuf.addIntLit(elemCount, info)
  arrTypeBuf.addParRi()

  let anonName = pool.syms.getOrIncl("anonArr." & $c.strLitCounter & "." & c.main)
  inc c.strLitCounter

  var constBuf = createTokenBuf(30)
  constBuf.addParLe("const", info)
  constBuf.addSymDef(anonName, info)
  constBuf.addDotToken() # no pragmas
  block:
    var arrCur = cursorAt(arrTypeBuf, 0)
    trAsNamedType(c, constBuf, arrCur)
  constBuf.addParLe("aconstr", info)
  block:
    var arrCur = cursorAt(arrTypeBuf, 0)
    trAsNamedType(c, constBuf, arrCur)
  swap dest, constBuf
  var elemCur = scan
  while elemCur.hasMore:
    trExpr(c, dest, elemCur)
  swap dest, constBuf
  constBuf.addParRi() # close aconstr
  constBuf.addParRi() # close const
  c.strLitBuf.add constBuf

  # Advance caller's cursor past the whole `(addr (aconstr ...))`;
  # `isAddrOfAconstrUarray` already validated the shape.
  skip n

  # Emit `(cast (ptr T) (addr <anon>))` at the original site.
  var ptrTypeBuf = createTokenBuf(8)
  ptrTypeBuf.addParLe("ptr", info)
  ptrTypeBuf.add elemTypeBuf
  ptrTypeBuf.addParRi()
  dest.addParLe("cast", info)
  var ptrTypeCur = cursorAt(ptrTypeBuf, 0)
  trType(c, dest, ptrTypeCur)
  dest.addParLe("addr", info)
  dest.addSymUse(anonName, info)
  dest.addParRi() # close addr
  dest.addParRi() # close cast

proc isAddrOfAconstrUarray(n: Cursor): bool =
  ## True when `n` points at `(addr (aconstr (uarray T) …))`. Used to
  ## detect the static-uarray-pointer shape produced by exprexec.
  var inner = n
  inc inner # past addr tag
  if inner.exprKind == AconstrX:
    var typSlot = inner
    inc typSlot # past aconstr tag
    result = typSlot.typeKind == UarrayT
  else:
    result = false

proc trExpr(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of TagLit:
    case n.exprKind
    of EqX, NeqX, LeX, LtX:
      # `(eq T X X)` in Nimony carries `T`, but NIFC comparisons are `(eq X X)` — see
      # `cmpOp` in llvmgenexprs.nim. Walk `T` with `trType` for side effects, omit from dest.
      takeInto dest, n:
        let beforeType = dest.len
        trType(c, dest, n)
        dest.shrink beforeType
        trExpr(c, dest, n)
        trExpr(c, dest, n)
    of AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX:
      # `(op T X X)` — NIFC typed binops need the type (`signedBinOp` / `unsignedBinOp`).
      takeInto dest, n:
        trType(c, dest, n)
        trExpr(c, dest, n)
        trExpr(c, dest, n)
    of BitnotX:
      # `(bitnot T X)` — NIFC expects type + expr (see BitnotC in llvmgenexprs.nim).
      takeInto dest, n:
        trType(c, dest, n)
        trExpr(c, dest, n)
    of BaseobjX:
      # `(baseobj T INTLIT X)` — keep `T` and depth for NIFC (BaseobjC).
      takeInto dest, n:
        trType(c, dest, n)
        expectIntLit c, n
        dest.addSubtree n
        inc n
        trExpr(c, dest, n)
    of CastX:
      takeInto dest, n:
        trType(c, dest, n)
        trExpr(c, dest, n)
    of HconvX, ConvX:
      trConv c, dest, n
    of DconvX:
      n.into:
        let beforeType = dest.len
        trType(c, dest, n)
        dest.shrink beforeType
        trExpr(c, dest, n)
    of AconstrX:
      dest.addParLe("aconstr", n.info)
      let constrStart = n
      n = sub(n)
      trType(c, dest, n)
      while n.hasMore:
        trExpr(c, dest, n)
      takeParRi dest, n, constrStart
    of OconstrX:
      dest.addParLe("oconstr", n.info)
      let constrStart = n
      n = sub(n)
      trType(c, dest, n)
      while n.hasMore:
        if n.substructureKind == KvU:
          takeInto dest, n: # KvU
            takeTree dest, n # key
            trExpr c, dest, n # value
            if n.hasMore:
              # optional inheritance
              takeTree dest, n
        else:
          trExpr c, dest, n
      takeParRi dest, n, constrStart
    of TupconstrX:
      trTupleConstr c, dest, n
    of CmdX, CallstrlitX, InfixX, PrefixX, HcallX, CallX:
      dest.addParLe(applicationTag(n), n.info)
      n.into:
        while n.hasMore:
          trExpr(c, dest, n)
      dest.addParRi()
    of ExprX:
      trStmtsExpr c, dest, n
    of ArratX:
      trArrAt c, dest, n
    of TupatX:
      let fieldType = getType(c.typeCache, n)
      dest.addParLe("dot", n.info)
      let dotStart = n
      n = sub(n) # skip tag
      trExpr c, dest, n # tuple
      expectIntLit c, n
      dest.addSymUse(ithTupleField(c, int n.intVal, fieldType), n.info)
      inc n # skip index
      dest.addIntLit(0, n.endInfo) # inheritance
      takeParRi dest, n, dotStart
    of DotX:
      dest.addParLe("dot", n.info)
      let dotStart = n
      n = sub(n) # skip tag
      trExpr c, dest, n # obj
      trFieldname c, dest, n # field
      if n.hasMore:
        trExpr c, dest, n # inheritance depth
      if n.isStringLit:
        # drop the access-token marker; NIFC has no visibility concept.
        skip n, SkipFull
      takeParRi dest, n, dotStart
    of DdotX:
      dest.addParLe("dot", n.info)
      dest.addParLe("deref", n.info)
      let dotStart = n
      n = sub(n) # skip tag
      trExpr c, dest, n
      dest.addParRi()
      trFieldname c, dest, n
      trExpr c, dest, n
      if n.isStringLit:
        skip n, SkipFull   # the access-token marker again
      takeParRi dest, n, dotStart
    of HaddrX, AddrX:
      if isAddrOfAconstrUarray(n):
        trAddrAconstrUarray(c, dest, n)
      else:
        # Keep the two apart: `(haddr x)` is the compiler binding x's LOCATION
        # for a `var`/`out` parameter, `(addr x)` is the user turning it into a
        # value. They lower identically, but a back end that can bind a location
        # without materialising a pointer needs to know which it is looking at.
        dest.addParLe((if n.exprKind == HaddrX: "haddr" else: "addr"), n.info)
        n.into:
          trExpr(c, dest, n)
          dest.addParRi(n.endInfo)
    of HderefX, DerefX:
      dest.addParLe("deref", n.info)
      n.into:
        trExpr(c, dest, n)
        dest.addParRi(n.endInfo)
    of SufX:
      var suf = n
      inc suf
      let arg = suf
      skip suf
      assert suf.isStringLit
      if arg.isStringLit:
        # no suffix for string literal in nifc
        n.into:
          if pool.strings[suf.strId] == "C":
            # cstring literal, add string lit directly:
            dest.addSubtree n
            inc n
          else:
            trExpr c, dest, n
          inc n # suf
      else:
        takeInto dest, n:
          trExpr c, dest, n
          dest.addSubtree n
          inc n
    of AshrX:
      dest.addParLe("shr", n.info)
      n.into:
        var bits = -1'i64
        if n.typeKind in {IntT, UIntT}:
          var bitsToken = n
          inc bitsToken
          bits = bitsToken.intVal
        else:
          #error c, "expected int/uint type for ashr, got: ", n
          discard
        trType(c, dest, n)
        dest.copyIntoKind CastX, n.info:
          dest.addParLe("i", n.info)
          dest.addIntLit(bits, n.info)
          dest.addParRi()
          trExpr c, dest, n
        dest.copyIntoKind CastX, n.info:
          dest.addParLe("u", n.info)
          dest.addIntLit(bits, n.info)
          dest.addParRi()
          trExpr c, dest, n
        dest.addParRi(n.endInfo)
    of ErrX, NewobjX, NewrefX, SetconstrX, PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX, LtsetX,
       InsetX, CardX, BracketX, CurlyX, TupX, CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, HighX, LowX, TypeofX, UnpackX,
       FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, DefaultobjX, DefaulttupX, DefaultdistinctX, DoX, CchoiceX, OchoiceX,
       EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, CurlyatX, PragmaxX, QuotedX, TabconstrX,
       InstanceofX, ProccallX, InternalTypeNameX, InternalFieldPairsX, FailedX, IsX, EnvpX, DelayX, Delay0X, SuspendX, ToClosureX:
      error c, "BUG: not eliminated: ", n
      #skip n
    of NilX:
      # `(nil T)` — the frontend types every `nil` (see `trNil` in derefs.nim)
      # and the type slot is a TYPE, not an expression. Keep it: it is what
      # tells `intramodinliner`, which substitutes a literal argument at each
      # use, what the pointer it splices actually is.
      takeInto dest, n:
        if n.hasMore:
          trType(c, dest, n)
          while n.hasMore: skip n   # the closure form's trailing nil environment
    of AtX, PatX, ParX, InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, NotX, NegX, OvfX:
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          trExpr c, dest, n
      dest.addParRi()
    of SizeofX, AlignofX, OffsetofX:
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        trType c, dest, n
        while n.hasMore:
          trExpr c, dest, n
      dest.addParRi()
    of XorX:
      dest.addParLe("neq", n.info)
      n.into:
        while n.hasMore:
          trExpr c, dest, n
      dest.addParRi()
    of KvX:
      takeInto dest, n:
        takeTree dest, n
        trExpr c, dest, n
        if n.hasMore:
          takeTree dest, n
    of NoExpr:
      trType c, dest, n
  of SymbolDef:
    dest.addSubtree n
    inc n
  of Symbol:
    if c.hoistedConsts.hasKey(n.symId):
      dest.addSymUse c.hoistedConsts.getOrQuit(n.symId), n.info
      inc n
      return
    var inlineValue = getInitValue(c.typeCache, n.symId)
    var inlineValueCopy = inlineValue
    if not cursorIsNil(inlineValue) and not inlineValue.isDotToken and isSimpleLiteral(inlineValueCopy):
      trExpr(c, dest, inlineValue)
    else:
      dest.addSubtree n
    inc n
  of StrLit:
    genStringLit c, dest, n
    inc n
  of UnknownToken, DotToken, Ident, CharLit, IntLit, UIntLit, FloatLit:
    dest.addSubtree n
    inc n
  else:
    # classic: a physical ')' or EofToken; nifcore: the suffix kinds — none
    # of which can appear as a cursor head here.
    error c, "BUG: unexpected ')' or EofToken"

proc trLocal(c: var EContext; dest: var TokenBuf; n: var Cursor; tag: SymKind; mode: TraverseMode; renameTo: SymId; constRef = false) =
  var symKind = if tag == ResultY: VarY else: tag
  var localDecl = n
  let toPatch = dest.len
  let vinfo = n.info
  dest.addParLe symKind, vinfo
  n.into:
    let (s0, sinfo) = getSymDef(c, n)
    let s = if renameTo != SymId(0): renameTo else: s0
    if tag == ResultY:
      c.resultSym = s
    skipExportMarker c, n
    let pinfo = n.info
    let prag = parsePragmas(c, dest, n)

    dest.addSymDef(s, sinfo)

    var genPragmas = openGenPragmas()
    if tag != ParamY:
      externPragmas c, dest, genPragmas, prag, pinfo

    if ThreadvarP in prag.flags:
      setTagAt(dest, toPatch, globalTags.registerTag("tvar"))
      symKind = TvarY
    elif GlobalP in prag.flags:
      setTagAt(dest, toPatch, globalTags.registerTag("gvar"))
      symKind = GvarY

    if prag.align != 0:
      dest.addKeyVal genPragmas, "align", prag.align, pinfo
    if prag.bits != 0:
      dest.addKeyVal genPragmas, "bits", prag.bits, pinfo
    # Location pins. Legal on a param, the result or a local — the back end
    # decides what they mean per target and rejects a bad one.
    if RegisterP in prag.flags and prag.register != StrId(0):
      dest.addKeyVal genPragmas, "register", prag.register, pinfo
    if StackP in prag.flags:
      dest.addKey genPragmas, "stack", pinfo
    if constRef:
      dest.addKey genPragmas, "constref", pinfo
    closeGenPragmas dest, genPragmas

    let typAt = n
    trType c, dest, n
    # Type queries during traversal walk INPUT trees, which carry the
    # original name — register that; the renamed symbol answers queries
    # against the emitted tree (the hoisted decl itself).
    c.typeCache.registerLocal(s0, symKind, typAt, n)
    if renameTo != SymId(0):
      c.typeCache.registerLocal(renameTo, symKind, typAt, n)

    if mode == TraverseSig:
      if localDecl.substructureKind == ParamU:
        # Parameter decls in NIFC have no dot token for the default value!
        discard
      else:
        # Imported variables don't need initial values.
        dest.addDotToken
      skip n
    else:
      trExpr c, dest, n
    dest.addParRi(n.endInfo)

proc trWhile(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  c.nestedIn.add (WhileS, SymId(0))
  takeInto dest, n:
    trExpr c, dest, n
    trStmt c, dest, n
  let lab = c.nestedIn[^1][1]
  if lab != SymId(0):
    dest.addParLe("lab", info)
    dest.addSymDef(lab, info)
    dest.addParRi()
  discard c.nestedIn.pop()

proc trBlock(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  n.into:
    if n.isDotToken:
      c.nestedIn.add (BlockS, SymId(0))
      inc n
    else:
      let (s, _) = getSymDef(c, n)
      c.nestedIn.add (BlockS, s)
    dest.addParLe("scope", info)
    trStmt c, dest, n
    dest.addParRi(n.endInfo)
  let lab = c.nestedIn[^1][1]
  if lab != SymId(0):
    dest.addParLe("lab", info)
    dest.addSymDef(lab, info)
    dest.addParRi()
  discard c.nestedIn.pop()

proc trBreak(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  n.into:
    if n.isDotToken:
      inc n
      dest.addParLe("break", info)
    else:
      expectSym c, n
      let lab = n.symId
      inc n
      dest.addParLe("jmp", info)
      dest.addSymUse(lab, info)
    dest.addParRi(n.endInfo)

proc trLab(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  ## `(lab :L)` — a Nimony-level merge label (`xelim`'s two-target condition
  ## compiler emits these for short-circuit chains). Leng has the very same
  ## construct, so this is a straight copy with the symbol registered.
  let info = n.info
  n.into:
    let (s, _) = getSymDef(c, n)
    dest.addParLe("lab", info)
    dest.addSymDef(s, info)
    dest.addParRi()

proc trJmp(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  n.into:
    expectSym c, n
    let lab = n.symId
    inc n
    dest.addParLe("jmp", info)
    dest.addSymUse(lab, info)
    dest.addParRi()

proc trIf(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  # (if cond (.. then ..) (.. else ..))
  takeInto dest, n:
    while n.isTagLit and n.substructureKind == ElifU:
      takeInto dest, n: # elif
        trExpr c, dest, n
        trStmt c, dest, n
    if n.isTagLit and n.substructureKind == ElseU:
      takeInto dest, n:
        trStmt c, dest, n

include stringcases

proc trStringCase(c: var EContext; dest: var TokenBuf; n: var Cursor): bool =
  var nb = n
  inc nb
  let selectorType = getType(c.typeCache, nb)
  if isSomeStringType(selectorType):
    transformStringCase(c, dest, n)
    result = true
  else:
    result = false

proc trCase(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  if trStringCase(c, dest, n):
    return
  takeInto dest, n:
    trExpr c, dest, n
    while n.hasMore:
      case n.substructureKind
      of OfU:
        takeInto dest, n:
          trBranchRanges c, dest, n
          trStmt c, dest, n
      of ElseU:
        takeInto dest, n:
          trStmt c, dest, n
      of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
          TypevarU, StaticTypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU, CaseU,
          StmtsU, ParamsU, PragmasU, EitherU, JoinU,
          UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU,
          GfldU, CallargsU, ForcallU, DeferexpansionU, NeedtypesU, DependencyU, NoSub:
        error c, "expected (of) or (else) but got: ", n

proc trKeepovf(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    trExpr c, dest, n # (add ...)
    trExpr c, dest, n # destination

proc trRaise(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  ## A `raise` reaching codegen can only be a routine exit: the `eraiser`
  ## turned every catchable one into a `jmp` to its handler, and the payload it
  ## carries is already the success tuple. The `goto` case that used to live
  ## here moved there with the rest of the `try` lowering.
  let info = n.info
  n.into:
    dest.addParLe RetS, info
    trExpr c, dest, n
    dest.addParRi(n.endInfo)

proc trTry(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  ## Only the `try`/`finally` that `cps` builds for the `corofor` trampoline
  ## still reaches codegen — the `eraiser` lowers every source-level one to
  ## `lab`/`jmp` long before here. It has no handlers and nothing in it raises,
  ## so "run the body, then the finally" is the whole translation.
  let tryStart = n
  n = sub(n)
  trStmt c, dest, n
  if n.substructureKind == ExceptU:
    error c, "BUG: `except` should have been lowered by the eraiser: ", n
  if n.substructureKind == FinU:
    n.into:
      trStmt c, dest, n
  n = tryStart; skip n

proc trStmt(c: var EContext; dest: var TokenBuf; n: var Cursor; mode = TraverseInner) =
  case n.kind
  of DotToken:
    dest.addSubtree n
    inc n
  of TagLit:
    case n.stmtKind
    of LabS: trLab c, dest, n
    of JmpS: trJmp c, dest, n
    of NoStmt:
      if n.cursorTagId == TagId(KeepovfTagId):
        trKeepovf c, dest, n
      else:
        error c, "unknown statement: ", n
    of PragmaxS:
      n.into:
        skip n
        while n.hasMore:
          trStmt c, dest, n, mode
    of StmtsS:
      if mode == TraverseTopLevel:
        n.into:
          while n.hasMore and n.kind != EofToken:
            trStmt c, dest, n, mode
      else:
        takeInto dest, n:
          while n.hasMore:
            trStmt c, dest, n, mode
    of ScopeS:
      c.typeCache.openScope()
      if mode == TraverseTopLevel:
        n.into:
          while n.hasMore and n.kind != EofToken:
            trStmt c, dest, n, mode
      else:
        takeInto dest, n:
          while n.hasMore:
            trStmt c, dest, n, mode
      c.typeCache.closeScope()
    of VarS, LetS, CursorS, PatternvarS:
      trLocal c, dest, n, VarY, mode, SymId(0)
    of ResultS:
      trLocal c, dest, n, ResultY, mode, SymId(0)
    of GvarS, GletS:
      trLocal c, dest, n, GvarY, mode, SymId(0)
    of TvarS, TletS:
      trLocal c, dest, n, TvarY, mode, SymId(0)
    of ConstS:
      if mode == TraverseTopLevel:
        trLocal c, dest, n, ConstY, mode, SymId(0)
      else:
        trHoistedConst c, dest, n, mode
    of CallKindsS:
      dest.addParLe(applicationTag(n), n.info)
      n.into:
        while n.hasMore:
          trExpr c, dest, n
        dest.addParRi(n.endInfo)
    of EmitS, AsmS:
      takeInto dest, n:
        while n.hasMore:
          if n.isStringLit:
            dest.addSubtree n
            inc n
          elif n.exprKind == SufX:
            n.into:
              assert n.isStringLit
              dest.addSubtree n
              while n.hasMore: skip n
          else:
            trExpr c, dest, n
    of AsgnS, RetS:
      takeInto dest, n:
        while n.hasMore:
          trExpr c, dest, n
    of DiscardS:
      let discardToken = n
      let discardStart = n
      n = sub(n)
      if n.isStringLit or n.isDotToken:
        # eliminates discard without side effects
        n = discardStart; skip n, SkipFull
      else:
        dest.addParLe(discardToken.cursorTagId, discardToken.info)
        trExpr c, dest, n
        takeParRi dest, n, discardStart
    of BreakS: trBreak c, dest, n
    of WhileS: trWhile c, dest, n
    of BlockS: trBlock c, dest, n
    of IfS: trIf c, dest, n
    of CaseS: trCase c, dest, n
    of YldS, ForS, CoroforS, InclS, ExclS, DeferS, UnpackdeclS:
      error c, "BUG: not eliminated: ", n
    of TryS:
      trTry c, dest, n
    of RaiseS:
      trRaise c, dest, n
    of FuncS, ProcS, ConverterS, MethodS:
      moveToTopLevel(c, dest, mode):
        trProc c, dest, n, mode
    of ImportS:
      # Collect module suffixes for init proc generation. The body of an
      # `(import …)` is a list of `(kv suffix "path")` pairs (sem emits this
      # paired form so doc-gen has the source path). We only need the suffix.
      n.into:                                   # (import …)
        while n.hasMore:
          if n.isTagLit and n.substructureKind == KvU:
            n.into:                             # (kv …)
              if n.isIdent:
                c.importedModuleSuffixes.add pool.strings[n.strId]
              while n.hasMore: skip n
          else:
            skip n
    of MacroS, TemplateS, IncludeS, FromimportS, ImportexceptS, ExportS, CommentS, IteratorS,
       ImportasS, ExportexceptS, BindS, MixinS, UsingS, StaticstmtS:
      # pure compile-time construct, ignore:
      skip n, SkipFull
    of TypeS:
      moveToTopLevel(c, dest, mode):
        trTypeDecl c, dest, n, mode
    of ContinueS, WhenS:
      error c, "unreachable: ", n
    of PragmasS, AssumeS, AssertS:
      skip n, SkipFull
  else:
    assert n.hasMore
    error c, "statement expected, but got: ", n

proc transformInlineRoutines(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  var swapped = createTokenBuf()
  swap dest, swapped

  var toTransform = createTokenBuf()
  toTransform.copyIntoKind StmtsS, n.info:
    takeTree(toTransform, n)
  var t = beginRead(toTransform)
  var dest = transform(c, t, c.main, c.bits)
  var d = beginRead(dest)

  inc d # skips (stmts

  swap dest, swapped

  trStmt c, dest, d, TraverseSig
  while d.hasMore:
    trStmt c, dest, d, TraverseAll

proc makeOutput(c: var EContext; dest: var TokenBuf; rootInfo: NifLineInfo): TokenBuf =
  # Build the final output with stmts wrapper and includes
  result = createTokenBuf()
  result.addParLe("stmts", rootInfo)

  # Add all the generated content
  result.add dest

  # Close the stmts wrapper
  result.addParRi()

proc libCandidates(s: string; dest: var seq[string]) =
  ## Expand a dynlib name pattern like "libX11.so(|.6)" into the concrete
  ## candidate names to try, mirroring Nim's `system/dynlib.libCandidates`.
  ## A single `(a|b|...)` group is expanded here; recursion handles any
  ## further groups in the resulting names.
  var le = -1
  for i in 0 ..< s.len:
    if s[i] == '(':
      le = i
      break
  var ri = -1
  if le >= 0:
    for i in le+1 ..< s.len:
      if s[i] == ')':
        ri = i
        break
  if le >= 0 and ri > le:
    let prefix = substr(s, 0, le-1)
    let suffix = substr(s, ri+1)
    var start = le+1
    var i = le+1
    while i <= ri:
      if i == ri or s[i] == '|':
        libCandidates(prefix & substr(s, start, i-1) & suffix, dest)
        start = i+1
      inc i
  else:
    dest.add s

proc emitDynlibLoad(dest: var TokenBuf; loadSym, stepSym: SymId;
                    candidates: seq[string]; idx: int; info: NifLineInfo) =
  ## Emit the left-nested load expression for candidates[0..idx]:
  ##   nimDynlibLoadStep(... nimLoadLibrary(c0) ..., c_idx)
  ## so the alternatives are tried in declared order, keeping the first hit.
  if idx <= 0:
    dest.addParLe("call", info)
    dest.addSymUse(loadSym, info)
    dest.addStrLit candidates[0]
    dest.addParRi()
  else:
    dest.addParLe("call", info)
    dest.addSymUse(stepSym, info)
    emitDynlibLoad(dest, loadSym, stepSym, candidates, idx-1, info)
    dest.addStrLit candidates[idx]
    dest.addParRi()

proc initDynlib(c: var EContext; dest: var TokenBuf; initDest: var TokenBuf;
                rootInfo: NifLineInfo) =
  ## Emit the `dynlib` bindings: one handle global per library plus one
  ## function-pointer global per proc pulled from it. Resolving either is CODE
  ## (`nimLoadLibrary` / `nimGetProcAddr`), so the declaration goes to `dest` with
  ## NO value and the resolution to `initDest` as an assignment — the same split
  ## `trToplevel` makes for a module-level `var` with a non-static initializer, and
  ## the reason no backend needs a rule for when a global's initializer runs.
  ##
  ## `initDest` is spliced into the module's init proc BEFORE the module's own
  ## top-level code, so the bindings are resolved before anything can call through
  ## one. (They used to live in the initializer so that a library nothing pulls
  ## from would die with its globals in DCE; as statements they are roots, and the
  ## library is loaded whether or not anything uses it. Teaching the liveness pass
  ## that an `asgn` to a dead global is dead would restore that.)
  for key, vals in c.dynlibs:
    let dynlib = pool.strings[key]
    var tmp = pool.syms.getOrIncl "Dl." & dynlib & "." & $getTmpId(c) & "." & c.main

    # Expand the dynlib name pattern at compile time (e.g. "libX11.so(|.6)"
    # -> ["libX11.so", "libX11.so.6"]) and load the library handle from the
    # first candidate that succeeds. The whole expression lives in `tmp`'s
    # initializer (never referenced at top level), so the library load stays
    # subject to dead-code elimination: if every proc pulled from it is dead,
    # `tmp` is dead too and nothing is loaded.
    var candidates: seq[string] = @[]
    libCandidates(dynlib, candidates)

    let loadSym = pool.syms.getOrIncl(getCompilerProc(c, "nimLoadLibrary", false))
    let stepSym = pool.syms.getOrIncl(getCompilerProc(c, "nimDynlibLoadStep", false))
    let checkSym = pool.syms.getOrIncl(getCompilerProc(c, "nimDynlibCheck", false))

    dest.addParLe("gvar", rootInfo)
    dest.addSymDef(tmp, rootInfo)
    dest.addDotToken()
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("void", rootInfo)
    dest.addParRi()
    dest.addParRi()
    dest.addDotToken()   # no value: the handle is resolved by the init proc
    dest.addParRi()   # close gvar

    # (asgn tmp (call nimDynlibCheck <candidate chain> "<original pattern>"))
    initDest.addParLe("asgn", rootInfo)
    initDest.addSymUse(tmp, rootInfo)
    initDest.addParLe("call", rootInfo)
    initDest.addSymUse(checkSym, rootInfo)
    emitDynlibLoad(initDest, loadSym, stepSym, candidates, candidates.len-1, rootInfo)
    initDest.addStrLit dynlib
    initDest.addParRi()   # close nimDynlibCheck call
    initDest.addParRi()   # close asgn

    # nimGetProcAddr
    for (varName, val, typeSym) in vals:
      let procName = pool.strings[val]
      dest.addParLe("gvar", rootInfo)
      dest.addSymDef(varName, rootInfo)
      dest.addDotToken()
      dest.addSymUse(typeSym, rootInfo)
      dest.addDotToken() # no value: resolved by the init proc, right below
      dest.addParRi()

      # (asgn varName (cast T (call nimGetProcAddr tmp "procName")))
      initDest.addParLe("asgn", rootInfo)
      initDest.addSymUse(varName, rootInfo)
      initDest.addParLe("cast", rootInfo)
      initDest.addSymUse(typeSym, rootInfo)
      initDest.addParLe("call", rootInfo)
      initDest.addSymUse(pool.syms.getOrIncl(getCompilerProc(c, "nimGetProcAddr", false)), rootInfo)
      initDest.addSymUse(tmp, rootInfo) # library
      initDest.addStrLit procName # proc name
      initDest.addParRi()   # close call
      initDest.addParRi()   # close cast
      initDest.addParRi()   # close asgn

proc initProcName(moduleSuffix: string): string =
  "`ini.0." & moduleSuffix

proc genInitProc(c: var EContext; dest: var TokenBuf; rootInfo: NifLineInfo; importedSuffixes: seq[string]) =
  ## Generate an explicit init proc for this module that:
  ## 1. Guards against double-initialization
  ## 2. Calls imported modules' init procs in order
  ## 3. Contains this module's top-level executable code (via a call from NIFC's init section)
  let initSym = pool.syms.getOrIncl(initProcName(c.main))
  let guardSym = pool.syms.getOrIncl("`iniGuard.0." & c.main)

  # Emit the guard variable: (gvar :InitGuard.suffix . (bool) .)
  dest.addParLe("gvar", rootInfo)
  dest.addSymDef(guardSym, rootInfo)
  dest.addDotToken()
  dest.addParLe("bool", rootInfo)
  dest.addParRi()
  dest.addDotToken()
  dest.addParRi()

  # Emit the init proc declaration: (proc NAME (params) RETTYPE PRAGMAS BODY)
  dest.addParLe("proc", rootInfo)
  dest.addSymDef(initSym, rootInfo)
  # params: empty
  dest.addParLe("params", rootInfo)
  dest.addParRi()
  # return type: void
  dest.addDotToken()
  # pragmas:
  dest.addDotToken()
  # body:
  dest.addParLe("stmts", rootInfo)

  # Guard: if InitGuard.suffix: return
  dest.addParLe("if", rootInfo)
  dest.addParLe("elif", rootInfo)
  dest.addSymUse(guardSym, rootInfo)
  dest.addParLe("stmts", rootInfo)
  dest.addParLe("ret", rootInfo)
  dest.addDotToken()
  dest.addParRi() # ret
  dest.addParRi() # stmts
  dest.addParRi() # elif
  dest.addParRi() # if

  # Set guard: (asgn InitGuard.suffix (true))
  dest.addParLe("asgn", rootInfo)
  dest.addSymUse(guardSym, rootInfo)
  dest.addParLe("true", rootInfo)
  dest.addParRi() # true
  dest.addParRi() # asgn

  # Call each imported module's init proc:
  for suffix in importedSuffixes:
    let importInitSym = pool.syms.getOrIncl(initProcName(suffix))
    dest.addParLe("call", rootInfo)
    dest.addSymUse(importInitSym, rootInfo)
    dest.addParRi()

proc genInitProcEnd(c: var EContext; dest: var TokenBuf; rootInfo: NifLineInfo) =
  # Close: stmts, proc
  dest.addParRi() # stmts (body)
  dest.addParRi() # proc

proc genMainProc(c: var EContext; dest: var TokenBuf; rootInfo: NifLineInfo;
                 isWindows: bool) =
  ## Generate cmdCount/cmdLine globals and a C main() wrapper for the main module.
  ## The gvars get exportc pragmas so NIFC defines them with the expected C names.
  ## Symbol names must contain dots to be recognized as Symbol tokens (not Ident) in NIF.
  ##
  ## Windows is the exception: a process entry point there receives no `argc`,
  ## `argv` or `envp` — the OS keeps the command line as one unsplit UTF-16
  ## string behind `GetCommandLineW` and the environment as a block behind
  ## `GetEnvironmentStringsW`. So `main` takes no parameters and the process
  ## vectors are not emitted at all; `std/cmdline` and `std/envvars` ask the
  ## Windows API for those two directly instead.
  let initSym = pool.syms.getOrIncl(initProcName(c.main))

  let ccharSym = pool.syms.getOrIncl("`cchar.0." & c.main)
  let cmdCountSym = pool.syms.getOrIncl("`cmdCount.0." & c.main)
  let cmdLineSym = pool.syms.getOrIncl("`cmdLine.0." & c.main)
  let nimEnvironSym = pool.syms.getOrIncl("`nimEnviron.0." & c.main)
  let argcSym = pool.syms.getOrIncl("`argc.0." & c.main)
  let argvSym = pool.syms.getOrIncl("`argv.0." & c.main)
  let envpSym = pool.syms.getOrIncl("`envp.0." & c.main)

  if not isWindows:
    # Declare a nodecl importc "char" type alias so argv/cmdLine use plain C `char`
    # instead of NC8 (unsigned char). The C standard requires char** for main's argv.
    dest.addParLe("type", rootInfo)
    dest.addSymDef(ccharSym, rootInfo)
    dest.addParLe("pragmas", rootInfo)
    dest.addParLe("importc", rootInfo)
    dest.addStrLit("char", rootInfo)
    dest.addParRi() # importc
    dest.addParLe("nodecl", rootInfo)
    dest.addParRi() # nodecl
    dest.addParRi() # pragmas
    dest.addParLe("i", rootInfo) # body: (i 8)
    dest.addIntLit(8, rootInfo)
    dest.addParRi() # i
    dest.addParRi() # type

    # (gvar :cmdCount (pragmas (exportc "cmdCount")) (i 32) .)
    dest.addParLe("gvar", rootInfo)
    dest.addSymDef(cmdCountSym, rootInfo)
    dest.addParLe("pragmas", rootInfo)
    dest.addParLe("exportc", rootInfo)
    dest.addStrLit("cmdCount", rootInfo)
    dest.addParRi() # exportc
    dest.addParRi() # pragmas
    dest.addParLe("i", rootInfo)
    dest.addIntLit(32, rootInfo)
    dest.addParRi() # i
    dest.addDotToken() # no init value
    dest.addParRi() # gvar

    # (gvar :cmdLine (pragmas (exportc "cmdLine")) (ptr (ptr cchar)) .)
    dest.addParLe("gvar", rootInfo)
    dest.addSymDef(cmdLineSym, rootInfo)
    dest.addParLe("pragmas", rootInfo)
    dest.addParLe("exportc", rootInfo)
    dest.addStrLit("cmdLine", rootInfo)
    dest.addParRi() # exportc
    dest.addParRi() # pragmas
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("ptr", rootInfo)

    dest.addParLe("c", rootInfo)
    dest.addIntLit(8, rootInfo)
    dest.addParRi() # c 8

    dest.addParRi() # inner ptr
    dest.addParRi() # outer ptr
    dest.addDotToken() # no init value
    dest.addParRi() # gvar

    # (gvar :nimEnviron (pragmas (exportc "nimEnviron")) (ptr (ptr cchar)) .)
    # The environment block (`char **`), written by `main` from its 3rd parameter.
    # Distinct from libc's `environ` ON PURPOSE: this same gvar is emitted for the C
    # backend too (codegen is shared), and an exportc `environ` would clash with
    # libc's. The libc-free backend has no `environ`, so std/envvars + std/posix read
    # `nimEnviron` instead under `-d:nimNativeIo` (on the C backend it's dead — those
    # modules keep using libc's `environ`). The native nifasm entry passes the
    # kernel-provided env pointer as main's 3rd arg, mirroring argc/argv.
    dest.addParLe("gvar", rootInfo)
    dest.addSymDef(nimEnvironSym, rootInfo)
    dest.addParLe("pragmas", rootInfo)
    dest.addParLe("exportc", rootInfo)
    dest.addStrLit("nimEnviron", rootInfo)
    dest.addParRi() # exportc
    dest.addParRi() # pragmas
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("c", rootInfo)
    dest.addIntLit(8, rootInfo)
    dest.addParRi() # c 8
    dest.addParRi() # inner ptr
    dest.addParRi() # outer ptr
    dest.addDotToken() # no init value
    dest.addParRi() # gvar

  # Generate: (proc :main (params (param :argc . (i 32)) (param :argv . (ptr (ptr cchar))) (param :envp . (ptr (ptr cchar)))) (i 32) (pragmas (exportc "main")) (stmts ...))
  # On Windows the params list is empty — nothing is handed to the entry point
  # there, so there is nothing to name.
  let mainSym = pool.syms.getOrIncl("`main.0." & c.main)
  dest.addParLe("proc", rootInfo)
  dest.addSymDef(mainSym, rootInfo)
  # params
  dest.addParLe("params", rootInfo)
  if not isWindows:
    # (param :argc . (i 32))
    dest.addParLe("param", rootInfo)
    dest.addSymDef(argcSym, rootInfo)
    dest.addDotToken()
    dest.addParLe("i", rootInfo)
    dest.addIntLit(32, rootInfo)
    dest.addParRi() # i
    dest.addParRi() # param
    # (param :argv . (ptr (ptr cchar)))
    dest.addParLe("param", rootInfo)
    dest.addSymDef(argvSym, rootInfo)
    dest.addDotToken()
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("ptr", rootInfo)
    dest.addSymUse(ccharSym, rootInfo)
    dest.addParRi() # inner ptr
    dest.addParRi() # outer ptr
    dest.addParRi() # param
    # (param :envp . (ptr (ptr cchar)))  — the environment block (3rd C-main arg)
    dest.addParLe("param", rootInfo)
    dest.addSymDef(envpSym, rootInfo)
    dest.addDotToken()
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("ptr", rootInfo)
    dest.addSymUse(ccharSym, rootInfo)
    dest.addParRi() # inner ptr
    dest.addParRi() # outer ptr
    dest.addParRi() # param
  dest.addParRi() # params
  # return type: (i 32)
  dest.addParLe("i", rootInfo)
  dest.addIntLit(32, rootInfo)
  dest.addParRi() # i
  # pragmas: (pragmas (exportc "main"))
  dest.addParLe("pragmas", rootInfo)
  dest.addParLe("exportc", rootInfo)
  dest.addStrLit("main", rootInfo)
  dest.addParRi() # exportc
  dest.addParRi() # pragmas
  # body
  dest.addParLe("stmts", rootInfo)
  if not isWindows:
    # (asgn cmdCount argc)
    dest.addParLe("asgn", rootInfo)
    dest.addSymUse(cmdCountSym, rootInfo)
    dest.addSymUse(argcSym, rootInfo)
    dest.addParRi() # asgn
    # (asgn cmdLine argv)
    dest.addParLe("asgn", rootInfo)
    dest.addSymUse(cmdLineSym, rootInfo)

    dest.addParLe("cast", rootInfo)
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("c", rootInfo)
    dest.addIntLit(8, rootInfo)
    dest.addParRi() # c 8
    dest.addParRi() # inner ptr
    dest.addParRi() # outer ptr

    dest.addSymUse(argvSym, rootInfo)
    dest.addParRi() # cast

    dest.addParRi() # asgn
    # (asgn nimEnviron (cast (ptr (ptr cchar)) envp))
    dest.addParLe("asgn", rootInfo)
    dest.addSymUse(nimEnvironSym, rootInfo)
    dest.addParLe("cast", rootInfo)
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("ptr", rootInfo)
    dest.addParLe("c", rootInfo)
    dest.addIntLit(8, rootInfo)
    dest.addParRi() # c 8
    dest.addParRi() # inner ptr
    dest.addParRi() # outer ptr
    dest.addSymUse(envpSym, rootInfo)
    dest.addParRi() # cast
    dest.addParRi() # asgn
  # (call ini.0.modname)
  dest.addParLe("call", rootInfo)
  dest.addSymUse(initSym, rootInfo)
  dest.addParRi() # call
  if c.nativeBackend:
    # Native image: terminate through system's `cExit(0)` — a DECLARED
    # cross-module call that flushes the std streams and then reaches
    # `ExitProcess` (windows) / `_exit` (linux) via the ordinary import
    # path. The backend synthesizes no process-exit of its own, so it needs
    # no OS-specific import knowledge for the entry.
    dest.addParLe("call", rootInfo)
    dest.addSymUse(pool.syms.getOrIncl(getCompilerProc(c, "cExit")), rootInfo)
    dest.addIntLit(0, rootInfo)
    dest.addParRi() # call
  else:
    # (call nimFlushStdStreams) — flush buffered std streams on normal exit, so
    # output is not lost when `main` returns without going through `quit`. A
    # no-op unless `syncio` installed a flush (e.g. under -d:nimNativeIo).
    dest.addParLe("call", rootInfo)
    dest.addSymUse(pool.syms.getOrIncl(getCompilerProc(c, "nimFlushStdStreams")), rootInfo)
    dest.addParRi() # call
  # (ret 0) — unreachable on the native path (`cExit` is noreturn), kept for a
  # well-formed proc body; the C `main` returns normally.
  dest.addParLe("ret", rootInfo)
  dest.addIntLit(0, rootInfo)
  dest.addParRi() # ret
  dest.addParRi() # stmts
  dest.addParRi() # proc

proc isTopLevelDecl(n: Cursor): bool {.inline.} =
  ## Returns true for declarations that should stay at the top level
  ## (outside the init proc). Everything else is executable code or
  ## local state that belongs inside the init proc.
  n.stmtKind in {ProcS, FuncS, ConverterS, MethodS, TypeS,
    IncludeS, ImportS, FromimportS, ImportexceptS, ExportS,
    ImportasS, ExportexceptS, CommentS, IteratorS,
    BindS, MixinS, UsingS, StaticstmtS,
    ConstS, PragmasS, EmitS}

const RuntimeVarKinds = {VarY, LetY, ResultY, CursorY, PatternvarY, GvarY, GletY, TvarY, TletY}

proc isStaticInitValue(c: var EContext; n: var Cursor): bool =
  ## Can the single tree/token at `n` be laid out as STATIC DATA — bytes (plus
  ## link-time addresses) the loader has in place before any code runs? Advances
  ## past it.
  ##
  ## This is the contract for what may stay on a global's declaration. Anything
  ## else is code, and code has to run somewhere: `trToplevel` moves it into the
  ## module's init proc as an assignment, in source order. A backend then needs no
  ## rule of its own about when a global's initializer runs — arkham enforces
  ## exactly this predicate (`isStaticConstInit`) and rejects a violation, and the
  ## C/LLVM ones stop needing their hoist into a constructor.
  ##
  ## An arithmetic node is deliberately NOT static even when both operands are
  ## literals: C would take `40 + 2` at file scope, but a machine-code backend has
  ## to fold it to emit bytes, and constant folding belongs upstream (nimsem), not
  ## in three backends.
  case n.kind
  of IntLit, UIntLit, FloatLit, CharLit, StrLit, DotToken:
    result = true
    inc n
  of Symbol:
    # A proc (or another compile-time constant) is a link-time ADDRESS. A runtime
    # variable would have to be READ, which only code can do.
    result = c.typeCache.fetchSymKind(n.symId) notin RuntimeVarKinds
    inc n
  of TagLit:
    if n.substructureKind == KvU:                 # (kv field value), inside an oconstr
      result = true
      n.into:
        while n.hasMore:
          let ok = isStaticInitValue(c, n)
          if not ok: result = false
    elif n.exprKind in {TrueX, FalseX, NilX, InfX, NeginfX, NanX}:
      result = true
      skip n
    elif n.exprKind in {AddrX, HaddrX}:
      # The address of a global or proc is fixed at link time whatever it holds, so
      # only the ROOT of the lvalue matters — but a COMPUTED one (`addr a[i]`) is
      # not something a linker can bake, so require a bare symbol.
      result = false
      n.into:
        if n.kind == Symbol: result = true
        while n.hasMore: skip n
    elif n.exprKind in {SufX, ParX, CastX, ConvX, NegX, AconstrX, OconstrX}:
      result = true
      n.into:
        while n.hasMore:
          let ok = isStaticInitValue(c, n)
          if not ok: result = false
    else:
      result = false
      skip n
  else:
    result = false
    inc n

proc initIsStatic(c: var EContext; n: Cursor): bool =
  ## Whether a global var/let decl's initializer may stay on the declaration.
  var n = n
  n = sub(n) # skip the gvar/glet/tvar/tlet tag; peek only
  skip n   # skip SymbolDef
  skip n   # skip export marker
  skip n   # skip pragmas
  skip n   # skip type
  # Now at the init value
  result = isStaticInitValue(c, n)

proc trToplevel(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  ## Consumes the whole `(stmts …)` node at `n`, including its close.
  n.into:
    while n.hasMore:
      let sk = n.stmtKind
      if sk in {GvarS, GletS, TvarS, TletS}:
        let tag = if sk in {TvarS, TletS}: TvarY else: GvarY
        if initIsStatic(c, n):
          # Static data (literal, constructor of literals, a link-time address):
          # keep it on the declaration. NIFC emits "Type var = value;" at C file
          # scope and arkham lays out the bytes for the loader to prefill.
          trLocal c, dest, n, tag, TraverseAll, SymId(0)
        else:
          # Complex init with function calls: emit a no-init declaration at top
          # level and place the actual init as an assignment inside the Init proc
          # body so that any temp variables created by to_stmts remain in scope.
          let savedN = n
          trLocal c, dest, n, tag, TraverseSig, SymId(0)
          var initN = savedN
          inc initN  # past gvar/glet tag -> at SymbolDef
          let (initSym, initInfo) = getSymDef(c, initN)
          skipExportMarker c, initN
          skip initN  # past pragmas -> at type
          skip initN  # past type -> at init value
          swap dest, c.initBody
          dest.addParLe AsgnS, initInfo
          dest.addSymUse(initSym, initInfo)
          trExpr c, dest, initN
          dest.addParRi()
          swap dest, c.initBody
      elif sk == StmtsS:
        # Nested stmts block: recurse to handle mixed decls and executable code
        trToplevel c, dest, n
      elif isTopLevelDecl(n):
        # Pure declarations and compile-time constructs stay at top level:
        trStmt c, dest, n, TraverseTopLevel
      else:
        # Executable code and local vars go into the init proc body:
        swap dest, c.initBody
        trStmt c, dest, n, TraverseAll
        swap dest, c.initBody

proc expand*(infile: string; bits: int; bigEndian: bool; flags: set[CheckMode]; isMain: bool; outdir: string; appType = appConsole; native = false; isWindows = defined(windows)) =
  let mp = splitModulePath(infile)
  let dir =
    if outdir.len > 0: outdir
    elif mp.dir.len == 0:
      try: getCurrentDir()
      except: quit "cannot get current working directory"
    else: mp.dir
  var c = EContext(dir: dir, ext: mp.ext, main: mp.name,
    nestedIn: @[(StmtsS, SymId(0))],
    typeCache: createTypeCache(bits),
    pending: createTokenBuf(),
    strLitBuf: createTokenBuf(),
    bits: bits,
    bigEndian: bigEndian,
    nativeBackend: native,
    isWindows: isWindows,
    localDeclCounters: 1000,
    activeChecks: flags,
    liftingCtx: createLiftingCtx(mp.name, bits)
  )
  c.typeCache.openScope()

  var owningBuf = createTokenBuf(300)

  var c0 = setupProgram(infile, infile.changeModuleExt ".x.nif", owningBuf, true)
  let cBits = c.bits
  var dest = transform(c, c0, mp.name, cBits)

  var n = beginRead(dest)
  let rootInfo = n.info

  var toplevels = createTokenBuf()
  c.initBody = createTokenBuf()
  var cdest = createTokenBuf(300)
  swap cdest, toplevels
  if stmtKind(n) == StmtsS:
    trToplevel c, cdest, n
  else:
    error c, "expected (stmts) but got: ", n
  swap cdest, toplevels

  # The dynlib bindings' RESOLUTION goes into the init proc ahead of the module's
  # own top-level code (`c.initBody`), so nothing can call through an unresolved
  # function pointer; their declarations stay at top level, in `cdest`.
  var dynlibInit = createTokenBuf(64)
  initDynlib(c, cdest, dynlibInit, rootInfo)

  when sso:
    cdest.add c.strLitBuf

  cdest.add toplevels
  cdest.add c.pending

  # Generate the init proc after all other code so NIFC places it last
  # in the C file, after all function definitions it may call.
  let importedSuffixes = c.importedModuleSuffixes
  genInitProc(c, cdest, rootInfo, importedSuffixes)
  cdest.add dynlibInit
  cdest.add c.initBody
  genInitProcEnd(c, cdest, rootInfo)

  if isMain and appType in {appConsole, appGui}:
    genMainProc(c, cdest, rootInfo, isWindows)

  # the module's close was consumed by `trToplevel`
  let destfileName = c.dir / c.main & ".x.nif"

  var outputBuf = makeOutput(c, cdest, rootInfo)
  optimizeLengOutput(outputBuf, c.main, c.bits)
  try:
    writeFile outputBuf, destfileName, OnlyIfChanged
  except:
    quit "could not write file: " & destfileName
  c.typeCache.closeScope()

  # Use the in-memory buffer to avoid re-reading the file we just wrote
  writeDceOutput outputBuf, c.dir / c.main & ".dce.nif", "." & c.main
