#
#
#        Lengc type navigator — nifcore port
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A type navigator that recomputes the type of a Leng expression, over
## **nifcore** cursors. This is the nifcore port of `lengc/typenav.nim`: same
## algorithm, same shared `models/leng_tags` enums, but built on `nifcoreparse`
## / `nifcdecl` and the nifcore `MainModule` (`shoggoth/nifmodules`) so symbol
## resolution is fully cross-module — `MainModule.getDeclOrNil` lazily loads
## foreign declarations on demand.

import std / [assertions, tables]
import ".." / "lib" / nifcoreparse        # re-exports nifcore + parseFromBuffer
import ".." / "lib" / nifcdecl              # stmtKind/exprKind/typeKind, tag enums
import ".." / "models" / tags               # *TagId ordinals for synthesis
import nifmodules                                   # MainModule, getDeclOrNil
import ".." / "lib" / intrinsics             # the shared `{.instruction.}` row table
export intrinsics   # `intrinsicOfCallee` returns an `IntrinsicOp`, so every
                    # importer needs the enum and the row table with it

proc firstChild(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc intrinsicOfCallee*(m: var MainModule; callee: Cursor;
                        bits: var int): IntrinsicOp =
  ## The opcode a `(instr SYM …)` names, plus the width its row bound (taken
  ## from the declared first operand). Reads the `(instruction X)` /
  ## `(intrinsic X)` pragma off the callee's declaration — a table lookup on an
  ## ident, not a match against a C name. Shared by both Leng back ends: the
  ## C one keys its `__builtin_*` choice off it and the LLVM one reuses that
  ## same choice, so neither re-derives the row from the declaration itself.
  result = NoIntrinsicOp
  bits = 0
  if callee.kind != Symbol: return
  let d = m.getDeclOrNil(callee.symId)
  if d == nil or d.kind != ProcY: return
  var n = d.pos
  n.into:
    inc n                                   # name
    if n.typeKind == ParamsT:               # first param's type → the width
      var p = n
      p.loopInto:
        if p.substructureKind == ParamU and bits == 0:
          let pd = takeParamDecl(p)
          if pd.typ.kind == TagLit and pd.typ.typeKind in {IT, UT, CT}:
            var b = pd.typ
            inc b
            if b.kind == IntLit: bits = int(b.intVal)
        else:
          skip p
    skip n                                  # params
    skip n                                  # return type
    if n.substructureKind == PragmasU:
      var p = n
      p.loopInto:
        let pk = p.pragmaKind
        if pk in {InstructionP, IntrinsicP}:
          var a = p; a = sub(a)
          if a.kind == StrLit:
            result = intrinsicOpByName(m.pool.strings[a.strId],
                       (if pk == InstructionP: icPinned else: icPortable))
        skip p
    while n.hasMore: skip n

proc cBuiltinFor*(op: IntrinsicOp; bits: int): string =
  ## The GCC/clang builtin a *portable* opcode lowers to. Target-pinned rows
  ## have no portable C spelling by construction and return "" — the C backend
  ## rejects them rather than guessing an equivalent.
  case op
  of CtzOp:      (if bits <= 32: "__builtin_ctz" else: "__builtin_ctzll")
  of ClzOp:      (if bits <= 32: "__builtin_clz" else: "__builtin_clzll")
  of PopcountOp: (if bits <= 32: "__builtin_popcount" else: "__builtin_popcountll")
  of BswapOp:
    if bits <= 16: "__builtin_bswap16"
    elif bits <= 32: "__builtin_bswap32"
    else: "__builtin_bswap64"
  # The atomics map back to the `__atomic_*` builtins their declarations used to
  # `importc` directly, so the generated C is unchanged by their becoming rows.
  # `bits` is not consulted: unlike the bit-counting builtins these are
  # type-generic in GCC — the width comes from the pointer argument — which is
  # also why `intrinsicOfCallee` leaves `bits` at 0 here (the first param is a
  # `ptr T`, not an integer).
  of AtomicLoadOp: "__atomic_load_n"
  of AtomicStoreOp: "__atomic_store_n"
  of AtomicExchangeOp: "__atomic_exchange_n"
  of AtomicCompareExchangeOp: "__atomic_compare_exchange_n"
  of AtomicFetchAddOp: "__atomic_fetch_add"
  of AtomicFetchSubOp: "__atomic_fetch_sub"
  of AtomicFetchAndOp: "__atomic_fetch_and"
  of AtomicFetchOrOp: "__atomic_fetch_or"
  of AtomicFetchXorOp: "__atomic_fetch_xor"
  of AtomicAddFetchOp: "__atomic_add_fetch"
  of AtomicSubFetchOp: "__atomic_sub_fetch"
  of AtomicTestAndSetOp: "__atomic_test_and_set"
  of AtomicClearOp: "__atomic_clear"
  of AtomicThreadFenceOp: "__atomic_thread_fence"
  of AtomicSignalFenceOp: "__atomic_signal_fence"
  else: ""

proc isImportC*(m: var MainModule; n: Cursor): bool =
  if n.kind in {Symbol, SymbolDef}:
    let d = m.getDeclOrNil(n.symId)
    result = d != nil and d.isImport
  else:
    result = false

proc registerParams*(c: var MainModule; params: Cursor) =
  ## Register a routine's parameters in the current scope (their types feed
  ## `getType`). `params` is the `(params (param :name pragmas type) …)` node, or
  ## `.` for a parameterless routine.
  if params.kind != TagLit: return
  var p = params
  p.loopInto:
    if p.substructureKind == ParamU:
      let d = takeParamDecl(p)
      if d.name.kind == SymbolDef:
        registerLocal(c, d.name.symId, d.typ)
    else:
      skip p

# ---- synthesized types ----------------------------------------------------

proc createIntegralType*(c: var MainModule; name: string): Cursor =
  result = c.builtinTypes.getOrDefault(name, default(Cursor))
  if cursorIsNil(result):
    var buf = parseFromBuffer(name, "<builtin>", 8, c.pool, c.tags)
    c.mem.add ensureMove(buf)
    result = cursorAt(c.mem[c.mem.len-1], 0)
    c.builtinTypes[name] = result

proc ptrTypeOf(c: var MainModule; elem: Cursor): Cursor =
  var buf = createTokenBuf(4, c.pool, c.tags)
  buf.openTag TagId(ord(PtrTagId))
  buf.addSubtree elem
  buf.closeTag()
  c.mem.add ensureMove(buf)
  result = cursorAt(c.mem[c.mem.len-1], 0)

# ---- field lookup ---------------------------------------------------------

type
  FieldSelector* = enum
    FieldType, FieldPragmas

proc typeOfField*(c: var MainModule; n: var Cursor; fld: SymId;
                  sel = FieldType): Cursor =
  if n.substructureKind == FldU:
    let decl = takeFieldDecl(n)
    if decl.name.kind == SymbolDef and decl.name.symId == fld:
      result = if sel == FieldType: decl.typ else: decl.pragmas
    else:
      result = default(Cursor)
  else:
    result = default(Cursor)
    let tk = n.typeKind
    if tk in {ObjectT, UnionT}:
      n.into:
        var hasBase = false
        var baseSym = default(SymId)
        if tk == ObjectT:
          if n.kind == Symbol:
            hasBase = true
            baseSym = n.symId
          skip n  # inheritance reference
        var done = false
        while n.hasMore and not done:
          result = typeOfField(c, n, fld, sel)
          if not cursorIsNil(result): done = true
        while n.hasMore: skip n  # mop up if we broke early
        if cursorIsNil(result) and hasBase:
          let d = c.getDeclOrNil(baseSym)
          if d != nil and d.pos.stmtKind == TypeS:
            var baseBody = asTypeDecl(d.pos).body
            result = typeOfField(c, baseBody, fld, sel)

proc navigateToObjectBody*(c: var MainModule; n: Cursor): Cursor =
  var counter = 20
  result = n
  while counter > 0 and result.kind == Symbol:
    dec counter
    let d = c.getDeclOrNil(result.symId)
    if d != nil and d.pos.stmtKind == TypeS:
      result = asTypeDecl(d.pos).body
    else:
      break

# ---- the navigator --------------------------------------------------------

proc getTypeImpl(c: var MainModule; n: Cursor): Cursor =
  case n.kind
  of DotToken, Ident, SymbolDef:
    result = createIntegralType(c, "(err)")
  of Symbol:
    var it {.cursor.} = c.current
    while it != nil:
      let res = it.locals.getOrDefault(n.symId, default(Cursor))
      if not cursorIsNil(res):
        return res
      it = it.parent
    let d = c.getDeclOrNil(n.symId)
    if d != nil:
      result = getTypeImpl(c, d.pos)
    else:
      # importC types are not defined
      result = createIntegralType(c, "(err)")
  of IntLit:
    result = createIntegralType(c, "(i -1)")
  of UIntLit:
    result = createIntegralType(c, "(u -1)")
  of FloatLit:
    result = createIntegralType(c, "(f +64)")
  of StrLit:
    result = createIntegralType(c, "(aptr (c +8))")
  of CharLit:
    result = createIntegralType(c, "(c +8)")
  of ExtendedSuffix, LineInfoLit, UnknownToken, EofToken, ParLe, ParRi:
    result = createIntegralType(c, "(err)")
  of TagLit:
    case n.exprKind
    of SizeofC, AlignofC, OffsetofC:
      result = createIntegralType(c, "(i +8)")
    of InfC, NegInfC, NanC:
      result = createIntegralType(c, "(f +64)")
    of TrueC, FalseC, AndC, OrC, NotC, EqC, NeqC, LeC, LtC, ErrvC, OvfC:
      result = createIntegralType(c, "(bool)")
    of CallC, InstrC:
      # `(instr SYM …)` is typed EXACTLY like `(call SYM …)` — the callee's
      # signature drives everything. Only the *cost* differs, which is what the
      # separate tag exists to make visible.
      var procType = navigateToObjectBody(c, getTypeImpl(c, firstChild(n)))
      if procType.typeKind == ProctypeT or procType.symKind == ProcY:
        inc procType
        skip procType  # name
      if procType.typeKind == ParamsT:
        result = procType
        skip result  # skip the parameters, return type follows
      else:
        result = createIntegralType(c, "(err)")
    of AtC, PatC:
      var arrayType = navigateToObjectBody(c, getTypeImpl(c, firstChild(n)))
      # Descend to the element type only when the base really is an indexable
      # type. Otherwise the base did not resolve (an unresolved `Symbol` or the
      # `(err)` sentinel), and a blind `inc` would run the cursor off the end of
      # its buffer — a later `kind`/`typeKind` on that exhausted cursor crashes.
      if arrayType.typeKind in {ArrayT, FlexarrayT, PtrT, AptrT}:
        result = arrayType
        inc result  # into the element type (first child of (arr …)/(ptr …))
      else:
        result = createIntegralType(c, "(err)")
    of DotC:
      var a = firstChild(n)
      var objType = navigateToObjectBody(c, getTypeImpl(c, a))
      skip a  # skip the object
      let fld = a.symId
      if objType.typeKind in {ObjectT, UnionT}:
        result = typeOfField(c, objType, fld)
        if cursorIsNil(result):
          result = createIntegralType(c, "(err)")
      else:
        result = createIntegralType(c, "(err)")
    of DerefC:
      let x = getTypeImpl(c, firstChild(n))
      if x.typeKind == PtrT:
        result = firstChild(x)
      else:
        result = createIntegralType(c, "(err)")
    of AddrC, HaddrC:                       # both are `&lvalue`
      let x = getTypeImpl(c, firstChild(n))
      result = ptrTypeOf(c, x)
    of ConvC, CastC, AconstrC, OconstrC, BaseobjC:
      result = firstChild(n)
    of NegC, AddC, SubC, MulC, DivC, ModC, ShrC, ShlC,
       BitandC, BitorC, BitxorC, BitnotC:
      result = firstChild(n)
    of ParC:
      result = getTypeImpl(c, firstChild(n))
    of NilC:
      result = createIntegralType(c, "(ptr (void))")
    of SufC:
      result = createIntegralType(c, "(err)")
      var a = firstChild(n)
      skip a
      if a.kind in {StrLit, Ident}:
        let s = strVal(a, c.pool)
        if s.len > 0:
          if s[0] == 'i':
            result = createIntegralType(c, "(i " & s.substr(1) & ")")
          elif s[0] == 'u':
            result = createIntegralType(c, "(u " & s.substr(1) & ")")
          elif s[0] == 'f':
            result = createIntegralType(c, "(f " & s.substr(1) & ")")
    of NoExpr:
      case n.stmtKind
      of ProcS:
        result = n
        inc result  # ProcS token
        skip result # skip the name
      of GvarS, TvarS, ConstS, VarS:
        result = n
        inc result  # token
        skip result # skip the name
        skip result # skip the pragmas
      else:
        if n.substructureKind in {ParamU, FldU}:
          result = n
          inc result  # token
          skip result # skip the name
          skip result # skip the pragmas
        else:
          result = createIntegralType(c, "(err)")

proc getType*(c: var MainModule; n: Cursor; skipAliases = true): Cursor =
  result = getTypeImpl(c, n)
  if skipAliases:
    result = navigateToObjectBody(c, result)

proc getNominalType*(c: var MainModule; n: Cursor): Cursor =
  ## Arrays are nominal types in NIFC too, so this does not skip aliases.
  result = getTypeImpl(c, n)

proc lookupField*(c: var MainModule; typ: Cursor; fld: SymId): Cursor =
  var body = navigateToObjectBody(c, typ)
  result = typeOfField(c, body, fld)
