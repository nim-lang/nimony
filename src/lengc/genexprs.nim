#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from codegen.nim

proc genx(c: var GeneratedCode; n: var Cursor)

proc typedBinOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  n.into:
    c.add ParLe
    c.add ParLe
    genType c, n
    c.add ParRi
    c.add ParLe
    genx c, n
    c.add opr
    genx c, n
    c.add ParRi
    c.add ParRi
    while n.hasMore: skip n

proc cmpOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  n.into:
    c.add ParLe
    genx c, n
    c.add opr
    genx c, n
    c.add ParRi
    while n.hasMore: skip n

proc unOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  n.into:
    c.add ParLe
    c.add opr
    genx c, n
    c.add ParRi
    while n.hasMore: skip n

proc typedUnOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  n.into:
    c.add ParLe
    c.add ParLe
    genType c, n
    c.add ParRi
    c.add opr
    genx c, n
    c.add ParRi
    while n.hasMore: skip n

proc genCall(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, info(n))
  n.into:
    let isCfn = isImportC(c.m, n)
    genx c, n
    c.add ParLe
    var i = 0
    while n.hasMore:
      if i > 0: c.add Comma
      if isCfn:
        c.flags.incl gfInCallImportC
      genx c, n
      inc i
    c.add ParRi

proc intrinsicOfCallee(c: var GeneratedCode; callee: Cursor;
                       bits: var int): IntrinsicOp =
  ## The opcode a `(instr SYM …)` names, plus the width its row bound (taken
  ## from the declared first operand). Reads the `(instruction X)` /
  ## `(intrinsic X)` pragma off the callee's declaration — a table lookup on an
  ## ident, not a match against a C name.
  result = NoIntrinsicOp
  bits = 0
  if callee.kind != Symbol: return
  let d = c.m.getDeclOrNil(callee.symId)
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
          if a.kind == Ident:
            result = intrinsicOpByName(c.m.pool.strings[a.strId],
                       (if pk == InstructionP: icPinned else: icPortable))
        skip p
    while n.hasMore: skip n

proc cBuiltinFor(op: IntrinsicOp; bits: int): string =
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
  else: ""

proc genInstr(c: var GeneratedCode; n: var Cursor) =
  ## `(instr SYM X*)` — an intrinsic application. Selection-final: the opcode
  ## the source named is the one emitted, so this never falls back to a call.
  genCLineDir(c, info(n))
  let start = n
  n.into:
    var bits = 0
    let op = intrinsicOfCallee(c, n, bits)
    var builtin = ""
    if op == NoIntrinsicOp:
      error c.m, "callee of (instr ...) carries no instruction/intrinsic pragma: ", start
    else:
      builtin = cBuiltinFor(op, bits)
      if builtin.len == 0:
        error c.m, "the C backend has no lowering for the target-pinned instruction `" &
          IntrinsicNames[op] & "`; use the portable `{.intrinsic: ....}` form " &
          "or guard the call with a `when`: ", start
    c.add builtin
    skip n                                  # the callee symbol
    c.add ParLe
    var i = 0
    while n.hasMore:
      if i > 0: c.add Comma
      genx c, n
      inc i
    c.add ParRi

proc genCallCanRaise(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, info(n))
  n.into:
    skip n # skip error action
    let isCfn = isImportC(c.m, n)
    genx c, n
    c.add ParLe
    var i = 0
    while n.hasMore:
      if i > 0: c.add Comma
      if isCfn:
        c.flags.incl gfInCallImportC
      genx c, n
      inc i
    c.add ParRi

proc genDeref(c: var GeneratedCode; n: var Cursor) =
  n.into:
    c.add ParLe
    let starAt = c.code.len
    c.add "*"
    genx c, n
    c.add ParRi
    if n.hasMore and n.typeQual == CppRefQ:
      if c.m.config.backend == backendCpp:
        c.code[starAt] = Token EmptyToken
      skip n
    while n.hasMore: skip n

proc genField(c: var GeneratedCode; fld: Cursor; objBody: Cursor; objTypeIsImported: bool) =
  if fld.kind == Symbol:
    let s = fld.symId
    var t = objBody
    let pragmas = typeOfField(c.m, t, s, FieldPragmas)
    if not cursorIsNil(pragmas) and pragmas.kind == TagLit:
      var p = pragmas
      p.into:
        while p.hasMore:
          case p.pragmaKind
          of ImportcP, ImportcppP, ExportcP:
            let litId = externName(s, p)
            c.add c.m.pool.strings[litId]
            return
          else:
            discard
          skip p
    var x = c.m.pool.syms[s]
    if objTypeIsImported:
      extractBasename x
      c.add x
    else:
      c.add mangleToC(x)
  else:
    error c.m, "expected field name but got: ", fld

proc isImportedArray(c: var GeneratedCode; n: Cursor): bool =
  if n.exprKind == DotC:
    let nn = n.firstSon
    var objType = getNominalType(c.m, nn)
    result = c.m.isImportC(objType)
  else:
    result = false

proc genLvalue(c: var GeneratedCode; n: var Cursor) =
  case n.exprKind
  of NoExpr:
    if n.kind == Symbol:
      c.requestedSyms.incl n.symId
      let name = mangleSym(c, n.symId)
      c.add name
      inc n
    else:
      error c.m, "expected expression but got: ", n
  of DerefC: genDeref c, n
  of AtC:
    n.into:
      let needsAwrapper = not isImportedArray(c, n)
      genx c, n
      if needsAwrapper:
        c.add Dot
        c.add "a"
      c.add BracketLe
      genx c, n
      c.add BracketRi
      while n.hasMore: skip n
  of PatC:
    n.into:
      genx c, n
      c.add BracketLe
      genx c, n
      c.add BracketRi
      while n.hasMore: skip n
  of DotC:
    n.into:
      let objType = getNominalType(c.m, n)
      let objBody = navigateToObjectBody(c.m, objType)
      genx c, n
      var fld = n
      skip n
      if n.hasMore and n.kind == IntLit:
        var inh = intVal(n)
        inc n
        while inh > 0:
          c.add ".Q"
          dec inh
      c.add Dot
      genField c, fld, objBody, c.m.isImportC(objType)
      while n.hasMore: skip n
  of ErrvC:
    if {gfMainModule, gfHasError} * c.flags == {}:
      moveToDataSection:
        c.add ExternKeyword
        c.add ThreadVarToken
        c.add "NB8 "
        c.add ErrToken
        c.add Semicolon
      c.flags.incl gfHasError
    c.add ErrToken
    skip n
  of OvfC:
    c.add OvfToken
    c.currentProc.needsOverflowFlag = true
    skip n
  else:
    error c.m, "expected expression but got: ", n

proc objConstrType(c: var GeneratedCode; n: var Cursor) =
  # C99 is strange, it requires (T){} for struct construction but not for
  # consts.
  if c.objConstrNeedsType:
    c.add ParLe
    genType c, n
    c.add ParRi
  else:
    skip n

proc suffixToType(c: var GeneratedCode; suffix: Cursor) =
  case c.m.pool.strings[suffix.litId]
  of "i64":
    c.add "NI64"
  of "i32":
    c.add "NI32"
  of "i16":
    c.add "NI16"
  of "i8":
    c.add "NI8"
  of "u64":
    c.add "NU64"
  of "u32":
    c.add "NU32"
  of "u16":
    c.add "NU16"
  of "u8":
    c.add "NU8"
  of "f64":
    c.add "NF64"
  of "f32":
    c.add "NF32"
  else:
    # TODO: f128?
    quit "unsupported suffix"

proc suffixConv(c: var GeneratedCode; value, suffix: Cursor) =
  c.add ParLe
  c.add ParLe
  suffixToType c, suffix
  c.add ParRi
  var value = value
  genx c, value
  c.add ParRi

proc genAddr(c: var GeneratedCode; n: var Cursor) =
  # If we take the address of an array expression, add the `.a` field access.
  let inCallImportC = gfInCallImportC in c.flags
  n.into:
    let needsAwrapper = not isImportedArray(c, n)
    let arrType = getType(c.m, n)
    c.add ParLe
    let ampAt = c.code.len
    c.add "&"
    genx c, n
    if arrType.typeKind == ArrayT and needsAwrapper and inCallImportC:
      c.add ".a[0]"
    c.add ParRi
    if n.hasMore and n.typeQual == CppRefQ:
      if c.m.config.backend == backendCpp:
        c.code[ampAt] = Token EmptyToken
      skip n
    while n.hasMore: skip n

proc genCond(c: var GeneratedCode; n: var Cursor) =
  # Special cased so that we do not end up with `if ((a == b))` which
  # produced warnings.
  case n.exprKind
  of EqC: cmpOp c, n, " == "
  of NeqC: cmpOp c, n, " != "
  of LeC: cmpOp c, n, " <= "
  of LtC: cmpOp c, n, " < "
  else:
    c.add ParLe
    genx c, n
    c.add ParRi

proc genx(c: var GeneratedCode; n: var Cursor) =
  if n.exprKind != AddrC and n.kind != StrLit:
    c.flags.excl gfInCallImportC
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:
      genIntLit c, intVal(n)
      inc n
    of UIntLit:
      genUIntLit c, uintVal(n)
      inc n
    of FloatLit:
      c.add $floatVal(n)
      inc n
    of CharLit:
      let ch = n.charLit
      c.add "(NC8)"
      var s = "'"
      toCChar ch, s
      s.add "'"
      c.add s
      inc n
    of StrLit:
      if gfInCallImportC notin c.flags and gfInFlexArray notin c.flags:
        c.add "(NC8*)"
      c.add makeCString(c.m.pool.strings[n.litId])
      inc n
    else:
      genLvalue c, n
  of FalseC:
    c.add "NIM_FALSE"
    skip n
  of TrueC:
    c.add "NIM_TRUE"
    skip n
  of NilC:
    c.add NullPtr
    skip n
  of InfC:
    c.add "INF"
    skip n
  of NegInfC:
    c.add "-INF"
    skip n
  of NanC:
    c.add "NAN"
    skip n
  of AconstrC:
    n.into:
      let isUncheckedArray = n.typeKind in {PtrT, AptrT, FlexarrayT}
      c.objConstrType(n)
      c.add CurlyLe
      if not isUncheckedArray:
        c.add ".a = "
        c.add CurlyLe
      var i = 0
      while n.hasMore:
        if i > 0: c.add Comma
        c.genx n
        inc i
      if not isUncheckedArray:
        c.add CurlyRi
      c.add CurlyRi
  of OconstrC:
    n.into:
      let objType = n
      let objBody = navigateToObjectBody(c.m, n)
      c.objConstrType(n)
      c.add CurlyLe
      var i = 0
      while n.hasMore:
        if i > 0: c.add Comma
        if n.substructureKind == KvU:
          n.into:
            c.add Dot
            var depth = n
            skip depth
            skip depth
            if depth.hasMore and depth.hasMore:
              # inheritance depth
              assert depth.kind == IntLit
              let d = intVal(depth)
              for _ in 0 ..< d:
                c.add "Q"
                c.add Dot
            let fldSym = if n.kind == Symbol: n.symId else: SymId(0)
            c.genField n, objBody, c.m.isImportC(objType)
            inc n
            c.add AsgnOpr
            # For flexible array member fields, suppress the (NC8*) cast on string literals
            var fldBody = objBody
            let fldType = if fldSym != SymId(0): typeOfField(c.m, fldBody, fldSym) else: default(Cursor)
            let isFlexArr = not cursorIsNil(fldType) and fldType.typeKind == FlexarrayT
            if isFlexArr: c.flags.incl gfInFlexArray
            c.genx n
            if isFlexArr: c.flags.excl gfInFlexArray
            while n.hasMore: skip n
        elif n.exprKind == OconstrC:
          # inheritance
          c.add Dot
          c.add "Q"
          c.add AsgnOpr
          c.genx n
        else:
          c.genx n
        inc i
      c.add CurlyRi
  of BaseobjC:
    n.into:
      skip n # type not interesting for us
      var counter = intVal(n)
      skip n
      c.genx n
      while counter > 0:
        c.add ".Q"
        dec counter
      while n.hasMore: skip n
  of ParC:
    c.add ParLe
    n.into:
      genx c, n
      c.add ParRi
      while n.hasMore: skip n
  of AddrC:
    genAddr c, n
    c.flags.excl gfInCallImportC
  of SizeofC:
    c.add "sizeof"
    c.add ParLe
    n.into:
      genType c, n
      c.add ParRi
      while n.hasMore: skip n
  of AlignofC:
    c.add "NIM_ALIGNOF"
    c.add ParLe
    n.into:
      genType c, n
      c.add ParRi
      while n.hasMore: skip n
  of OffsetofC:
    n.into:
      c.add "offsetof"
      c.add ParLe
      genType c, n
      c.add Comma
      let name = mangleSym(c, n.symId)
      inc n
      c.add name
      c.add ParRi
      while n.hasMore: skip n
  of CallC: genCall c, n
  of InstrC: genInstr c, n
  of AddC: typedBinOp c, n, " + "
  of SubC: typedBinOp c, n, " - "
  of MulC: typedBinOp c, n, " * "
  of DivC: typedBinOp c, n, " / "
  of ModC: typedBinOp c, n, " % "
  of ShlC: typedBinOp c, n, " << "
  of ShrC: typedBinOp c, n, " >> "
  of BitandC: typedBinOp c, n, " & "
  of BitorC: typedBinOp c, n, " | "
  of BitxorC: typedBinOp c, n, " ^ "
  of BitnotC: typedUnOp c, n, " ~ "
  of AndC: cmpOp c, n, " && "
  of OrC: cmpOp c, n, " || "
  of NotC: unOp c, n, "!"
  of NegC: typedUnOp c, n, "-"
  of EqC: cmpOp c, n, " == "
  of NeqC: cmpOp c, n, " != "
  of LeC: cmpOp c, n, " <= "
  of LtC: cmpOp c, n, " < "
  of CastC: typedUnOp c, n, ""
  of ConvC: typedUnOp c, n, ""
  of SufC:
    var value: Cursor
    var suffix: Cursor
    n.into:
      value = n
      skip n
      suffix = n
      skip n
      while n.hasMore: skip n
    if value.kind == StrLit:
      genx c, value
    else:
      suffixConv c, value, suffix
  of ErrvC, AtC, DerefC, DotC, PatC, OvfC:
    genLvalue c, n
