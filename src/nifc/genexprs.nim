#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from codegen.nim

proc genx(c: var GeneratedCode; n: var Cursor)

proc typedBinOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  inc n
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
  skipParRi n

proc cmpOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  inc n
  c.add ParLe
  genx c, n
  c.add opr
  genx c, n
  c.add ParRi
  skipParRi n

proc unOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  inc n
  c.add ParLe
  c.add opr
  genx c, n
  c.add ParRi
  skipParRi n

proc typedUnOp(c: var GeneratedCode; n: var Cursor; opr: string) =
  inc n
  c.add ParLe
  c.add ParLe
  genType c, n
  c.add ParRi
  c.add opr
  genx c, n
  c.add ParRi
  skipParRi n

proc genCall(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, info(n))
  inc n
  let isCfn = isImportC(n)
  genx c, n
  c.add ParLe
  var i = 0
  while n.kind != ParRi:
    if i > 0: c.add Comma
    if isCfn:
      c.flags.incl gfInCallImportC
    genx c, n
    inc i
  c.add ParRi
  skipParRi n

proc genCallCanRaise(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, info(n))
  inc n
  skip n # skip error action
  let isCfn = isImportC(n)
  genx c, n
  c.add ParLe
  var i = 0
  while n.kind != ParRi:
    if i > 0: c.add Comma
    if isCfn:
      c.flags.incl gfInCallImportC
    genx c, n
    inc i
  c.add ParRi
  skipParRi n

proc genDeref(c: var GeneratedCode; n: var Cursor) =
  inc n
  c.add ParLe
  let starAt = c.code.len
  c.add "*"
  genx c, n
  c.add ParRi
  if n.kind != ParRi and n.typeQual == CppRefQ:
    if c.m.config.backend == backendCpp:
      c.code[starAt] = Token EmptyToken
    skip n
  skipParRi n

proc genLvalue(c: var GeneratedCode; n: var Cursor) =
  case n.exprKind
  of NoExpr:
    if n.kind == Symbol:
      let name = mangle(pool.syms[n.symId])
      c.add name
      c.requestedSyms.incl name
      inc n
    else:
      error c.m, "expected expression but got: ", n
  of DerefC: genDeref c, n
  of AtC:
    inc n
    let arrType = getType(c.m, n)
    genx c, n
    if not (c.m.isImportC(arrType) or arrType.typeKind == NoType):
      c.add Dot
      c.add "a"
    c.add BracketLe
    genx c, n
    c.add BracketRi
    skipParRi n
  of PatC:
    inc n
    genx c, n
    c.add BracketLe
    genx c, n
    c.add BracketRi
    skipParRi n
  of DotC:
    inc n
    genx c, n
    var fld = n
    skip n
    if n.kind == IntLit:
      var inh = pool.integers[n.intId]
      inc n
      while inh > 0:
        c.add ".Q"
        dec inh
    c.add Dot
    genx c, fld
    skipParRi n
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
  case pool.strings[suffix.litId]
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
  inc n
  let arrType = getType(c.m, n)
  c.add ParLe
  let ampAt = c.code.len
  c.add "&"
  genx c, n
  if arrType.typeKind == ArrayT and not (c.m.isImportC(arrType) or arrType.typeKind == NoType) and
        inCallImportC:
    c.add ".a[0]"
  c.add ParRi
  if n.kind != ParRi and n.typeQual == CppRefQ:
    if c.m.config.backend == backendCpp:
      c.code[ampAt] = Token EmptyToken
    skip n
  skipParRi n

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
  if n.exprKind != AddrC and n.kind != StringLit:
    c.flags.excl gfInCallImportC
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:
      genIntLit c, n.intId
      inc n
    of UIntLit:
      genUIntLit c, n.uintId
      inc n
    of FloatLit:
      c.add $pool.floats[n.floatId]
      inc n
    of CharLit:
      let ch = n.charLit
      c.add "(NC8)"
      var s = "'"
      toCChar ch, s
      s.add "'"
      c.add s
      inc n
    of StringLit:
      if gfInCallImportC notin c.flags:
        c.add "(NC8*)"
      c.add makeCString(pool.strings[n.litId])
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
    inc n
    let isUncheckedArray = n.typeKind in {PtrT, AptrT, FlexarrayT}
    c.objConstrType(n)
    c.add CurlyLe
    if not isUncheckedArray:
      c.add ".a = "
      c.add CurlyLe
    var i = 0
    while n.kind != ParRi:
      if i > 0: c.add Comma
      c.genx n
      inc i
    if not isUncheckedArray:
      c.add CurlyRi
    c.add CurlyRi
    skipParRi n
  of OconstrC:
    inc n
    c.objConstrType(n)
    c.add CurlyLe
    var i = 0
    while n.kind != ParRi:
      if i > 0: c.add Comma
      if n.substructureKind == KvU:
        inc n
        c.add Dot
        var depth = n
        skip depth
        skip depth
        if depth.kind != ParRi:
          # inheritance depth
          assert depth.kind == IntLit
          let d = pool.integers[depth.intId]
          for _ in 0 ..< d:
            c.add "Q"
            c.add Dot
        c.genx n
        c.add AsgnOpr
        c.genx n
        if n.kind != ParRi: skip n
        skipParRi n
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
    skipParRi n
  of BaseobjC:
    inc n
    skip n # type not interesting for us
    var counter = pool.integers[n.intId]
    skip n
    c.genx n
    while counter > 0:
      c.add ".Q"
      dec counter
    skipParRi n
  of ParC:
    c.add ParLe
    inc n
    genx c, n
    c.add ParRi
    skipParRi n
  of AddrC:
    genAddr c, n
    c.flags.excl gfInCallImportC
  of SizeofC:
    c.add "sizeof"
    c.add ParLe
    inc n
    genType c, n
    c.add ParRi
    skipParRi n
  of AlignofC:
    c.add "NIM_ALIGNOF"
    c.add ParLe
    inc n
    genType c, n
    c.add ParRi
    skipParRi n
  of OffsetofC:
    inc n
    c.add "offsetof"
    c.add ParLe
    genType c, n
    c.add Comma
    let name = mangle(pool.syms[n.symId])
    inc n
    c.add name
    c.add ParRi
    skipParRi n
  of CallC: genCall c, n
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
    inc n
    var value = n
    skip n
    let suffix = n
    skip n
    skipParRi n
    if value.kind == StringLit:
      genx c, value
    else:
      suffixConv c, value, suffix
  of ErrvC, AtC, DerefC, DotC, PatC, OvfC:
    genLvalue c, n
