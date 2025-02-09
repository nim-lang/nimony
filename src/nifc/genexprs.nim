#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from codegen.nim

proc genx(c: var GeneratedCode; n: var Cursor)

template typedBinOp(opr) =
  let (typ, a, b) = sons3(t, n)
  c.add ParLe
  c.add ParLe
  genType c, t, typ
  c.add ParRi
  c.add ParLe
  genx c, t, a
  c.add opr
  genx c, t, b
  c.add ParRi
  c.add ParRi

template cmpOp(opr) =
  let (a, b) = sons2(t, n)
  c.add ParLe
  genx c, t, a
  c.add opr
  genx c, t, b
  c.add ParRi

template unOp(opr) =
  c.add ParLe
  c.add opr
  genx c, n.firstSon
  c.add ParRi

template typedUnOp(opr) =
  let (typ, a) = sons2(t, n)
  c.add ParLe
  c.add ParLe
  genType c, t, typ
  c.add ParRi
  c.add opr
  genx c, t, a
  c.add ParRi

proc genCall(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, t, info(t, n))
  genx c, n.firstSon
  c.add ParLe
  var i = 0
  for ch in sonsFromX(t, n):
    if i > 0: c.add Comma
    genx c, t, ch
    inc i
  c.add ParRi

proc genCallCanRaise(c: var GeneratedCode; n: var Cursor) =
  genCLineDir(c, t, info(t, n))
  genx c, t, ithSon(t, n, 1)
  c.add ParLe
  var i = 0
  for ch in sonsFromX(t, n, 2):
    if i > 0: c.add Comma
    genx c, t, ch
    inc i
  c.add ParRi

proc genLvalue(c: var GeneratedCode; n: var Cursor) =
  case t[n].kind
  of Sym:
    let lit = t[n].litId
    let name = mangle(c.m.lits.strings[lit])
    c.add name
    c.requestedSyms.incl name
  of DerefC: unOp "*"
  of AtC:
    let (a, i) = sons2(t, n)
    genx c, t, a
    let tyArr = getType(c.m, t, a)
    if tyArr.isError:
      error c.m, "cannot get the type of ", t, a
    if not c.m.isImportC(tyArr):
      c.add Dot
      c.add "a"
    c.add BracketLe
    genx c, t, i
    c.add BracketRi
  of PatC:
    let (a, i) = sons2(t, n)
    genx c, t, a
    c.add BracketLe
    genx c, t, i
    c.add BracketRi
  of DotC:
    let (obj, fld, inheritance) = sons3(t, n)
    let inhs {.cursor.} = c.m.lits.strings[t[inheritance].litId]
    if inhs != "0":
      var inh = parseInt(inhs)
      genx c, t, obj
      while inh > 0:
        c.add ".Q"
        dec inh
    else:
      genx c, t, obj
    c.add Dot
    genx c, t, fld
  of ErrC:
    if {gfMainModule, gfHasError} * c.flags == {}:
      moveToDataSection:
        c.add ExternKeyword
        c.add ThreadVarToken
        c.add "NB8 "
        c.add ErrToken
        c.add Semicolon
      c.flags.incl gfHasError
    c.add ErrToken
  else:
    error c.m, "expected expression but got: ", n

proc objConstrType(c: var GeneratedCode; n: var Cursor) =
  # C99 is strange, it requires (T){} for struct construction but not for
  # consts.
  if c.inSimpleInit == 0:
    c.add ParLe
    genType(c, n)
    c.add ParRi

proc suffixToType(c: var GeneratedCode; t: Tree; suffix: NodePos) =
  case c.m.lits.strings[t[suffix].litId]
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

proc suffixConv(c: var GeneratedCode; t: Tree; value: NodePos, suffix: NodePos) =
  c.add ParLe
  c.add ParLe
  suffixToType c, t, suffix
  c.add ParRi
  genx c, t, value
  c.add ParRi

proc genx(c: var GeneratedCode; n: var Cursor) =
  case t[n].kind
  of IntLit:
    genIntLit c, t[n].litId
  of UIntLit:
    genUIntLit c, t[n].litId
  of FloatLit:
    c.add c.m.lits.strings[t[n].litId]
  of CharLit:
    let ch = t[n].uoperand
    var s = "'"
    toCChar char(ch), s
    s.add "'"
    c.add s
  of FalseC: c.add "NIM_FALSE"
  of TrueC: c.add "NIM_TRUE"
  of StrLit:
    c.add makeCString(c.m.lits.strings[t[n].litId])
  of NilC:
    c.add NullPtr
  of InfC:
    c.add "INF"
  of NegInfC:
    c.add "-INF"
  of NanC:
    c.add "NAN"
  of AconstrC:
    c.objConstrType(t, n.firstSon)
    c.add CurlyLe
    c.add ".a = "
    c.add CurlyLe
    var i = 0
    for ch in sonsFromX(t, n):
      if i > 0: c.add Comma
      c.genx t, ch
      inc i
    c.add CurlyRi
    c.add CurlyRi
  of OconstrC:
    c.objConstrType(t, n.firstSon)
    c.add CurlyLe
    var i = 0
    for ch in sonsFromX(t, n):
      if i > 0: c.add Comma
      if t[ch].kind == OconstrC:
        # inheritance
        c.add Dot
        c.add "Q"
        c.add AsgnOpr
        c.genx t, ch
      else:
        let (k, v) = sons2(t, ch)
        c.add Dot
        c.genx t, k
        c.add AsgnOpr
        c.genx t, v
      inc i
    c.add CurlyRi
  of ParC:
    let arg = n.firstSon
    c.add ParLe
    genx c, t, arg
    c.add ParRi
  of AddrC: unOp "&"
  of SizeofC:
    let arg = n.firstSon
    c.add "sizeof"
    c.add ParLe
    genType c, t, arg
    c.add ParRi
  of AlignofC:
    let arg = n.firstSon
    c.add "NIM_ALIGNOF"
    c.add ParLe
    genType c, t, arg
    c.add ParRi
  of OffsetofC:
    let (typ, mem) = sons2(t, n)
    c.add "offsetof"
    c.add ParLe
    genType c, t, typ
    c.add Comma
    let lit = t[mem].litId
    let name = mangle(c.m.lits.strings[lit])
    c.add name
    c.add ParRi
  of CallC: genCall c, n
  of AddC: typedBinOp " + "
  of SubC: typedBinOp " - "
  of MulC: typedBinOp " * "
  of DivC: typedBinOp " / "
  of ModC: typedBinOp " % "
  of ShlC: typedBinOp " << "
  of ShrC: typedBinOp " >> "
  of BitandC: typedBinOp " & "
  of BitorC: typedBinOp " | "
  of BitxorC: typedBinOp " ^ "
  of BitnotC: typedUnOp " ~ "
  of AndC: cmpOp " && "
  of OrC: cmpOp " || "
  of NotC: unOp "!"
  of NegC: typedUnOp "-"
  of EqC: cmpOp " == "
  of NeqC: cmpOp " != "
  of LeC: cmpOp " <= "
  of LtC: cmpOp " < "
  of CastC: typedUnOp ""
  of ConvC: typedUnOp ""
  of SufC:
    let (value, suffix) = sons2(t, n)
    if t[value].kind == StrLit:
      genx c, t, value
    else:
      suffixConv(c, t, value, suffix)
  of OnErrC:
    genCallCanRaise c, n
  else:
    genLvalue c, n
