#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from codegen.nim

## Generates C types from NIFC types.

type
  TypeList = object
    processed: IntSet
    s: seq[(Cursor, PredefinedToken)]

proc add(dest: var TypeList; elem: Cursor; decl: PredefinedToken) =
  if not containsOrIncl(dest.processed, elem.toUniqueId()):
    dest.s.add (elem, decl)

type
  TypeOrder = object
    forwardedDecls, ordered: TypeList
    lookedAt: IntSet
    lookedAtBodies: HashSet[SymId]

proc traverseObjectBody(m: Module; o: var TypeOrder; t: Cursor)

proc recordDependencyImpl(m: Module; o: var TypeOrder; parent, child: Cursor;
                          viaPointer: var bool) =
  var ch = child
  while true:
    case ch.typeKind
    of APtrT, PtrT:
      viaPointer = true
      ch = elementType(ch)
    of FlexarrayT:
      viaPointer = false
      ch = elementType(ch)
    else:
      break

  case ch.typeKind
  of ObjectT, UnionT:
    let decl = if ch.typeKind == ObjectT: TypedefStruct else: TypedefUnion
    let obj = ch
    if viaPointer:
      o.forwardedDecls.add parent, decl
    else:
      if not containsOrIncl(o.lookedAt, obj.toUniqueId()):
        traverseObjectBody(m, o, obj)
      o.ordered.add tracebackTypeC(ch), decl
  of ArrayT:
    if viaPointer:
      o.forwardedDecls.add parent, TypedefStruct
    else:
      if not containsOrIncl(o.lookedAt, ch.toUniqueId()):
         var viaPointer = false
         recordDependencyImpl m, o, ch, ch.firstSon, viaPointer
      o.ordered.add tracebackTypeC(ch), TypedefStruct
  of EnumT:
    # enums do not depend on anything so always safe to generate them
    o.ordered.add tracebackTypeC(ch), TypedefKeyword
  else:
    if ch.kind == Symbol:
      # follow the symbol to its definition:
      let id = ch.symId
      let def = m.defs.getOrDefault(id)
      if def.pos == 0:
        if pool.syms[id].endsWith(".c"):
          # imported from c, no need to check dependency
          discard
        else:
          error m, "undeclared symbol: ", ch
      else:
        var n = readonlyCursorAt(m.src, def.pos)
        let decl = asTypeDecl(n)
        if not containsOrIncl(o.lookedAtBodies, decl.name.symId):
          recordDependencyImpl m, o, n, decl.body, viaPointer
    else:
      discard "uninteresting type as we only focus on the required struct declarations"

proc recordDependency(m: Module; o: var TypeOrder; parent, child: Cursor) =
  var viaPointer = false
  recordDependencyImpl m, o, parent, child, viaPointer

proc traverseObjectBody(m: Module; o: var TypeOrder; t: Cursor) =
  var n = t
  inc n
  if n.kind == Symbol:
    # inheritance
    recordDependency m, o, t, n
    inc n
  elif n.kind == DotToken:
    inc n
  else:
    error m, "expected `Symbol` or `.` for inheritance but got: ", n
  while n.substructureKind == FldU:
    let decl = takeFieldDecl(n)
    recordDependency m, o, t, decl.typ

proc traverseProctypeBody(m: Module; o: var TypeOrder; t: Cursor) =
  var n = t
  let procType = takeProcType(n)
  var param = procType.params
  if param.kind == ParLe:
    param = param.firstSon
    while param.kind != ParRi:
      let paramDecl = takeParamDecl(param)
      recordDependency m, o, t, paramDecl.typ
  recordDependency m, o, t, procType.returnType

proc traverseTypes(m: Module; o: var TypeOrder) =
  for ch in m.types:
    let n = readonlyCursorAt(m.src, ch)
    let decl = asTypeDecl(n)
    let t = decl.body
    case t.typeKind
    of ObjectT:
      traverseObjectBody m, o, t
      o.ordered.add n, TypedefStruct
    of UnionT:
      traverseObjectBody m, o, t
      o.ordered.add n, TypedefUnion
    of ArrayT:
      recordDependency m, o, t, t.firstSon
      o.ordered.add n, TypedefStruct
    of ProctypeT:
      traverseProctypeBody m, o, t
      o.ordered.add n, TypedefKeyword
    of EnumT:
      o.ordered.add n, TypedefKeyword
    else: discard

template integralBits(t: Cursor): string =
  let res = pool.integers[t.intId]
  case res
  of -1:
    ""
  else: # 8, 16, 32, 64 etc.
    $res

proc genProcTypePragma(c: var GeneratedCode; n: Cursor; isVarargs: var bool) =
  # ProcTypePragma ::= CallingConvention | (varargs) | Attribute
  case n.pragmaKind
  of VarargsP:
    isVarargs = true
  of AttrP:
    c.add " __attribute__((" & toString(n.firstSon, false) & "))"
  else:
    if n.callConvKind != NoCallConv:
      discard "already handled"
    else:
      error c.m, "invalid proc type pragma: ", n

proc genProcTypePragmas(c: var GeneratedCode; n: var Cursor; isVarargs: var bool) =
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      genProcTypePragma(c, n, isVarargs)
      skip n
    inc n
  else:
    error c.m, "expected proc type pragmas but got: ", n

proc genWasPragma(c: var GeneratedCode; n: var Cursor) =
  inc n
  c.add "/* " & toString(n, false) & " */"
  skip n
  skipParRi n

proc genFieldPragmas(c: var GeneratedCode; n: var Cursor; bits: var BiggestInt) =
  # CommonPragma ::= (align Number) | (was Identifier) | Attribute
  # FieldPragma ::= CommonPragma | (bits Number)
  if n.kind == DotToken:
    inc n
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      case n.pragmaKind
      of AlignP:
        inc n
        c.add " NIM_ALIGN(" & $pool.integers[n.intId] & ")"
        skip n
        skipParRi n
      of WasP:
        genWasPragma c, n
      of AttrP:
        inc n
        c.add " __attribute__((" & toString(n, false) & "))"
        skip n
        skipParRi n
      of BitsP:
        inc n
        bits = pool.integers[n.intId]
        skip n
        skipParRi n
      else:
        error c.m, "invalid proc type pragma: ", n
    inc n
  else:
    error c.m, "expected field pragmas but got: ", n

proc getNumberQualifier(c: var GeneratedCode; n: Cursor): string =
  case n.typeQual
  of RoQ:
    result = "const "
  of AtomicQ:
    if c.m.config.backend == backendC:
      result = "_Atomic "
    else:
      # TODO: cpp doesn't support _Atomic
      result = ""
  of RestrictQ, NoQualifier, CppRefQ:
    error c.m, "expected number qualifier but got: ", n

proc getPtrQualifier(c: var GeneratedCode; n: Cursor; isCppRef: var bool): string =
  case n.typeQual
  of RoQ:
    result = "const "
  of AtomicQ:
    if c.m.config.backend == backendC:
      result = "_Atomic "
    else:
      # TODO: cpp doesn't support _Atomic
      result = ""
  of RestrictQ:
    result = "restrict "
  of CppRefQ:
    if c.m.config.backend == backendCpp:
      isCppRef = true
    result = ""
  of NoQualifier:
    error c.m, "expected pointer qualifier but got: ", n

proc genType(c: var GeneratedCode; n: var Cursor; name = "")

template maybeAddName(c: var GeneratedCode; name: string) =
  if name != "":
    c.add Space
    c.add name

template atom(c: var GeneratedCode; s, name: string) =
  c.add s
  maybeAddName(c, name)

proc atomNumber(c: var GeneratedCode; n: var Cursor; typeName, name: string; isBool = false) =
  if isBool:
    inc n
    while n.kind != ParRi:
      c.add getNumberQualifier(c, n)
      skip n
    atom(c, typeName, name)
    inc n
  else:
    var s = ""
    inc n
    assert n.kind == IntLit
    s = typeName & integralBits(n)
    inc n
    while n.kind != ParRi:
      c.add getNumberQualifier(c, n)
      skip n
    skipParRi n
    atom(c, s, name)

proc atomPointer(c: var GeneratedCode; n: var Cursor; name: string) =
  inc n
  var elem = n
  skip n # element type
  var isCppRef = false
  while n.kind != ParRi:
    c.add getPtrQualifier(c, n, isCppRef)
    skip n
  inc n # ParRi
  genType c, elem
  if isCppRef:
    c.add "&"
  else:
    c.add Star
  maybeAddName(c, name)

proc genProcType(c: var GeneratedCode; n: var Cursor; name = "") =
  let decl = takeProcType(n)
  var lastCallConv = NoCallConv
  if decl.pragmas.kind == ParLe:
    var p = decl.pragmas.firstSon
    while p.kind != ParRi:
      let cc = p.callConvKind
      if cc != NoCallConv:
        lastCallConv = cc
      skip p
  var isVarargs = false
  if lastCallConv != NoCallConv:
    c.add callingConvToStr(lastCallConv)
    c.add "_PTR"
    c.add ParLe
    if decl.returnType.kind == DotToken:
      c.add "void"
    else:
      var ret = decl.returnType
      genType c, ret
    c.add Comma
    var pragmas = decl.pragmas
    genProcTypePragmas c, pragmas, isVarargs
    maybeAddName(c, name)
    c.add ParRi
  else:
    if decl.returnType.kind == DotToken:
      c.add "void"
    else:
      var ret = decl.returnType
      genType c, ret
    c.add Space
    c.add ParLe
    var pragmas = decl.pragmas
    genProcTypePragmas c, pragmas, isVarargs
    c.add Star # "(*fn)"
    maybeAddName(c, name)
    c.add ParRi
  c.add ParLe
  var i = 0
  if decl.params.kind == ParLe:
    var p = decl.params.firstSon
    while p.kind != ParRi:
      var param = takeParamDecl(p)
      if i > 0: c.add Comma
      genType c, param.typ
      inc i

  if isVarargs:
    if i > 0: c.add Comma
    c.add "..."
  if i == 0:
    c.add "void"
  c.add ParRi

proc genType(c: var GeneratedCode; n: var Cursor; name = "") =
  case n.typeKind
  of VoidT:
    atom(c, "void", name)
    skip n
  of IT:
    atomNumber(c, n, "NI", name)
  of UT:
    atomNumber(c, n, "NU", name)
  of FT:
    atomNumber(c, n, "NF", name)
  of BoolT:
    atomNumber(c, n, "NB8", name, isBool = true)
  of CT:
    atomNumber(c, n, "NC", name)
  of NoType:
    if n.kind == Symbol:
      atom(c, mangle(pool.syms[n.symId]), name)
      inc n
    else:
      error c.m, "node is not a type: ", n
  of PtrT, APtrT:
    atomPointer(c, n, name)
  of FlexarrayT:
    inc n
    genType c, n
    maybeAddName(c, name)
    c.add BracketLe
    c.add BracketRi
    skipParRi n
  of ProctypeT:
    genProcType(c, n, name)
  of ParamsT, UnionT, ObjectT, EnumT, ArrayT:
    error c.m, "nominal type not allowed here: ", n

proc mangleField(c: var GeneratedCode; n: Cursor): string =
  if n.kind in {Symbol, SymbolDef}:
    result = mangle(pool.syms[n.symId])
  else:
    result = "InvalidFieldName"
    error c.m, "field name must be a SymDef, but got: ", n

proc genObjectOrUnionBody(c: var GeneratedCode; n: var Cursor) =
  inc n
  if n.kind == DotToken:
    inc n
  elif n.kind == Symbol:
    genType c, n, "Q"
    c.add Semicolon
  else:
    error c.m, "expected `Symbol` or `.` for inheritance but got: ", n

  while n.kind != ParRi:
    if n.substructureKind == FldU:
      var decl = takeFieldDecl(n)
      let f = mangleField(c, decl.name)
      var bits = 0'i64
      genFieldPragmas c, decl.pragmas, bits
      genType c, decl.typ, f
      if bits > 0:
        c.add " : "
        c.add $bits
      c.add Semicolon
    else:
      error c.m, "expected `fld` but got: ", n
  inc n # ParRi

proc genEnumDecl(c: var GeneratedCode; n: var Cursor; name: string) =
  # (efld SymbolDef Expr)
  # EnumDecl ::= (enum Type EnumFieldDecl+)
  inc n
  c.add TypedefKeyword
  let baseType = n
  c.genType n
  c.add Space
  c.add name
  c.add Semicolon
  c.add NewLine

  while n.kind != ParRi:
    if n.substructureKind == EfldU:
      inc n
      if n.kind == SymbolDef:
        let enumFieldName = mangle(pool.syms[n.symId])
        inc n
        c.add "#define "
        c.add enumFieldName
        c.add Space
        c.add ParLe
        c.add ParLe
        var base = baseType
        c.genType base
        c.add ParRi
        case n.kind
        of IntLit:
          c.genIntLit n.intId
          inc n
        of UIntLit:
          c.genUIntLit n.uintId
          inc n
        else:
          error c.m, "expected `Number` but got: ", n
        c.add ParRi
        c.add NewLine
      else:
        error c.m, "expected `SymbolDef` but got: ", n
      skipParRi n
    else:
      error c.m, "expected `efld` but got: ", n
  inc n # ParRi

proc generateTypes(c: var GeneratedCode; o: TypeOrder) =
  for (d, declKeyword) in o.forwardedDecls.s:
    var n = d
    let decl = takeTypeDecl(n)
    let s = mangle(pool.syms[decl.name.symId])
    c.add declKeyword
    c.add s
    c.add Space
    c.add s
    c.add Semicolon

  for (d, declKeyword) in o.ordered.s:
    var n = d
    var decl = takeTypeDecl(n)
    if not c.generatedTypes.containsOrIncl(decl.name.symId):
      let s = mangle(pool.syms[decl.name.symId])
      case decl.body.typeKind
      of ArrayT:
        c.add declKeyword
        c.add s
        c.add CurlyLe
        var n = decl.body.firstSon
        genType c, n, "a"
        c.add BracketLe
        case n.kind
        of IntLit: c.genIntLit n.intId
        of UIntLit: c.genUIntLit n.uintId
        else: error c.m, "array size must be an int literal, but got: ", n
        c.add BracketRi
        c.add Semicolon
        c.add CurlyRi
        c.add s
        c.add Semicolon
      of EnumT:
        genEnumDecl c, decl.body, s
      of ProctypeT:
        c.add TypedefKeyword
        genType c, decl.body, s
        c.add Semicolon
      else:
        c.add declKeyword
        c.add s
        c.add CurlyLe
        # XXX generate attributes and pragmas here
        c.genObjectOrUnionBody decl.body
        c.add CurlyRi
        c.add s
        c.add Semicolon
