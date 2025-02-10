#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
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
        traverseObjectBody(m, o, ch)
      o.ordered.add tracebackTypeC(ch), TypedefStruct
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
        var n = readonlyCursorAt(m.code, def.pos)
        let decl = takeTypeDecl(n)
        if not containsOrIncl(o.lookedAtBodies, decl.name.symId):
          recordDependencyImpl m, o, n, decl.body, viaPointer
    else:
      discard "uninteresting type as we only focus on the required struct declarations"

proc recordDependency(m: Module; o: var TypeOrder; parent, child: Cursor) =
  var viaPointer = false
  recordDependencyImpl m, o, parent, child, viaPointer

proc traverseObjectBody(m: Module; o: var TypeOrder; t: Cursor) =
  var n = t
  while n.kind != ParRi:
    if n.kind == Symbol:
      # inheritance
      recordDependency m, o, t, n
      inc n
    elif n.substructureKind == FldU:
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
    let n = readonlyCursorAt(m.code, ch)
    var nn = n
    let decl = takeTypeDecl(nn)
    let t = decl.body
    case t.typeKind
    of ObjectT:
      traverseObjectBody m, o, t
      o.ordered.add n, TypedefStruct
    of UnionT:
      traverseObjectBody m, o, t
      o.ordered.add n, TypedefUnion
    of ArrayT:
      traverseObjectBody m, o, t
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
        inc n
        c.add "/* " & toString(n, false) & " */"
        skip n
        skipParRi n
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
  of RestrictQ, NoQualifier:
    error c.m, "expected number qualifier but got: ", n

proc getPtrQualifier(c: var GeneratedCode; n: Cursor): string =
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
      skipParRi n
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
    atom(c, s, name)

proc atomPointer(c: var GeneratedCode; n: var Cursor; name: string) =
  inc n
  var elem = n
  skip n # element type
  while n.kind != ParRi:
    c.add getPtrQualifier(c, n)
    skip n
  inc n # ParRi
  genType c, elem
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

proc genObjectOrUnionBody(c: var GeneratedCode; n: Cursor) =
  for x in sons(n):
    case types[x].kind
    of FldC:
      let decl = asFieldDecl(x)
      let f = mangleField(c, decl.name)
      var bits = ""
      genFieldPragmas c, decl.pragmas, bits
      genType c, decl.typ, f
      if bits.len > 0:
        c.add " : "
        c.add bits
      c.add Semicolon
    of Sym:
      genType c, x, "Q"
      c.add Semicolon
    else: discard

proc genEnumDecl(c: var GeneratedCode; n: Cursor; name: string) =
  # (efld SymbolDef Expr)
  # EnumDecl ::= (enum Type EnumFieldDecl+)
  let baseType = n.firstSon
  c.add TypedefKeyword
  c.genType t, baseType
  c.add Space
  c.add name
  c.add Semicolon
  c.add NewLine

  for ch in sonsFromX(t, n):
    if t[ch].kind == EfldC:
      let (a, b) = sons2(t, ch)
      if t[a].kind == SymDef:
        let enumFieldName = mangle(c.m.lits.strings[t[a].litId])
        c.add "#define "
        c.add enumFieldName
        c.add Space
        c.add ParLe
        c.add ParLe
        c.genType t, baseType
        c.add ParRi
        case t[b].kind
        of IntLit: c.genIntLit t[b].litId
        of UIntLit: c.genUIntLit t[b].litId
        else:
          error c.m, "expected `Number` but got: ", t, a
        c.add ParRi
        c.add NewLine
      else:
        error c.m, "expected `SymbolDef` but got: ", t, a
    else:
      error c.m, "expected `efld` but got: ", t, ch

proc generateTypes(c: var GeneratedCode; o: TypeOrder) =
  for (d, declKeyword) in o.forwardedDecls.s:
    let decl = asTypeDecl(d)
    let s = mangle(c.m.lits.strings[types[decl.name].litId])
    c.add declKeyword
    c.add s
    c.add Space
    c.add s
    c.add Semicolon

  for (d, declKeyword) in o.ordered.s:
    let decl = asTypeDecl(d)
    let litId = types[decl.name].litId
    if not c.generatedTypes.containsOrIncl(litId.int):
      let s = mangle(c.m.lits.strings[litId])
      case types[decl.body].kind
      of ArrayC:
        c.add declKeyword
        c.add CurlyLe
        let (elem, size) = sons2(decl.body)
        genType c, elem, "a"
        c.add BracketLe
        c.genIntLit types[size].litId
        c.add BracketRi
        c.add Semicolon
        c.add CurlyRi
        c.add s
        c.add Semicolon
      of EnumC:
        genEnumDecl c, decl.body, s
      of ProctypeC:
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
