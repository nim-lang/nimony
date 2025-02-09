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
  case pool.integers[t.intId]
  of -1:
    ""
  else: # 8, 16, 32, 64 etc.
    $res

proc genProcTypePragma(c: var GeneratedCode; n: var Cursor; isVarargs: var bool) =
  # ProcTypePragma ::= CallingConvention | (varargs) | Attribute
  case types[n].kind
  of CallingConventions:
    discard "already handled"
  of VarargsC:
    isVarargs = true
  of AttrC:
    c.add " __attribute__((" & toString(n.firstSon, c.m) & "))"
  else:
    error c.m, "invalid proc type pragma: ", n

proc genProcTypePragmas(c: var GeneratedCode; n: var Cursor; isVarargs: var bool) =
  if types[n].kind == Empty: return
  if types[n].kind == PragmasC:
    for ch in sons(n):
      genProcTypePragma(c, ch, isVarargs)
  else:
    error c.m, "expected proc type pragmas but got: ", n

proc genFieldPragmas(c: var GeneratedCode; n: var Cursor; bits: var string) =
  # CommonPragma ::= (align Number) | (was Identifier) | Attribute
  # FieldPragma ::= CommonPragma | (bits Number)
  if types[n].kind == Empty: return
  if types[n].kind == PragmasC:
    for ch in sons(n):
      case types[ch].kind
      of AlignC:
        c.add " NIM_ALIGN(" & toString(ch.firstSon, c.m) & ")"
      of WasC:
        c.add "/* " & toString(ch.firstSon, c.m) & " */"
      of AttrC:
        c.add " __attribute__((" & toString(ch.firstSon, c.m) & "))"
      of BitsC:
        bits = toString(ch.firstSon, c.m)
      else:
        error c.m, "invalid proc type pragma: ", ch
  else:
    error c.m, "expected field pragmas but got: ", n

proc getNumberQualifier(c: var GeneratedCode; t: TypeId): string =
  case types[t].kind
  of RoC:
    result = "const "
  of AtomicC:
    if c.m.config.backend == backendC:
      result = "_Atomic "
    else:
      # TODO: cpp doesn't support _Atomic
      result = ""
  else:
    raiseAssert "unreachable: " & $types[t].kind

proc getPtrQualifier(c: var GeneratedCode; t: TypeId): string =
  case types[t].kind
  of RoC:
    result = "const "
  of AtomicC:
    if c.m.config.backend == backendC:
      result = "_Atomic "
    else:
      # TODO: cpp doesn't support _Atomic
      result = ""
  of RestrictC:
    result = "restrict "
  else:
    raiseAssert "unreachable: " & $types[t].kind

proc genType(c: var GeneratedCode; t: TypeId; name = "")

template maybeAddName(c: var GeneratedCode; name: string) =
  if name != "":
    c.add Space
    c.add name

template atom(c: var GeneratedCode; s: string; name: string) =
  c.add s
  maybeAddName(c, name)

proc atomNumber(c: var GeneratedCode, t: TypeId, typeName: string, name: string, isBool = false) =
  if isBool:
    for son in sons(t):
      c.add getNumberQualifier(c, son)
    atom(c, typeName, name)
  else:
    var i = 0
    var s = ""
    for son in sons(t):
      if i == 0:
        s = typeName & types.integralBits(son)
      else:
        c.add getNumberQualifier(c, son)
      inc i
    atom(c, s, name)

proc atomPointer(c: var GeneratedCode, t: TypeId; name: string) =
  var i = 0
  for son in sons(t):
    if i == 0:
      discard
    else:
      c.add getPtrQualifier(c, son)
    inc i
  genType c, elementType(t)
  c.add Star
  maybeAddName(c, name)

proc genType(c: var GeneratedCode; t: TypeId; name = "") =
  case types[t].kind
  of VoidC: atom(c, "void", name)
  of IntC:
    atomNumber(c, t, "NI", name)
  of UIntC:
    atomNumber(c, t, "NU", name)
  of FloatC:
    atomNumber(c, t, "NF", name)
  of BoolC:
    atomNumber(c, t, "NB8", name, isBool = true)
  of CharC:
    atomNumber(c, t, "NC", name)
  of Sym:
    atom(c, mangle(c.m.lits.strings[types[t].litId]), name)
  of PtrC, APtrC:
    atomPointer(c, t, name)
  of FlexarrayC:
    genType c, elementType(t)
    maybeAddName(c, name)
    c.add BracketLe
    c.add BracketRi
  of ProctypeC:
    let decl = asProcType(t)
    var lastCallConv = Empty
    if types[decl.pragmas].kind == PragmasC:
      for ch in sons(decl.pragmas):
        case types[ch].kind
        of CallingConventions:
          lastCallConv = types[ch].kind
        else:
          discard
    var isVarargs = false
    if lastCallConv != Empty:
      c.add CallingConvToStr[lastCallConv]
      c.add "_PTR"
      c.add ParLe
      if types[decl.returnType].kind == Empty:
        c.add "void"
      else:
        genType c, decl.returnType
      c.add Comma
      genProcTypePragmas c, decl.pragmas, isVarargs
      maybeAddName(c, name)
      c.add ParRi
    else:
      if types[decl.returnType].kind == Empty:
        c.add "void"
      else:
        genType c, decl.returnType
      c.add Space
      c.add ParLe
      genProcTypePragmas c, decl.pragmas, isVarargs
      c.add Star # "(*fn)"
      maybeAddName(c, name)
      c.add ParRi
    c.add ParLe
    var i = 0
    for ch in sons(decl.params):
      let param = asParamDecl(ch)
      if i > 0: c.add Comma
      genType c, param.typ
      inc i
    if isVarargs:
      if i > 0: c.add Comma
      c.add "..."
    if i == 0:
      c.add "void"
    c.add ParRi
  else:
    error c.m, "node is not a type: ", t

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
