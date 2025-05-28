#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from genpreasm.nim

## Generates PreASM types from NIFC types.

proc mergeBranch(arg: var AsmSlot; value: AsmSlot) =
  arg.offset = max(arg.offset, value.offset)
  arg.align = max(arg.align, value.align)

type
  TypeList = object
    processed: IntSet
    s: seq[Cursor]

proc add(dest: var TypeList; elem: Cursor) =
  if not containsOrIncl(dest.processed, elem.toUniqueId()):
    dest.s.add elem

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
      ch = elementType(ch)
    else:
      break

  case ch.typeKind
  of ObjectT, UnionT:
    let obj = ch
    if viaPointer:
      discard "we know the size of a pointer anyway"
    else:
      if not containsOrIncl(o.lookedAt, obj.toUniqueId):
        traverseObjectBody(m, o, obj)
      o.ordered.add tracebackTypeC(ch)
  of ArrayT:
    if viaPointer:
      discard "we know the size of a pointer anyway"
    else:
      if not containsOrIncl(o.lookedAt, ch.toUniqueId):
        traverseObjectBody(m, o, ch)
      o.ordered.add tracebackTypeC(ch)
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

proc traverseTypes(m: Module; o: var TypeOrder) =
  for ch in m.types:
    let n = readonlyCursorAt(m.src, ch)
    let decl = asTypeDecl(n)
    let t = decl.body
    case t.typeKind
    of ObjectT:
      traverseObjectBody m, o, t
      o.ordered.add n
    of UnionT:
      traverseObjectBody m, o, t
      o.ordered.add n
    of ArrayT:
      recordDependency m, o, t, t.firstSon
      o.ordered.add n
    of ProctypeT:
      o.ordered.add n
    of EnumT:
      o.ordered.add n
    else: discard

template integralBits(c: GeneratedCode; t: Cursor): int =
  let res = pool.integers[t.firstSon.intId]
  if res == -1:
    c.intmSize
  else: # 8, 16, 32, 64 etc.
    res

proc genFieldPragmas(c: var GeneratedCode; n: Cursor;
                     field: var AsmSlot) =
  # CommonPragma ::= (align Number) | (was Identifier) | Attribute
  # FieldPragma ::= CommonPragma | (bits Number)
  var n = n
  if n.kind == DotToken:
    discard
  elif n.substructureKind == PragmasU:
    inc n
    while n.kind != ParRi:
      case n.pragmaKind
      of AlignP:
        inc n
        field.align = pool.integers[n.intId]
        inc n
        skipParRi n
      of WasP, AttrP:
        skip n
      of BitsP:
        error c.m, "bit sizes fields are not supported: ", n
        skip n
      else:
        error c.m, "invalid proc type pragma: ", n
        skip n
    inc n
  else:
    error c.m, "expected field pragmas but got: ", n
    skip n

proc fieldName(c: var GeneratedCode; n: Cursor): SymId =
  if n.kind in {Symbol, SymbolDef}:
    result = n.symId
  else:
    error c.m, "field name must be a SymDef, but got: ", n
    result = SymId(0)

proc setField(c: var GeneratedCode; name: SymId; obj: AsmSlot; t: var AsmSlot) =
  t.offset = obj.size + (obj.size mod t.align)
  c.fields[name] = t

proc fillTypeSlot(c: var GeneratedCode; t: Cursor; dest: var AsmSlot)

proc genObjectBody(c: var GeneratedCode; n: Cursor;
                   obj: var AsmSlot; k: NifcType) =
  obj.kind = AMem
  var n = n.firstSon
  if n.kind == Symbol:
    # inheritance
    fillTypeSlot c, n, obj
    inc n
  elif n.kind == DotToken:
    inc n
  else:
    error c.m, "expected `Symbol` or `.` for inheritance but got: ", n
  while n.substructureKind == FldU:
    let decl = takeFieldDecl(n)
    let fn = fieldName(c, decl.name)
    var f = AsmSlot()
    genFieldPragmas c, decl.pragmas, f
    fillTypeSlot c, decl.typ, f
    setField c, fn, obj, f
    if k == ObjectT:
      inc obj.size, f.size
    else:
      # union:
      obj.size = max(obj.size, f.size)
    obj.align = max(obj.align, f.align)
  # padding at object end:
  obj.size = obj.size + (obj.size mod obj.align)

proc inBytes(bits: int): int =
  if bits < 0: 8 # wordsize
  else: bits div 8

proc fillTypeSlot(c: var GeneratedCode; t: Cursor; dest: var AsmSlot) =
  let k = t.typeKind
  case k
  of VoidT:
    error c.m, "internal error: Cannot handle 'void' type: ", t
  of IT:
    let bytes = integralBits(c, t).inBytes
    dest = AsmSlot(kind: AInt, size: bytes, align: bytes)
  of UT, CT:
    let bytes = integralBits(c, t).inBytes
    dest = AsmSlot(kind: AUInt, size: bytes, align: bytes)
  of FT:
    let bytes = integralBits(c, t).inBytes
    dest = AsmSlot(kind: AFloat, size: bytes, align: bytes)
  of BoolT:
    dest = AsmSlot(kind: ABool, size: 1, align: 1)
  of PtrT, APtrT, ProctypeT:
    dest = AsmSlot(kind: AUInt, size: c.intmSize, align: c.intmSize)
  of FlexarrayT:
    # Call `elementType` to get the alignment right:
    fillTypeSlot c, t.firstSon, dest
    dest.kind = AMem
    dest.size = 0
  of EnumT:
    let baseType = t.firstSon
    fillTypeSlot c, baseType, dest
  of ArrayT:
    var n = t.firstSon
    fillTypeSlot c, n, dest
    skip n
    if n.kind == IntLit:
      dest.size *= pool.integers[n.intId]
    else:
      error c.m, "expected `IntLit` but got: ", n
    dest.kind = AMem
  of ObjectT, UnionT:
    genObjectBody c, t, dest, k
  else:
    if t.kind == Symbol:
      let id = t.symId
      let def = c.m.defs.getOrDefault(id)
      if def.pos == 0:
        error c.m, "undeclared symbol: ", t
      else:
        if c.types.hasKey(id):
          dest = c.types[id]
        else:
          let n = readonlyCursorAt(c.m.src, def.pos)
          let decl = asTypeDecl(n)
          fillTypeSlot c, decl.body, dest
          c.types[id] = dest
    else:
      error c.m, "node is not a type: ", t

proc generateTypes(c: var GeneratedCode; o: TypeOrder) =
  for d in o.ordered.s:
    var n = d
    var decl = takeTypeDecl(n)
    if not c.generatedTypes.containsOrIncl(decl.name.symId):
      var dest = AsmSlot()
      fillTypeSlot c, decl.body, dest
      c.types[decl.name.symId] = dest

proc getAsmSlot(c: var GeneratedCode; n: Cursor): AsmSlot =
  let t = getType(c.m, n)
  if t.tagId == ErrT:
    error c.m, "cannot compute type of expression: ", n
  else:
    result = AsmSlot()
    fillTypeSlot c, t, result

proc typeToSlot(c: var GeneratedCode; t: Cursor): AsmSlot =
  result = AsmSlot()
  fillTypeSlot c, t, result
