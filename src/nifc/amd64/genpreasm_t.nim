#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
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
    s: seq[TypeId]

proc add(dest: var TypeList; elem: TypeId) =
  if not containsOrIncl(dest.processed, int(elem)):
    dest.s.add elem

type
  TypeOrder = object
    ordered: TypeList
    lookedAt, lookedAtBodies: IntSet

proc traverseObjectBody(m: Module; o: var TypeOrder; t: TypeId)

proc recordDependency(m: Module; o: var TypeOrder; parent, child: TypeId) =
  var ch = child
  var viaPointer = false
  while true:
    case m.code[ch].kind
    of APtrC, PtrC:
      viaPointer = true
      ch = elementType(m.code, ch)
    of FlexarrayC:
      ch = elementType(m.code, ch)
    else:
      break

  case m.code[ch].kind
  of ObjectC, UnionC:
    let obj = ch
    if viaPointer:
      discard "we know the size of a pointer anyway"
    else:
      if not containsOrIncl(o.lookedAt, obj.int):
        traverseObjectBody(m, o, obj)
      o.ordered.add tracebackTypeC(m, ch)
  of ArrayC:
    if viaPointer:
      discard "we know the size of a pointer anyway"
    else:
      if not containsOrIncl(o.lookedAt, ch.int):
        traverseObjectBody(m, o, ch)
      o.ordered.add tracebackTypeC(m, ch)
  of Sym:
    # follow the symbol to its definition:
    let id = m.code[ch].litId
    let def = m.defs.getOrDefault(id)
    if def.pos == NodePos(0):
      error m, "undeclared symbol: ", m.code, ch
    else:
      let decl = asTypeDecl(m.code, def.pos)
      if not containsOrIncl(o.lookedAtBodies, decl.body.int):
        recordDependency m, o, parent, decl.body
  else:
    discard "uninteresting type as we only focus on the required struct declarations"

proc traverseObjectBody(m: Module; o: var TypeOrder; t: TypeId) =
  for x in sons(m.code, t):
    case m.code[x].kind
    of FldC:
      let decl = asFieldDecl(m.code, x)
      recordDependency m, o, t, decl.typ
    of Sym:
      # inheritance
      recordDependency m, o, t, x
    else: discard

proc traverseTypes(m: Module; o: var TypeOrder) =
  for ch in m.types:
    let decl = asTypeDecl(m.code, ch)
    let t = decl.body
    case m.code[t].kind
    of ObjectC:
      traverseObjectBody m, o, t
    of UnionC:
      traverseObjectBody m, o, t
    of ArrayC:
      traverseObjectBody m, o, t
    of ProctypeC:
      o.ordered.add ch
    of EnumC:
      # (efld SymbolDef Expr)
      # EnumDecl ::= (enum Type EnumFieldDecl+)
      o.ordered.add ch
    else: discard

template integralBits(c: GeneratedCode; t: TypeDesc): int =
  let res = bits(c.m, t)
  if res == -1:
    c.intmSize
  else: # 8, 16, 32, 64 etc.
    res

proc genFieldPragmas(c: var GeneratedCode; types: TypeGraph; n: NodePos;
                     field: var AsmSlot; bits: var string) =
  # CommonPragma ::= (align Number) | (was Identifier) | Attribute
  # FieldPragma ::= CommonPragma | (bits Number)
  if types[n].kind == Empty:
    discard
  elif types[n].kind == PragmasC:
    for ch in sons(types, n):
      case types[ch].kind
      of AlignC:
        field.align = parseInt(c.m.lits.strings[types[ch.firstSon].litId])
      of WasC, AttrC:
        discard "ignore attributes"
      of BitsC:
        error c.m, "bit sizes fields are not supported: ", types, ch
      else:
        error c.m, "invalid proc type pragma: ", types, ch
  else:
    error c.m, "expected field pragmas but got: ", types, n

proc fieldName(c: var GeneratedCode; tree: TypeGraph; n: NodePos): LitId =
  if tree[n].kind in {Sym, SymDef}:
    result = tree[n].litId
  else:
    error c.m, "field name must be a SymDef, but got: ", tree, n
    result = LitId(0)

proc setField(c: var GeneratedCode; name: LitId; obj: AsmSlot; t: var AsmSlot) =
  t.offset = obj.size + (obj.size mod t.align)
  c.fields[name] = t

proc fillTypeSlot(c: var GeneratedCode; t: TypeDesc; dest: var AsmSlot)

proc genObjectBody(c: var GeneratedCode; n: NodePos;
                   obj: var AsmSlot; k: NifcKind) =
  obj.kind = AMem
  for x in sons(c.m.code, n):
    case c.m.code[x].kind
    of FldC:
      let decl = asFieldDecl(c.m.code, x)
      let fn = fieldName(c, c.m.code, decl.name)
      var bits = ""
      var f = AsmSlot()
      genFieldPragmas c, c.m.code, decl.pragmas, f, bits
      fillTypeSlot c, typeFromPos(decl.typ), f
      setField c, fn, obj, f
      if k == ObjectC:
        inc obj.size, f.size
      else:
        # union:
        obj.size = max(obj.size, f.size)
      obj.align = max(obj.align, f.align)
    of Sym:
      fillTypeSlot c, typeFromPos(x), obj
    else: discard
  # padding at object end:
  obj.size = obj.size + (obj.size mod obj.align)

proc inBytes(bits: int): int =
  if bits < 0: 8 # wordsize
  else: bits div 8

proc fillTypeSlot(c: var GeneratedCode; t: TypeDesc; dest: var AsmSlot) =
  let k = kind(c.m.code, t)
  case k
  of VoidC:
    error c.m, "internal error: Cannot handle 'void' type: ", c.m.code, rawPos(t)
  of IntC:
    let bytes = integralBits(c, t).inBytes
    dest = AsmSlot(kind: AInt, size: bytes, align: bytes)
  of UIntC, CharC:
    let bytes = integralBits(c, t).inBytes
    dest = AsmSlot(kind: AUInt, size: bytes, align: bytes)
  of FloatC:
    let bytes = integralBits(c, t).inBytes
    dest = AsmSlot(kind: AFloat, size: bytes, align: bytes)
  of BoolC:
    dest = AsmSlot(kind: ABool, size: 1, align: 1)
  of Sym:
    let id = c.m.code[rawPos t].litId
    let def = c.m.defs.getOrDefault(id)
    if def.pos == NodePos(0):
      error c.m, "undeclared symbol: ", c.m.code, rawPos(t)
    else:
      if c.types.hasKey(id):
        dest = c.types[id]
      else:
        let decl = asTypeDecl(c.m.code, def.pos)
        fillTypeSlot c, typeFromPos(decl.body), dest
        c.types[id] = dest
  of PtrC, APtrC, ProctypeC:
    dest = AsmSlot(kind: AUInt, size: c.intmSize, align: c.intmSize)
  of FlexarrayC:
    # Call `elementType` to get the alignment right:
    fillTypeSlot c, typeFromPos(elementType(c.m.code, rawPos(t))), dest
    dest.kind = AMem
    dest.size = 0
  of EnumC:
    let baseType = rawPos(t).firstSon
    fillTypeSlot c, typeFromPos(baseType), dest
  of ArrayC:
    let (elem, size) = sons2(c.m.code, rawPos(t))
    fillTypeSlot c, typeFromPos(elem), dest
    dest.kind = AMem
    dest.size *= parseInt(c.m.lits.strings[c.m.code[size].litId])
  of ObjectC, UnionC:
    genObjectBody c, rawPos(t), dest, k
  else:
    error c.m, "node is not a type: ", c.m.code, rawPos(t)

proc generateTypes(c: var GeneratedCode; o: TypeOrder) =
  for d in o.ordered.s:
    let decl = asTypeDecl(c.m.code, d)
    let litId = c.m.code[decl.name].litId
    if not c.generatedTypes.containsOrIncl(litId.int):
      var dest = AsmSlot()
      fillTypeSlot c, typeFromPos(decl.body), dest
      c.types[litId] = dest

proc getAsmSlot(c: var GeneratedCode; n: NodePos): AsmSlot =
  let t = getType(c.m, c.m.code, n)
  if isError(t):
    error c.m, "cannot compute type of expression: ", c.m.code, n
  else:
    result = AsmSlot()
    fillTypeSlot c, t, result

proc typeToSlot(c: var GeneratedCode; t: NodePos): AsmSlot =
  result = AsmSlot()
  fillTypeSlot c, typeFromPos(t), result

proc typeToSlot(c: var GeneratedCode; t: TypeDesc): AsmSlot =
  result = AsmSlot()
  fillTypeSlot c, t, result
