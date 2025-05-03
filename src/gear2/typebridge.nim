#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

# included from bridge.nim

## Implements the mapping from Nim type graph to NIF ("gear2").

proc writeFlags[E](b: var Builder; flags: set[E]; tag: string) =
  var flagsAsIdent = ""
  genFlags(flags, flagsAsIdent)
  if flagsAsIdent.len > 0:
    b.withTree tag:
      b.addIdent flagsAsIdent

proc writeTypeFlags(c: var WContext; t: PType) =
  writeFlags c.b, t.flags, "tf"

proc isNominalRef(t: PType): bool {.inline.} =
  let e = t.elementType
  t.sym != nil and e.kind == tyObject and (e.sym == nil or sfAnon in e.sym.flags)

template singleElement(keyw: string) {.dirty.} =
  c.b.withTree keyw:
    writeTypeFlags(c, t)
    if t.hasElementType:
      toNif t.elementType, parent, c
    else:
      c.b.addEmpty

proc atom(t: PType; c: var WContext; tag: string) =
  c.b.withTree tag:
    writeTypeFlags(c, t)

proc atom(t: PType; c: var WContext) =
  c.b.withTree toNifTag(t.kind):
    writeTypeFlags(c, t)

template typeHead(c: var WContext; t: PType; body: untyped) =
  c.b.withTree toNifTag(t.kind):
    writeTypeFlags(c, t)
    body

proc toNif*(t: PType; parent: PNode; c: var WContext) =
  if t == nil:
    c.b.addKeyw "missing"
    return

  case t.kind
  of tyNone: atom t, c
  of tyBool: atom t, c
  of tyChar: atom t, c, "c 8"
  of tyEmpty: c.b.addEmpty
  of tyInt: atom t, c, "i -1"
  of tyInt8: atom t, c, "i 8"
  of tyInt16: atom t, c, "i 16"
  of tyInt32: atom t, c, "i 32"
  of tyInt64: atom t, c, "i 64"
  of tyUInt: atom t, c, "u -1"
  of tyUInt8: atom t, c, "u 8"
  of tyUInt16: atom t, c, "u 16"
  of tyUInt32: atom t, c, "u 32"
  of tyUInt64: atom t, c, "u 64"
  of tyFloat, tyFloat64: atom t, c, "f 64"
  of tyFloat32: atom t, c, "f 32"
  of tyFloat128: atom t, c, "f 128"
  of tyAlias:
    c.typeHead t:
      toNif t.skipModifier, parent, c
  of tyNil: atom t, c
  of tyUntyped: atom t, c
  of tyTyped: atom t, c
  of tyTypeDesc:
    c.typeHead t:
      if t.kidsLen == 0 or t.elementType.kind == tyNone:
        c.b.addEmpty
      else:
        toNif t.elementType, parent, c
  of tyGenericParam:
    # See the nim-sem spec:
    c.typeHead t:
      symToNif t.sym, c
      #c.b.addIntLit t.sym.position

  of tyGenericInst:
    c.typeHead t:
      toNif t.genericHead, parent, c
      for _, a in t.genericInstParams:
        toNif a, parent, c
  of tyGenericInvocation:
    c.typeHead t:
      toNif t.genericHead, parent, c
      for _, a in t.genericInvocationParams:
        toNif a, parent, c
  of tyGenericBody:
    #toNif t.last, parent, c
    c.typeHead t:
      for _, son in t.ikids: toNif son, parent, c
  of tyDistinct, tyEnum:
    if t.sym != nil:
      symToNif t.sym, c
    else:
      c.typeHead t:
        for _, son in t.ikids: toNif son, parent, c
  of tyPtr:
    if isNominalRef(t):
      symToNif t.sym, c
    else:
      c.typeHead t:
        toNif t.elementType, parent, c
  of tyRef:
    if isNominalRef(t):
      symToNif t.sym, c
    else:
      c.typeHead t:
        toNif t.elementType, parent, c
  of tyVar:
    c.b.withTree(if isOutParam(t): "out" else: "mut"):
      toNif t.elementType, parent, c
  of tyAnd:
    c.typeHead t:
      for _, son in t.ikids: toNif son, parent, c
  of tyOr:
    c.typeHead t:
      for _, son in t.ikids: toNif son, parent, c
  of tyNot:
    c.typeHead t: toNif t.elementType, parent, c

  of tyFromExpr:
    if t.n == nil:
      atom t, c, "err"
    else:
      c.typeHead t:
        toNif t.n, parent, c

  of tyArray:
    c.typeHead t:
      if t.hasElementType:
        toNif t.elementType, parent, c
        toNif t.indexType, parent, c
      else:
        c.b.addEmpty 2
  of tyUncheckedArray:
    c.typeHead t:
      if t.hasElementType:
        toNif t.elementType, parent, c
      else:
        c.b.addEmpty

  of tySequence:
    singleElement toNifTag(t.kind)

  of tyOrdinal:
    c.typeHead t:
      if t.hasElementType:
        toNif t.skipModifier, parent, c
      else:
        c.b.addEmpty

  of tySet: singleElement toNifTag(t.kind)
  of tyOpenArray: singleElement toNifTag(t.kind)
  of tyIterable: singleElement toNifTag(t.kind)
  of tyLent: singleElement toNifTag(t.kind)

  of tyTuple:
    c.typeHead t:
      if t.n != nil:
        for i in 0..<t.n.len:
          assert(t.n[i].kind == nkSym)
          c.b.withTree "kv":
            c.b.addIdent t.n[i].sym.name.s
            toNif t.n[i].sym.typ, parent, c
      else:
        for _, son in t.ikids: toNif son, parent, c

  of tyRange:
    c.typeHead t:
      toNif t.elementType, parent, c
      if t.n != nil and t.n.kind == nkRange and t.n.len == 2:
        toNif t.n[0], parent, c
        toNif t.n[1], parent, c
      else:
        c.b.addEmpty 2

  of tyProc:
    let kind = if tfIterator in t.flags: "iteratortype"
               else: "proctype"
    c.b.withTree kind:

      c.b.addEmpty # name
      for i, a in t.paramTypes:
        let j = paramTypeToNodeIndex(i)
        if t.n != nil and j < t.n.len and t.n[j].kind == nkSym:
          c.b.addIdent(t.n[j].sym.name.s)
          toNif a, parent, c
      if tfUnresolved in t.flags:
        c.b.addRaw "[*missing parameters*]"
      if t.returnType != nil:
        toNif t.returnType, parent, c
      else:
        c.b.addEmpty

      c.b.withTree "effects":
        # XXX model explicit .raises and .tags annotations
        if tfNoSideEffect in t.flags:
          c.b.addKeyw "noside"
        if tfThread in t.flags:
          c.b.addKeyw "gcsafe"

      c.b.withTree "pragmas":
        if t.callConv == ccNimCall and tfExplicitCallConv notin t.flags:
          discard "no calling convention to generate"
        else:
          c.b.addKeyw toNifTag(t.callConv)

  of tyVarargs:
    c.typeHead t:
      if t.hasElementType:
        toNif t.elementType, parent, c
      else:
        c.b.addEmpty
      if t.n != nil:
        toNif t.n, parent, c
      else:
        c.b.addEmpty

  of tySink: singleElement toNifTag(t.kind)
  of tyOwned: singleElement toNifTag(t.kind)
  of tyVoid: atom t, c
  of tyPointer: atom t, c
  of tyString: atom t, c
  of tyCstring: atom t, c
  of tyObject: symToNif t.sym, c
  of tyForward: atom t, c
  of tyError: atom t, c
  of tyBuiltInTypeClass:
    # XXX See what to do with this.
    c.typeHead t:
      if t.kidsLen == 0 or t.genericHead.kind == tyNone:
        c.b.addEmpty
      else:
        toNif t.genericHead, parent, c

  of tyUserTypeClass, tyConcept:
    # ^ old style concept.  ^ new style concept.
    if t.sym != nil:
      symToNif t.sym, c
    else:
      atom t, c, "err"
  of tyUserTypeClassInst:
    # "instantiated" old style concept. Whatever that even means.
    if t.sym != nil:
      symToNif t.sym, c
    else:
      atom t, c, "err"
  of tyCompositeTypeClass: toNif t.last, parent, c
  of tyInferred: toNif t.skipModifier, parent, c
  of tyAnything: atom t, c
  of tyStatic:
    c.typeHead t:
      if t.hasElementType:
        toNif t.skipModifier, parent, c
      else:
        c.b.addEmpty
      if t.n != nil:
        toNif t.n, parent, c
      else:
        c.b.addEmpty

proc expect(c: var Cursor; k: NifKind) =
  if c.kind == k:
    inc c
  else:
    when defined(debug):
      writeStackTrace()
    quit "[Nif parser] expected: " & $k & " but got: " & $c.kind & toString c

proc isType*(c: Cursor): bool =
  let k = parseTypeKind(pool.tags[c.tag])
  result = k != tyNone

proc readNode(c: var Cursor; r: var RContext): PNode

proc loadSym*(m: var RModule; nifName: string; entry: NifIndexEntry; r: var RContext): PSym =
  assert entry.offset != 0, "cannot lookup: " & nifName
  m.s.r.jumpTo entry.offset

  var buf: seq[PackedToken] = @[]
  discard nifstreams.parse(m.s.r, buf, entry.info)
  var c = fromBuffer(buf)
  let n = readNode(c, r)
  assert n != nil and n.len > 0
  assert n[0].kind == nkSym
  result = n[0].sym

proc loadSym*(m: var RModule; nifName: string; r: var RContext): PSym =
  var entry = m.index.public.getOrDefault(nifName)
  if entry.offset == 0:
    entry = m.index.private.getOrDefault(nifName)
  result = loadSym(m, nifName, entry, r)

proc loadSym*(nifName: string; r: var RContext): PSym =
  let modname = extractModule(nifName)
  if modname.len == 0:
    result = loadSym(r.modules[r.thisModule], nifName, r)
  else:
    r.openNifModule modname
    result = loadSym(r.modules[modname], nifName, r)

proc loadLocalSym*(nifName: string; r: var RContext): PSym =
  result = loadSym(r.modules[r.thisModule], nifName, r)

proc loadSym*(s: SymId; r: var RContext): PSym =
  let nifName = pool.syms[s]
  result = loadSym(nifName, r)

proc loadType*(s: SymId; r: var RContext): PType =
  result = loadSym(s, r).typ

proc loadType*(s: string; r: var RContext): PType =
  let s = loadSym(s, r)
  result = s.typ
  assert result != nil

proc readSym(c: Cursor; r: var RContext; info: TLineInfo): PSym =
  let s = c.symId
  result = r.syms.getOrDefault(s).sym
  if result == nil:
    var isGlobal = false
    let name = getIdent(r.identCache, extractBasename(pool.syms[s], isGlobal))
    result = newSym(r.symKind, name, r.idgen, r.owner, info)
    r.syms[s] = LoadedSym(state: Loaded, sym: result)

proc readSymDef(c: Cursor; r: var RContext; info: TLineInfo): PSym =
  let s = c.symId
  var isGlobal = false
  assert r.identCache != nil
  let name = getIdent(r.identCache, extractBasename(pool.syms[s], isGlobal))
  result = newSym(r.symKind, name, r.idgen, r.owner, info)
  # always overwrite the entry here so that local syms are modelled properly.
  r.syms[s] = LoadedSym(state: Loaded, sym: result)

proc getMagic(r: var RContext; m: TMagic; info: TLineInfo): PSym =
  result = r.magics.getOrDefault(m).sym
  if result == nil:
    let name = getIdent(r.identCache, $m & "_forged")
    result = newSym(skProc, name, r.idgen, r.owner, info)
    r.magics[m] = LoadedSym(state: Loaded, sym: result)

proc repair(n: PNode; r: var RContext; tag: string): PNode =
  case tag
  of "htype":
    result = n
    let typ = result[0].typ
    result = result[1]
    result.typ() = typ
  of "suf":
    result = n[0]
    case n[1].strVal
    of "i64":
      transitionIntKind result, nkInt64Lit
    of "i32":
      transitionIntKind result, nkInt32Lit
    of "i16":
      transitionIntKind result, nkInt16Lit
    of "i8":
      transitionIntKind result, nkInt8Lit
    of "u64":
      transitionIntKind result, nkUInt64Lit
    of "u32":
      transitionIntKind result, nkUInt32Lit
    of "u16":
      transitionIntKind result, nkUInt16Lit
    of "u8":
      transitionIntKind result, nkUInt8Lit
    of "f64":
      result.kind = nkFloat64Lit
    of "f32":
      result.kind = nkFloat32Lit
  else:
    let m = parseMagic(tag)
    if m != mNone:
      result = newNodeI(nkCall, n.info)
      result.add newSymNode(getMagic(r, m, n.info), n.info)
      for son in n: result.add son
    else:
      result = n

proc readType*(c: var Cursor; r: var RContext): PType

proc readNode(c: var Cursor; r: var RContext): PNode =
  let info = translateLineInfo(r, c.info)
  case c.kind
  of ParLe:
    let tag = pool.tags[c.tag]
    let k = parseNodeKind(tag)
    result = newNodeI(k, info)
    inc c
    if c.kind == ParLe and pool.tags[c.tag] == "nf":
      inc c
      if c.kind == Ident:
        result.flags = parseNodeFlags(pool.strings[c.litId])
      expect c, ParRi
    case k
    of nkTypeSection:
      var childCounter = 0
      while c.kind != ParRi:
        if childCounter == 4:
          let t = readType(c, r)
          result[0].sym.typ = t
          result.add newNode(nkEmpty)
        else:
          result.addAllowNil readNode(c, r)
        inc childCounter
    else:
      while c.kind != ParRi:
        result.addAllowNil readNode(c, r)
    expect c, ParRi
    if k == nkNone:
      result = repair(result, r, tag)
  of EofToken, ParRi:
    result = nil
  of UnknownToken:
    result = nil
    inc c
  of SymbolDef:
    result = newSymNode(readSymDef(c, r, info), info)
    inc c
  of Symbol:
    result = newSymNode(readSym(c, r, info), info)
    inc c
  of DotToken:
    result = newNodeI(nkEmpty, info)
    inc c
  of Ident:
    result = newIdentNode(getIdent(r.identCache, pool.strings[c.litId]), info)
    inc c
  of StringLit:
    result = newStrNode(pool.strings[c.litId], info)
    inc c
  of CharLit:
    result = newIntNode(nkCharLit, int64 c.uoperand)
    result.info = info
    inc c
  of IntLit:
    result = newIntNode(nkIntLit, pool.integers[c.intId])
    result.info = info
    inc c
  of UIntLit:
    result = newIntNode(nkUIntLit, cast[BiggestInt](pool.uintegers[c.uintId]))
    result.info = info
    inc c
  of FloatLit:
    result = newFloatNode(nkFloatLit, pool.floats[c.floatId])
    result.info = info
    inc c

proc readTypeKind(c: var Cursor; tag: string): TTypeKind =
  result = parseTypeKind(tag)
  inc c
  if result == tyNone:
    case tag
    of "i":
      inc c
      if c.kind == IntLit:
        case pool.integers[c.intId]
        of -1: result = tyInt
        of 1: result = tyInt8
        of 2: result = tyInt16
        of 4: result = tyInt32
        of 8: result = tyInt64
        else: assert false
      else:
        expect c, IntLit
    of "u":
      inc c
      if c.kind == IntLit:
        case pool.integers[c.intId]
        of -1: result = tyUInt
        of 1: result = tyUInt8
        of 2: result = tyUInt16
        of 4: result = tyUInt32
        of 8: result = tyUInt64
        else: assert false
      else:
        expect c, IntLit
    of "f":
      inc c
      if c.kind == IntLit:
        case pool.integers[c.intId]
        of -1: result = tyFloat
        of 4: result = tyFloat32
        of 8: result = tyFloat64
        else: assert false
      else:
        expect c, IntLit

proc readTypeImpl(c: var Cursor; r: var RContext; kind: TTypeKind; res: PType) =
  case kind
  of tyFromExpr, tyEnum:
    res.n = readNode(c, r)
  of tyStatic:
    res.addAllowNil readType(c, r)
    res.n = readNode(c, r)
  of tyObject:
    # inheritance:
    res.addAllowNil readType(c, r)
    res.n = readNode(c, r)
  else:
    while c.kind != ParRi:
      res.addAllowNil readType(c, r)
    expect c, ParRi

proc readType*(c: var Cursor; r: var RContext): PType =
  case c.kind
  of Symbol:
    let s = c.symId
    result = r.types.getOrDefault(s).typ
    if result == nil:
      let symA = r.syms.getOrDefault(s).sym
      if symA != nil:
        assert symA.kind == skType
        result = symA.typ
      else:
        result = loadType(s, r)
        r.types[s] = LoadedType(state: Loaded, typ: result)

  of ParLe:
    let tag = pool.tags[c.tag]
    if tag == "missing":
      result = nil
    else:
      let k = readTypeKind(c, tag)
      result = newType(k, r.idgen, r.owner)
      if c.kind == ParLe and pool.tags[c.tag] == "tf":
        inc c
        if c.kind == Ident:
          result.flags = parseTypeFlags pool.strings[c.litId]
          inc c
        else:
          expect c, Ident
        expect c, ParRi
      readTypeImpl c, r, k, result
  of UnknownToken, DotToken, Ident, SymbolDef, StringLit,
     CharLit, IntLit, UIntLit, FloatLit, ParRi, EofToken:
    expect c, ParLe
