## Plugin backing value-level `repr`.
##
## `repr` expands to `reprTyped(typeof(x), x)`. This plugin receives that
## `(typedesc T)` plus the value, resolves the nominal types it needs with
## `needTypes`, and emits a call to one of the formatting helpers in
## `std/private/reprimpl.nim`. Generic calls are deferred until instantiation,
## because object type names and the distinction between `object` and `ref
## object` can only be decided from the concrete type.

import std/strutils
import plugins
import typetraits_render
import ../private/reprimpl

type
  ReprKind = enum
    rkUnknown
    rkInt
    rkUInt
    rkFloat
    rkChar
    rkBool
    rkEnum
    rkAnum
    rkString
    rkCString
    rkPointer
    rkProc
    rkSeq
    rkOpenArray
    rkArray
    rkSet
    rkUncheckedArray
    rkSlice
    rkTupleNamed
    rkTuplePositional
    rkObject
    rkRefObject
    rkPtrObject
    rkRef
    rkPtr
    rkDistinct
    rkRange

proc scanType(defs: NifCursor; n: NifCursor; missing: var seq[SymId];
              hasTypevar: var bool; seen: var seq[SymId]) =
  ## Walk the part of a type graph the plugin has to decide on. Object bodies
  ## are deliberately not entered: an instantiated object has its field types
  ## substituted already, and the uninstantiated case is caught by the
  ## `typevars` slot. This keeps `needTypes` to the declarations that matter.
  var n = n
  if n.kind == Symbol:
    let decl = findDecl(defs, n.symId)
    if not decl.hasMore:
      if n.symId notin missing:
        missing.add n.symId
      return
    if isTypevarDecl(decl):
      hasTypevar = true
      return
    if n.symId in seen:
      return
    seen.add n.symId

    let td = asTypeDecl(decl)
    let typevars = td.typevars
    if typevars.typeKind == AtT:
      # A generic instance's head is the uninstantiated declaration; walking
      # it would rediscover its type variables. Only the arguments matter.
      var a = firstChild(typevars)
      if a.hasMore:
        skip a
      while a.hasMore:
        scanType(defs, a, missing, hasTypevar, seen)
        skip a
    else:
      scanType(defs, typevars, missing, hasTypevar, seen)

    let body = td.body
    if body.kind == TagLit and body.typeKind in {ObjectT, EnumT, AnumT, ProctypeT, ItertypeT}:
      discard
    else:
      scanType(defs, body, missing, hasTypevar, seen)
  elif n.kind == TagLit:
    case n.typeKind
    of ObjectT, EnumT, AnumT, ProctypeT, ItertypeT:
      discard
    of AtT:
      var a = firstChild(n)
      if a.hasMore:
        skip a
      while a.hasMore:
        scanType(defs, a, missing, hasTypevar, seen)
        skip a
    else:
      n.into:
        while n.hasMore:
          scanType(defs, n, missing, hasTypevar, seen)
          skip n

proc targetType(n: NifCursor): NifCursor =
  result = firstChild(n)
  while result.hasMore and result.kind != TagLit and
      result.otherKind in {NotnilU, NilU, UncheckedU}:
    skip result

proc headName(typevars: NifCursor): string =
  result = ""
  if typevars.typeKind == AtT:
    let head = firstChild(typevars)
    if head.kind == Symbol:
      result = symBasename(head.symId)
    elif head.kind == Ident:
      result = head.identText

proc tupleIsNamed(n: NifCursor): bool =
  result = false
  var c = firstChild(n)
  while c.hasMore:
    if c.otherKind == KvU:
      return true
    skip c

proc classify(defs: NifCursor; n: NifCursor; missing: var seq[SymId];
              seen: var seq[SymId]): ReprKind =
  var n = n
  if n.kind == Symbol:
    let base = symBasename(n.symId)
    case base
    of "string": return rkString
    of "cstring": return rkCString
    of "bool": return rkBool
    else: discard

    if n.symId in seen:
      return rkUnknown
    let decl = findTypeDecl(defs, n.symId)
    if not decl.hasMore:
      if n.symId notin missing:
        missing.add n.symId
      return rkUnknown
    seen.add n.symId

    let td = asTypeDecl(decl)
    case headName(td.typevars)
    of "seq": return rkSeq
    of "openArray": return rkOpenArray
    of "UncheckedArray": return rkUncheckedArray
    of "HSlice": return rkSlice
    else: discard

    return classify(defs, td.body, missing, seen)
  elif n.kind != TagLit:
    return rkUnknown

  case n.typeKind
  of IT: rkInt
  of UT: rkUInt
  of FT: rkFloat
  of CT: rkChar
  of BoolT: rkBool
  of EnumT: rkEnum
  of AnumT: rkAnum
  of ObjectT: rkObject
  of CstringT: rkCString
  of PointerT: rkPointer
  of ProctypeT, ItertypeT: rkProc
  of ArrayT: rkArray
  of SetT: rkSet
  of UarrayT: rkUncheckedArray
  of TupleT, ClosureTupleT:
    if tupleIsNamed(n): rkTupleNamed else: rkTuplePositional
  of DistinctT: rkDistinct
  of RangetypeT: rkRange
  of RefT, PtrT:
    let target = targetType(n)
    let targetKind = classify(defs, target, missing, seen)
    if targetKind == rkObject:
      if n.typeKind == RefT: rkRefObject else: rkPtrObject
    else:
      if n.typeKind == RefT: rkRef else: rkPtr
  of AtT:
    case headName(n)
    of "seq": rkSeq
    of "openArray": rkOpenArray
    of "UncheckedArray": rkUncheckedArray
    of "HSlice": rkSlice
    else: rkUnknown
  of TypekindT:
    var child = firstChild(n)
    if child.hasMore:
      classify(defs, child, missing, seen)
    else:
      rkUnknown
  else:
    rkUnknown

func stripObjSuffix(s: string): string =
  let p = s.find(".Obj")
  result = s
  if p >= 0:
    result.delete(p .. p + 3)

proc renderObjectType(defs: NifCursor; target: NifCursor): string =
  ## Unwrap only the hidden `T.Obj` implementation type of a `ref object`;
  ## an explicit `ref T`/`ptr T` keeps its prefix.
  var inner = targetType(target)
  let innerName = renderTypeName(defs, inner)
  if innerName.contains(".Obj"):
    result = stripObjSuffix(innerName)
  else:
    var outer = target
    result = renderTypeName(defs, outer)

proc reprTypeName(defs: NifCursor; orig: NifCursor; kind: ReprKind): string =
  var n = orig
  case kind
  of rkRefObject, rkPtrObject:
    if n.kind == Symbol:
      result = renderTypeName(defs, n)
    else:
      result = renderObjectType(defs, n)
  else:
    result = renderTypeName(defs, n)

proc bindHelper(b: var NifBuilder; name: string) =
  ## `bindSym` only accepts literals, so map the dynamic helper name here.
  case name
  of "reprInt": b.bindSym "reprInt"
  of "reprUInt": b.bindSym "reprUInt"
  of "reprFloat": b.bindSym "reprFloat"
  of "reprBool": b.bindSym "reprBool"
  of "reprEnum": b.bindSym "reprEnum"
  of "reprChar": b.bindSym "reprChar"
  of "reprString": b.bindSym "reprString"
  of "reprCstring": b.bindSym "reprCstring"
  of "reprPointer": b.bindSym "reprPointer"
  of "reprProc": b.bindSym "reprProc"
  of "reprSeq": b.bindSym "reprSeq"
  of "reprOpenArray": b.bindSym "reprOpenArray"
  of "reprArray": b.bindSym "reprArray"
  of "reprSet": b.bindSym "reprSet"
  of "reprUncheckedArray": b.bindSym "reprUncheckedArray"
  of "reprSlice": b.bindSym "reprSlice"
  of "reprTupleNamed": b.bindSym "reprTupleNamed"
  of "reprTuplePositional": b.bindSym "reprTuplePositional"
  of "reprObject": b.bindSym "reprObject"
  of "reprRefObject": b.bindSym "reprRefObject"
  of "reprPtrObject": b.bindSym "reprPtrObject"
  of "reprRef": b.bindSym "reprRef"
  of "reprPtr": b.bindSym "reprPtr"
  else: b.addIdent name

proc singleCall(name: string; info: LineInfo; value: var NifCursor): NifBuilder =
  result = createTree()
  result.withTree CallX, info:
    result.bindHelper name
    result.takeTree value

proc typeNameCall(name, typeName: string; info: LineInfo;
                  value: var NifCursor): NifBuilder =
  result = createTree()
  result.withTree CallX, info:
    result.bindHelper name
    result.takeTree value
    result.addStrLit typeName

proc baseCall(info: LineInfo; value: var NifCursor; base: NifCursor): NifBuilder =
  result = createTree()
  result.withTree CallX, info:
    result.bindSym "reprBase"
    result.takeTree value
    var b = base
    result.takeTree b

proc anumCall(info: LineInfo; value: var NifCursor; body: NifCursor): NifBuilder =
  var names: seq[(int, string)] = @[]
  var maxOrd = -1
  var c = firstChild(body)
  while c.hasMore:
    if c.otherKind == EfldU:
      var name = ""
      var ord = -1
      var d = firstChild(c)
      if d.hasMore and d.kind in {Symbol, SymbolDef}:
        name = symBasename(d.symId)
      skip d
      while d.hasMore:
        if d.kind == TagLit and d.exprKind == TupX:
          var f = firstChild(d)
          if f.hasMore and f.kind == IntLit:
            ord = int(f.intVal)
        skip d
      if ord >= 0:
        names.add (ord, name)
        if ord > maxOrd:
          maxOrd = ord
    skip c

  var labels: seq[string] = @[]
  for i in 0 .. maxOrd:
    labels.add "?"
  for (ord, name) in names:
    if ord >= 0 and ord < labels.len:
      labels[ord] = name

  var underlying = firstChild(body)
  result = createTree()
  result.withTree CallX, info:
    result.bindSym "reprAnum"
    result.takeTree value
    result.takeTree underlying
    result.withTree TupX, info:
      for name in labels:
        result.addStrLit name

proc tr(root: NifCursor): NifBuilder =
  let info = root.info
  let name = pluginName(root)
  var args = callArgs(root)
  if name == "reprTypedesc":
    var target = args
    if target.typeKind == TypedescT:
      target = firstChild(target)
    let defs = loadTypeDefinitions()
    var missing: seq[SymId] = @[]
    var hasTypevar = false
    var seen: seq[SymId] = @[]
    scanType(defs, target, missing, hasTypevar, seen)
    if missing.len > 0:
      return needTypes(missing)
    var rendered = ""
    if target.kind == TagLit and target.typeKind in {RefT, PtrT}:
      var kindMissing: seq[SymId] = @[]
      var kindSeen: seq[SymId] = @[]
      let kind = classify(defs, target, kindMissing, kindSeen)
      if kindMissing.len > 0:
        return needTypes(kindMissing)
      if kind in {rkRefObject, rkPtrObject}:
        rendered = renderObjectType(defs, target)
      else:
        rendered = renderTypeName(defs, target)
    else:
      rendered = renderTypeName(defs, target)
    result = createTree()
    result.addStrLit rendered
    return
  var typ = args
  var value = args
  skip value
  var target = typ
  if target.typeKind == TypedescT:
    target = firstChild(target)

  let defs = loadTypeDefinitions()
  var missing: seq[SymId] = @[]
  var hasTypevar = false
  var seen: seq[SymId] = @[]
  scanType(defs, target, missing, hasTypevar, seen)
  if missing.len > 0:
    return needTypes(missing)
  if hasTypevar:
    return deferExpansion()

  var classifyMissing: seq[SymId] = @[]
  var classifySeen: seq[SymId] = @[]
  let kind = classify(defs, target, classifyMissing, classifySeen)
  if classifyMissing.len > 0:
    return needTypes(classifyMissing)

  case kind
  of rkInt: singleCall("reprInt", info, value)
  of rkUInt: singleCall("reprUInt", info, value)
  of rkFloat: singleCall("reprFloat", info, value)
  of rkBool: singleCall("reprBool", info, value)
  of rkEnum: singleCall("reprEnum", info, value)
  of rkChar: singleCall("reprChar", info, value)
  of rkString: singleCall("reprString", info, value)
  of rkCString: singleCall("reprCstring", info, value)
  of rkPointer: singleCall("reprPointer", info, value)
  of rkProc: singleCall("reprProc", info, value)
  of rkSeq: singleCall("reprSeq", info, value)
  of rkOpenArray: singleCall("reprOpenArray", info, value)
  of rkArray: singleCall("reprArray", info, value)
  of rkSet: singleCall("reprSet", info, value)
  of rkUncheckedArray: singleCall("reprUncheckedArray", info, value)
  of rkSlice: singleCall("reprSlice", info, value)
  of rkTupleNamed: singleCall("reprTupleNamed", info, value)
  of rkTuplePositional: singleCall("reprTuplePositional", info, value)
  of rkObject:
    let name = reprTypeName(defs, target, kind)
    typeNameCall("reprObject", name, info, value)
  of rkRefObject:
    let name = reprTypeName(defs, target, kind)
    typeNameCall("reprRefObject", name, info, value)
  of rkPtrObject:
    let name = reprTypeName(defs, target, kind)
    typeNameCall("reprPtrObject", name, info, value)
  of rkRef: singleCall("reprRef", info, value)
  of rkPtr: singleCall("reprPtr", info, value)
  of rkAnum:
    var anumBody = target
    if target.kind == Symbol:
      let decl = findTypeDecl(defs, target.symId)
      if decl.hasMore:
        anumBody = asTypeDecl(decl).body
    anumCall(info, value, anumBody)
  of rkDistinct, rkRange:
    var body = target
    if target.kind == Symbol:
      let decl = findTypeDecl(defs, target.symId)
      if decl.hasMore:
        body = asTypeDecl(decl).body
    # Resolve aliases until the modifier itself is reached.
    var fuel = 20
    while body.kind == Symbol and fuel > 0:
      dec fuel
      let decl = findTypeDecl(defs, body.symId)
      if not decl.hasMore:
        break
      body = asTypeDecl(decl).body
    if body.kind == TagLit and body.typeKind in {DistinctT, RangetypeT}:
      let base = firstChild(body)
      baseCall(info, value, base)
    else:
      errorTree("repr: cannot find the base type", root)
  of rkUnknown:
    errorTree("repr: unsupported type", root)

var inp = loadPluginInput()
saveTree tr(inp)
