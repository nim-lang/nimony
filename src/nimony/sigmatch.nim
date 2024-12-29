#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [sets, tables, assertions]

import bitabs, nifreader, nifstreams, nifcursors, lineinfos

import nimony_model, decls, programs, semdata, typeprops

type
  Item* = object
    n*, typ*: Cursor
    kind*: SymKind

  FnCandidate* = object
    kind*: SymKind
    sym*: SymId
    typ*: Cursor

  MatchError* = object
    info: PackedLineInfo
    msg: string
    pos: int

  Match* = object
    inferred*: Table[SymId, Cursor]
    tvars: HashSet[SymId]
    fn*: FnCandidate
    args*, typeArgs*: TokenBuf
    err*, flipped*: bool
    skippedMod: TypeKind
    argInfo: PackedLineInfo
    pos, opened: int
    inheritanceCosts, intCosts: int
    returnType*: Cursor
    context: ptr SemContext
    error: MatchError
    firstVarargPosition*: int

proc createMatch*(context: ptr SemContext): Match = Match(context: context, firstVarargPosition: -1)

proc concat(a: varargs[string]): string =
  result = a[0]
  for i in 1..high(a): result.add a[i]

proc typeToString*(n: Cursor): string =
  result = toString(n, false)

proc error(m: var Match; msg: sink string) =
  if m.err: return # first error is the important one
  m.err = true
  m.error = MatchError(info: m.argInfo, msg: msg, pos: m.pos+1)

proc addErrorMsg*(dest: var string; m: Match) =
  assert m.err
  dest.add "[" & $(m.error.pos) & "] " & m.error.msg

proc addErrorMsg*(dest: var TokenBuf; m: Match) =
  assert m.err
  dest.addParLe ErrT, m.argInfo
  dest.addDotToken()
  let str = "For type " & typeToString(m.fn.typ) & " mismatch at position\n" &
    "[" & $(m.pos+1) & "] " & m.error.msg
  dest.addStrLit str
  dest.addParRi()

proc expected(f, a: Cursor): string =
  concat("expected: ", typeToString(f), " but got: ", typeToString(a))

proc typeImpl(s: SymId): Cursor =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  result = res.decl
  assert result.stmtKind == TypeS
  inc result # skip ParLe
  for i in 1..4:
    skip(result) # name, export marker, pragmas, generic parameter

proc objtypeImpl*(s: SymId): Cursor =
  result = typeImpl(s)
  let k = typeKind result
  if k in {RefT, PtrT}:
    inc result

proc getTypeSection*(s: SymId): TypeDecl =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  result = asTypeDecl(res.decl)

proc getProcDecl*(s: SymId): Routine =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  result = asRoutine(res.decl)

proc isObjectType(s: SymId): bool =
  let impl = objtypeImpl(s)
  result = impl.typeKind == ObjectT

proc isEnumType*(n: Cursor): bool =
  if n.kind == Symbol:
    let impl = getTypeSection(n.symId)
    result = impl.kind == TypeY and impl.body.typeKind in {EnumT, HoleyEnumT}
  else:
    result = false

proc isConcept(s: SymId): bool =
  #let impl = typeImpl(s)
  # XXX Model Concept in the grammar
  #result = impl.tag == ConceptT
  result = false

iterator inheritanceChain(s: SymId): SymId =
  var objbody = objtypeImpl(s)
  while true:
    let od = asObjectDecl(objbody)
    if od.kind == ObjectT:
      var parent = od.parentType
      if parent.typeKind in {RefT, PtrT}:
        inc parent
      if parent.kind == Symbol:
        let ps = parent.symId
        yield ps
        objbody = objtypeImpl(ps)
      else:
        break
    else:
      break

proc matchesConstraint(m: var Match; f: var Cursor; a: Cursor): bool

proc matchesConstraintAux(m: var Match; f: var Cursor; a: Cursor): bool =
  result = false
  case f.typeKind
  of NotT:
    inc f
    if not matchesConstraint(m, f, a):
      result = true
    if f.kind != ParRi: result = false
    skipToEnd f
  of AndT:
    inc f
    result = true
    while f.kind != ParRi:
      if not matchesConstraint(m, f, a):
        result = false
        break
    skipToEnd f
  of OrT:
    inc f
    while f.kind != ParRi:
      if matchesConstraint(m, f, a):
        result = true
        break
    skipToEnd f
  of ConceptT:
    # XXX Use some algorithm here that can cache the result
    # so that it can remember e.g. "int fulfils Fibable". For
    # now this should be good enough for our purposes:
    result = true
    skip f
  of TypeKindT:
    var aTag = a
    if a.kind == Symbol:
      aTag = typeImpl(a.symId)
    if aTag.typeKind == TypeKindT:
      inc aTag
    inc f
    assert f.kind == ParLe
    result = aTag.kind == ParLe and f.tagId == aTag.tagId
    inc f
    assert f.kind == ParRi
    inc f
    assert f.kind == ParRi
    inc f
  of OrdinalT:
    result = isOrdinalType(a)
    skip f
  else:
    result = false

proc matchesConstraint(m: var Match; f: var Cursor; a: Cursor): bool =
  result = false
  if f.kind == DotToken:
    result = true
    inc f
  elif a.kind == Symbol:
    let res = tryLoadSym(a.symId)
    assert res.status == LacksNothing
    if res.decl.symKind == TypevarY:
      var typevar = asTypevar(res.decl)
      result = matchesConstraint(m, f, typevar.typ)
    elif res.decl.symKind == TypeY:
      result = matchesConstraintAux(m, f, a)
  elif f.kind == Symbol:
    let res = tryLoadSym(f.symId)
    assert res.status == LacksNothing
    var typeImpl = asTypeDecl(res.decl)
    if typeImpl.kind == TypeY:
      result = matchesConstraint(m, typeImpl.body, a)
    inc f
  else:
    result = matchesConstraintAux(m, f, a)

proc matchesConstraint(m: var Match; f: SymId; a: Cursor): bool =
  let res = tryLoadSym(f)
  assert res.status == LacksNothing
  var typevar = asTypevar(res.decl)
  assert typevar.kind == TypevarY
  result = matchesConstraint(m, typevar.typ, a)

proc isTypevar(s: SymId): bool =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  let typevar = asTypevar(res.decl)
  result = typevar.kind == TypevarY

proc linearMatch(m: var Match; f, a: var Cursor, containsStartTag = true) =
  var nested = 0
  while true:
    if f.kind == Symbol and isTypevar(f.symId):
      # type vars are specal:
      let fs = f.symId
      if m.inferred.contains(fs):
        # rematch?
        linearMatch(m, m.inferred[fs], a)
        if m.err: break
      elif matchesConstraint(m, fs, a):
        m.inferred[fs] = a # NOTICE: Can introduce modifiers for a type var!
        inc f
        skip a
        continue
      else:
        m.error concat(typeToString(a), " does not match constraint ", typeToString(f))
        break
    elif f.kind == a.kind:
      case f.kind
      of UnknownToken, EofToken,
          DotToken, Ident, Symbol, SymbolDef,
          StringLit, CharLit, IntLit, UIntLit, FloatLit:
        if f.uoperand != a.uoperand:
          m.error expected(f, a)
          break
      of ParLe:
        if f.uoperand != a.uoperand:
          m.error expected(f, a)
          break
        inc nested
      of ParRi:
        if nested == ord(containsStartTag): break
        dec nested
    else:
      m.error expected(f, a)
      break
    inc f
    inc a

const
  TypeModifiers = {MutT, OutT, LentT, SinkT, StaticT}

proc skipModifier*(a: Cursor): Cursor =
  result = a
  if result.kind == ParLe and result.typeKind in TypeModifiers:
    inc result

proc commonType(f, a: Cursor): Cursor =
  # XXX Refine
  result = a

proc typevarRematch(m: var Match; typeVar: SymId; f, a: Cursor) =
  let com = commonType(f, a)
  if com.kind == ParLe and com.tagId == ErrT:
    m.error concat("could not match again: ", pool.syms[typeVar], "; expected ",
      typeToString(f), " but got ", typeToString(a))
  elif matchesConstraint(m, typeVar, com):
    m.inferred[typeVar] = skipModifier(com)
  else:
    m.error concat(typeToString(a), " does not match constraint ", typeToString(typeImpl typeVar))

proc useArg(m: var Match; arg: Item) =
  var usedDeref = false
  if arg.typ.typeKind in {MutT, LentT, OutT} and m.skippedMod notin {MutT, LentT, OutT}:
    m.args.addParLe HderefX, arg.n.info
    usedDeref = true
  m.args.addSubtree arg.n
  if usedDeref:
    m.args.addParRi()

proc singleArgImpl(m: var Match; f: var Cursor; arg: Item)

proc matchSymbol(m: var Match; f: Cursor; arg: Item) =
  let a = skipModifier(arg.typ)
  let fs = f.symId
  if isTypevar(fs):
    if m.inferred.contains(fs):
      typevarRematch(m, fs, m.inferred[fs], a)
    elif matchesConstraint(m, fs, a):
      m.inferred[fs] = a
    else:
      m.error concat(typeToString(a), " does not match constraint ", typeToString(f))
  elif isObjectType(fs):
    if a.kind != Symbol:
      m.error expected(f, a)
    elif a.symId == fs:
      discard "direct match, no annotation required"
    else:
      var diff = 1
      for fparent in inheritanceChain(fs):
        if fparent == a.symId:
          m.args.addParLe OconvX, m.argInfo
          m.args.addIntLit diff, m.argInfo
          if m.flipped:
            dec m.inheritanceCosts, diff
          else:
            inc m.inheritanceCosts, diff
          inc m.opened
          diff = 0 # mark as success
          break
        inc diff
      if diff != 0:
        m.error expected(f, a)
      elif m.skippedMod == OutT:
        m.error "subtype relation not available for `out` parameters"
  elif isConcept(fs):
    m.error "'concept' is not implemented"
  else:
    # fast check that works for aliases too:
    if a.kind == Symbol and a.symId == fs:
      discard "perfect match"
    else:
      var impl = typeImpl(fs)
      if impl.kind == ParLe and impl.tagId == ErrT:
        m.error expected(f, a)
      else:
        singleArgImpl(m, impl, arg)

proc cmpTypeBits(context: ptr SemContext; f, a: Cursor): int =
  if (f.kind == IntLit or f.kind == InlineInt) and
     (a.kind == IntLit or a.kind == InlineInt):
    result = typebits(context.g.config, f.load) - typebits(context.g.config, a.load)
  else:
    result = -1

proc matchIntegralType(m: var Match; f: var Cursor; arg: Item) =
  var a = skipModifier(arg.typ)
  if f.tag == a.tag:
    inc a
  else:
    m.error expected(f, a)
    return
  let forig = f
  inc f
  let cmp = cmpTypeBits(m.context, f, a)
  if cmp == 0:
    discard "same types"
  elif cmp > 0:
    # f has more bits than a, great!
    if m.skippedMod in {MutT, OutT}:
      m.error "implicit conversion to " & typeToString(forig) & " is not mutable"
    else:
      m.args.addParLe HconvX, m.argInfo
      m.args.addSubtree forig
      inc m.intCosts
      inc m.opened
  else:
    m.error expected(f, a)
  inc f

proc expectParRi(m: var Match; f: var Cursor) =
  if f.kind == ParRi:
    inc f
  else:
    m.error "BUG: formal type not at end!"

proc singleArgImpl(m: var Match; f: var Cursor; arg: Item) =
  case f.kind
  of Symbol:
    matchSymbol m, f, arg
    inc f
  of ParLe:
    let fk = f.typeKind
    case fk
    of MutT:
      var a = arg.typ
      if a.typeKind in {MutT, OutT, LentT}:
        inc a
      else:
        m.skippedMod = f.typeKind
        m.args.addParLe HaddrX, m.argInfo
        inc m.opened
      inc f
      singleArgImpl m, f, Item(n: arg.n, typ: a)
      expectParRi m, f
    of IntT, UIntT, FloatT, CharT:
      matchIntegralType m, f, arg
      expectParRi m, f
    of BoolT, StringT:
      var a = skipModifier(arg.typ)
      if a.typeKind != fk:
        m.error expected(f, a)
      inc f
      expectParRi m, f
    of InvokeT:
      # Keep in mind that (invok GenericHead Type1 Type2 ...)
      # is tyGenericInvokation in the old Nim. A generic *instance*
      # is always a nominal type ("Symbol") like
      # `(type GeneratedName (invok MyInst ConcreteTypeA ConcreteTypeB) (object ...))`.
      # This means a Symbol can match an InvokT.
      var a = skipModifier(arg.typ)
      if a.kind == Symbol:
        var t = getTypeSection(a.symId)
        if t.typevars.typeKind == InvokeT:
          linearMatch m, f, t.typevars
        else:
          m.error expected(f, a)
      else:
        linearMatch m, f, a
      expectParRi m, f
    of RangeT:
      # for now acts the same as base type
      var a = skipModifier(arg.typ)
      inc f # skip to base type
      linearMatch m, f, a
      skip f
      skip f
      expectParRi m, f
    of ArrayT, SetT, UncheckedArrayT:
      var a = skipModifier(arg.typ)
      linearMatch m, f, a
      expectParRi m, f
    of CstringT:
      var a = skipModifier(arg.typ)
      if a.typeKind == NilT:
        discard "ok"
        inc f
      elif a.typeKind == StringT and arg.n.kind == StringLit:
        m.args.addParLe HconvX, m.argInfo
        m.args.addSubtree f
        inc m.opened
        inc m.intCosts
        inc f
      else:
        linearMatch m, f, a
      expectParRi m, f
    of PointerT:
      var a = skipModifier(arg.typ)
      case a.typeKind
      of NilT:
        discard "ok"
        inc f
      of PtrT:
        inc m.intCosts
      else:
        linearMatch m, f, a
      expectParRi m, f
    of PtrT, RefT:
      var a = skipModifier(arg.typ)
      case a.typeKind
      of NilT:
        discard "ok"
        inc f
        skip f
      else:
        linearMatch m, f, a
      expectParRi m, f
    of TypedescT:
      # do not skip modifier
      var a = arg.typ
      linearMatch m, f, a
      expectParRi m, f
    of VarargsT:
      discard "do not even advance f here"
      if m.firstVarargPosition < 0:
        m.firstVarargPosition = m.args.len
    of UntypedT, TypedT:
      # `typed` and `untyped` simply match everything:
      inc f
      expectParRi m, f
    of TupleT:
      let fOrig = f
      let aOrig = arg.typ
      var a = aOrig
      if a.typeKind != TupleT:
        m.error expected(fOrig, aOrig)
        skip f
      else:
        # skip tags:
        inc f
        inc a
        while f.kind != ParRi:
          if a.kind == ParRi:
            # len(f) > len(a)
            m.error expected(fOrig, aOrig)
          # only the type of the field is important:
          var ffld = asLocal(f).typ
          var afld = asLocal(a).typ
          linearMatch m, ffld, afld
          # skip fields:
          skip f
          skip a
        if a.kind != ParRi:
          # len(a) > len(f)
          m.error expected(fOrig, aOrig)
    of NoType, ObjectT, EnumT, HoleyEnumT, VoidT, OutT, LentT, SinkT, NilT, OrT, AndT, NotT,
        ConceptT, DistinctT, StaticT, ProcT, IterT, AutoT, SymKindT, TypeKindT, OrdinalT:
      m.error "BUG: unhandled type: " & pool.tags[f.tagId]
  else:
    m.error "BUG: " & expected(f, arg.typ)

proc singleArg(m: var Match; f: var Cursor; arg: Item) =
  singleArgImpl(m, f, arg)
  if not m.err:
    m.useArg arg # since it was a match, copy it
    while m.opened > 0:
      m.args.addParRi()
      dec m.opened

proc typematch*(m: var Match; formal: Cursor; arg: Item) =
  var f = formal
  singleArg m, f, arg

type
  TypeRelation* = enum
    NoMatch
    ConvertibleMatch
    GenericMatch
    EqualMatch

proc usesConversion*(m: Match): bool {.inline.} =
  result = abs(m.inheritanceCosts) + m.intCosts > 0

proc sigmatchLoop(m: var Match; f: var Cursor; args: openArray[Item]) =
  var i = 0
  var isVarargs = false
  while f.kind != ParRi:
    m.skippedMod = NoType

    assert f.symKind == ParamY
    let param = asLocal(f)
    var ftyp = param.typ
    # This is subtle but only this order of `i >= args.len` checks
    # is correct for all cases (varargs/too few args/too many args)
    if ftyp != "varargs":
      if i >= args.len: break
      skip f
    else:
      isVarargs = true
      if i >= args.len: break
    m.argInfo = args[i].n.info

    singleArg m, ftyp, args[i]
    if m.err: break
    inc m.pos
    inc i
  if isVarargs:
    if m.firstVarargPosition < 0:
      m.firstVarargPosition = m.args.len
    skip f


iterator typeVars(fn: SymId): SymId =
  let res = tryLoadSym(fn)
  assert res.status == LacksNothing
  var c = res.decl
  if isRoutine(c.symKind):
    inc c # skip routine tag
    for i in 1..3:
      skip c # name, export marker, pattern
    if c.substructureKind == TypevarsS:
      inc c
      while c.kind != ParRi:
        if c.symKind == TypeVarY:
          var tv = c
          inc tv
          yield tv.symId
        skip c

proc collectDefaultValues(f: var Cursor): seq[Item] =
  result = @[]
  while f.symKind == ParamY:
    let param = asLocal(f)
    if param.val.kind == DotToken: break
    result.add Item(n: param.val, typ: param.typ)
    skip f

proc sigmatch*(m: var Match; fn: FnCandidate; args: openArray[Item];
               explicitTypeVars: Cursor) =
  assert fn.kind != NoSym or fn.sym == SymId(0)
  m.tvars = initHashSet[SymId]()
  m.fn = fn
  if fn.kind in RoutineKinds:
    var e = explicitTypeVars
    for v in typeVars(fn.sym):
      m.tvars.incl v
      if e.kind != DotToken and e.kind != ParRi:
        if matchesConstraint(m, v, e):
          m.inferred[v] = e
        else:
          let res = tryLoadSym(v)
          assert res.status == LacksNothing
          var typevar = asTypevar(res.decl)
          assert typevar.kind == TypevarY
          m.error concat(typeToString(e), " does not match constraint ", typeToString(typevar.typ))
        skip e
  elif explicitTypeVars.kind != DotToken:
    # aka there are explicit type vars
    if m.tvars.len == 0:
      m.error "routine is not generic"
      return

  var f = fn.typ
  assert f == "params"
  inc f # "params"
  sigmatchLoop m, f, args

  if m.pos < args.len:
    # not all arguments where used, error:
    m.error "too many arguments"
  elif f.kind != ParRi:
    # use default values for these parameters, but this needs to be done
    # properly with generics etc. so we use a helper `args` seq and pretend
    # the programmer had written out these arguments:
    let moreArgs = collectDefaultValues(f)
    sigmatchLoop m, f, moreArgs
    if f.kind != ParRi:
      m.error "too few arguments"

  if f.kind == ParRi:
    inc f
    m.returnType = f # return type follows the parameters in the token stream

  # check all type vars have a value:
  if not m.err and fn.kind in RoutineKinds:
    for v in typeVars(fn.sym):
      let inf = m.inferred.getOrDefault(v)
      if inf == default(Cursor):
        m.error "could not infer type for " & pool.syms[v]
        break
      m.typeArgs.addSubtree inf

proc matchesBool*(m: var Match; t: Cursor) =
  var a = skipModifier(t)
  if a.typeKind == BoolT:
    inc a
    if a.kind == ParRi: return
  m.error concat("expected: 'bool' but got: ", typeToString(t))

type
  DisambiguationResult* = enum
    NobodyWins,
    FirstWins,
    SecondWins

proc cmpMatches*(a, b: Match): DisambiguationResult =
  assert not a.err
  assert not b.err
  if a.inheritanceCosts < b.inheritanceCosts:
    result = FirstWins
  elif a.inheritanceCosts > b.inheritanceCosts:
    result = SecondWins
  elif a.intCosts < b.intCosts:
    result = FirstWins
  elif a.intCosts > b.intCosts:
    result = SecondWins
  else:
    result = NobodyWins

# How to implement named parameters: In a preprocessing step
# The signature is matched against the named parameters. The
# call is then reordered to `f`'s needs. This keeps the common case fast
# where no named parameters are used at all.

