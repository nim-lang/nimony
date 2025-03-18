#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [sets, tables, assertions]

import bitabs, nifreader, nifstreams, nifcursors, lineinfos

import nimony_model, decls, programs, semdata, typeprops, xints, builtintypes, renderer, symparser
import ".." / models / tags

type
  Item* = object
    n*, typ*: Cursor
    kind*: SymKind

  FnCandidate* = object
    kind*: SymKind
    sym*: SymId
    typ*: Cursor

  MatchErrorKind* = enum
    InvalidMatch
    InvalidRematch
    ConstraintMismatch
    FormalTypeNotAtEndBug
    FormalParamsMismatch
    CallConvMismatch
    UnavailableSubtypeRelation
    NotImplementedConcept
    ImplicitConversionNotMutable
    UnhandledTypeBug
    MismatchBug
    MissingExplicitGenericParameter
    ExtraGenericParameter
    RoutineIsNotGeneric
    CouldNotInferTypeVar
    TooManyArguments
    TooFewArguments

  MatchError* = object
    info: PackedLineInfo
    #msg: string
    kind: MatchErrorKind
    typeVar: SymId
    expected, got: TypeCursor
    pos: int

  Match* = object
    inferred*: Table[SymId, Cursor]
    tvars: HashSet[SymId]
    fn*: FnCandidate
    args*, typeArgs*: TokenBuf
    err*, flipped*: bool
    concreteMatch: bool
    skippedMod: TypeKind
    argInfo: PackedLineInfo
    pos, opened: int
    inheritanceCosts, intLitCosts, intConvCosts, convCosts: int
    returnType*: Cursor
    context: ptr SemContext
    error: MatchError
    firstVarargPosition*: int
    genericConverter*, checkEmptyArg*, insertedParam*: bool

proc createMatch*(context: ptr SemContext): Match = Match(context: context, firstVarargPosition: -1)

proc concat(a: varargs[string]): string =
  result = a[0]
  for i in 1..high(a): result.add a[i]

proc error(m: var Match; k: MatchErrorKind; expected, got: Cursor) =
  if m.err: return # first error is the important one
  m.err = true
  m.error = MatchError(info: m.argInfo, kind: k,
                       expected: expected, got: got, pos: m.pos+1)
  #writeStackTrace()
  #echo "ERROR: ", msg

proc error0(m: var Match; k: MatchErrorKind) =
  if m.err: return # first error is the important one
  m.err = true
  m.error = MatchError(info: m.argInfo, kind: k, pos: m.pos+1)

proc errorTypevar(m: var Match; k: MatchErrorKind; expected, got: Cursor; typevar: SymId) =
  if m.err: return # first error is the important one
  m.err = true
  m.error = MatchError(info: m.argInfo, kind: k,
                       typeVar: typevar,
                       expected: expected, got: got, pos: m.pos+1)

proc error0Typevar(m: var Match; k: MatchErrorKind; typevar: SymId) =
  if m.err: return # first error is the important one
  m.err = true
  m.error = MatchError(info: m.argInfo, kind: k,
                       typeVar: typevar, pos: m.pos+1)

proc getErrorMsg*(m: Match): string =
  case m.error.kind
  of InvalidMatch:
    concat("expected: ", typeToString(m.error.expected), " but got: ", typeToString(m.error.got))
  of InvalidRematch:
    concat("Could not match again: ", pool.syms[m.error.typeVar], " expected ",
      typeToString(m.error.expected), " but got ", typeToString(m.error.got))
  of ConstraintMismatch:
    concat(typeToString(m.error.got), " does not match constraint ",
      typeToString(m.error.expected))
  of FormalTypeNotAtEndBug:
    "BUG: formal type not at end!"
  of FormalParamsMismatch:
    "parameter lists do not match"
  of CallConvMismatch:
    "calling conventions do not match"
  of UnavailableSubtypeRelation:
    "subtype relation not available for `out` parameters"
  of NotImplementedConcept:
    "'concept' is not implemented"
  of ImplicitConversionNotMutable:
    concat("implicit conversion to ", typeToString(m.error.expected), " is not mutable")
  of UnhandledTypeBug:
    concat("BUG: unhandled type: ", pool.tags[m.error.expected.tagId])
  of MismatchBug:
    concat("BUG: expected: ", typeToString(m.error.expected), " but got: ", typeToString(m.error.got))
  of MissingExplicitGenericParameter:
    concat("missing explicit generic parameter for ", pool.syms[m.error.typeVar])
  of ExtraGenericParameter:
    "extra generic parameter"
  of RoutineIsNotGeneric:
    "routine is not generic"
  of CouldNotInferTypeVar:
    concat("could not infer type for ", pool.syms[m.error.typeVar])
  of TooManyArguments:
    "too many arguments"
  of TooFewArguments:
    "too few arguments"

proc addErrorMsg*(dest: var string; m: Match) =
  assert m.err
  dest.add "[" & $(m.error.pos) & "] " & getErrorMsg(m)

proc addErrorMsg*(dest: var TokenBuf; m: Match) =
  assert m.err
  dest.addParLe ErrT, m.argInfo
  dest.addDotToken()
  let str = "For type " & typeToString(m.fn.typ) & " mismatch at position\n" &
    "[" & $(m.pos+1) & "] " & getErrorMsg(m)
  dest.addStrLit str
  dest.addParRi()

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
  result = asRoutine(res.decl, SkipInclBody)

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
    if aTag.typeKind == InvokeT:
      inc aTag
    if aTag.kind == Symbol:
      aTag = typeImpl(aTag.symId)
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
    inc f
    return true
  if a.kind == Symbol:
    let res = tryLoadSym(a.symId)
    assert res.status == LacksNothing
    if res.decl.symKind == TypevarY:
      var typevar = asTypevar(res.decl)
      return matchesConstraint(m, f, typevar.typ)
  if f.kind == Symbol:
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

proc cmpTypeBits(context: ptr SemContext; f, a: Cursor): int =
  if (f.kind == IntLit or f.kind == InlineInt) and
     (a.kind == IntLit or a.kind == InlineInt):
    result = typebits(context.g.config, f.load) - typebits(context.g.config, a.load)
  else:
    result = -1

proc cmpExactTypeBits(f, a: Cursor): int =
  # compares type bits without normalizing
  if (f.kind == IntLit or f.kind == InlineInt) and
     (a.kind == IntLit or a.kind == InlineInt):
    result = typebits(f.load) - typebits(a.load)
  else:
    result = -1

proc sameSymbol(a, b: SymId): bool =
  if a == b:
    return true
  # symbols might be different for instantiations from different modules,
  # consider this case by checking if the instantiation keys are equal:
  let sa = pool.syms[a]
  let sb = pool.syms[b]
  result = isInstantiation(sa) and isInstantiation(sb) and
    removeModule(sa) == removeModule(sb)

proc expectParRi(m: var Match; f: var Cursor) =
  if f.kind == ParRi:
    inc f
  else:
    m.error FormalTypeNotAtEndBug, f, f

proc procTypeMatch(m: var Match; f, a: var Cursor)

type LinearMatchFlag = enum
  ExactBits ## do not normalize bits

proc linearMatch(m: var Match; f, a: var Cursor; flags: set[LinearMatchFlag] = {}) =
  let fOrig = f
  let aOrig = a
  var nested = 0
  while true:
    if f.kind == Symbol and isTypevar(f.symId):
      # type vars are specal:
      let fs = f.symId
      if m.concreteMatch:
        # generic param is from provided argument type
        # instead of considering inference, treat as a standalone value
        if matchesConstraint(m, fs, a):
          inc f
          skip a
        else:
          m.error(ConstraintMismatch, f, a)
          break
      elif m.inferred.contains(fs):
        # rematch?
        var prev = m.inferred[fs]
        # mark that the match is to a given type from the outside context:
        m.concreteMatch = true
        linearMatch(m, prev, a, flags) # skips a
        m.concreteMatch = false # was already false because of `elif`
        inc f
      elif matchesConstraint(m, fs, a):
        m.inferred[fs] = a # NOTICE: Can introduce modifiers for a type var!
        inc f
        skip a
      else:
        m.error(ConstraintMismatch, f, a)
        break
    elif f.kind == a.kind:
      case f.kind
      of UnknownToken, EofToken,
          DotToken, Ident, SymbolDef,
          StringLit, CharLit, IntLit, UIntLit, FloatLit:
        if f.uoperand != a.uoperand:
          m.error(InvalidMatch, fOrig, aOrig)
          break
        inc f
        inc a
      of Symbol:
        if not sameSymbol(f.symId, a.symId):
          m.error(InvalidMatch, fOrig, aOrig)
          break
        inc f
        inc a
      of ParLe:
        # special cases:
        case f.typeKind
        of ProctypeT, ParamsT:
          if a.typeKind notin {ProctypeT, ParamsT}:
            m.error(InvalidMatch, fOrig, aOrig)
            break
          var a2 = a # since procTypeMatch does not skip it properly
          procTypeMatch m, f, a2
          skip a # XXX consider when a is (params)
        of IntT, UIntT, FloatT, CharT:
          if a.typeKind != f.typeKind:
            m.error(InvalidMatch, fOrig, aOrig)
            break
          inc f
          inc a
          if ExactBits in flags:
            if cmpExactTypeBits(f, a) != 0:
              m.error(InvalidMatch, fOrig, aOrig)
              break
          else:
            if cmpTypeBits(m.context, f, a) != 0:
              m.error(InvalidMatch, fOrig, aOrig)
              break
          skip f
          skip a
          expectParRi m, f
          expectParRi m, a
        else:
          if f.uoperand != a.uoperand:
            m.error(InvalidMatch, fOrig, aOrig)
            break
          inc nested
          inc f
          inc a
      of ParRi:
        assert nested > 0
        dec nested
        inc f
        inc a
    elif f.typeKind == InvokeT and a.kind == Symbol:
      # Keep in mind that (invok GenericHead Type1 Type2 ...)
      # is tyGenericInvokation in the old Nim. A generic *instance*
      # is always a nominal type ("Symbol") like
      # `(type GeneratedName (invok MyInst ConcreteTypeA ConcreteTypeB) (object ...))`.
      # This means a Symbol can match an InvokT.
      var t = getTypeSection(a.symId)
      if t.kind == TypeY and t.typevars.typeKind == InvokeT:
        linearMatch m, f, t.typevars, flags # skips f
        inc a
      else:
        m.error(InvalidMatch, fOrig, aOrig)
        break
    else:
      m.error(InvalidMatch, fOrig, aOrig)
      break
    # only match a single tree/token:
    if nested == 0:
      # successful match
      return
  # arriving here means the loop was exited early, make sure arguments are fully skipped
  f = fOrig
  a = aOrig
  skip f
  skip a

proc extractCallConv(c: var Cursor): CallConv =
  result = Fastcall
  if c.substructureKind == PragmasU:
    inc c
    while c.kind != ParRi:
      let res = callConvKind(c)
      if res != NoCallConv:
        result = res
      skip c
    inc c
  elif c.kind == DotToken:
    inc c
  else:
    raiseAssert "BUG: No pragmas found"

proc procTypeMatch(m: var Match; f, a: var Cursor) =
  if f.typeKind == ProctypeT:
    inc f
    for i in 1..4: skip f
  if a.typeKind == ProctypeT:
    inc a
    for i in 1..4: skip a
  var hasParams = 0
  if f.typeKind == ParamsT:
    inc f
    if f.kind != ParRi: inc hasParams
  if a.typeKind == ParamsT:
    inc a
    if a.kind != ParRi: inc hasParams, 2
  if hasParams == 3:
    while f.kind != ParRi and a.kind != ParRi:
      var fParam = takeLocal(f, SkipFinalParRi)
      var aParam = takeLocal(a, SkipFinalParRi)
      assert fParam.kind == ParamY
      assert aParam.kind == ParamY
      linearMatch m, fParam.typ, aParam.typ
    if f.kind == ParRi:
      if a.kind == ParRi:
        discard "ok"
      else:
        m.error FormalParamsMismatch, f, a
        skipUntilEnd a
    else:
      m.error FormalParamsMismatch, f, a
      skipUntilEnd f
      skipUntilEnd a
  elif hasParams == 2:
    m.error FormalParamsMismatch, f, a
    skipUntilEnd a
  elif hasParams == 1:
    m.error FormalParamsMismatch, f, a
    skipUntilEnd f

  # also correct for the DotToken case:
  inc f
  inc a

  # match return types:
  let fret = typeKind f
  let aret = typeKind a
  if fret == aret and fret == VoidT:
    skip f
    skip a
  else:
    linearMatch m, f, a
  # match calling conventions:
  let fcc = extractCallConv(f)
  let acc = extractCallConv(a)
  if fcc != acc:
    m.error CallConvMismatch, f, a
  # XXX consider when f or a is (params):
  skip f # effects
  #skip a # effects
  skip f # body
  #skip a # body
  expectParRi m, f
  #expectParRi m, a

proc commonType(f, a: Cursor): Cursor =
  # XXX Refine
  result = a

proc typevarRematch(m: var Match; typeVar: SymId; f, a: Cursor) {.used.} =
  # now unused, maybe bring back error message somehow
  let com = commonType(f, a)
  if com.kind == ParLe and com.tagId == ErrT:
    m.errorTypevar InvalidRematch, f, a, typeVar
  elif matchesConstraint(m, typeVar, com):
    m.inferred[typeVar] = skipModifier(com)
  else:
    m.error ConstraintMismatch, typeImpl(typeVar), a

proc useArg(m: var Match; arg: Item) =
  m.args.addSubtree arg.n

proc singleArgImpl(m: var Match; f: var Cursor; arg: Item)

proc matchSymbol(m: var Match; f: Cursor; arg: Item) =
  let a = skipModifier(arg.typ)
  let fs = f.symId
  if isTypevar(fs):
    if m.concreteMatch:
      # generic param is from provided argument type
      # instead of considering inference, treat as a standalone value
      if not matchesConstraint(m, fs, a):
        m.error ConstraintMismatch, f, a
    elif m.inferred.contains(fs):
      # used to call typevarRematch
      var prev = m.inferred[fs]
      # mark that the match is to a given type from the outside context:
      m.concreteMatch = true
      singleArgImpl(m, prev, arg)
      m.concreteMatch = false # was already false because of `elif`
    elif matchesConstraint(m, fs, a):
      m.inferred[fs] = a
    else:
      m.error ConstraintMismatch, f, a
  elif isObjectType(fs):
    if a.kind != Symbol:
      m.error InvalidMatch, f, a
    elif sameSymbol(fs, a.symId):
      discard "direct match, no annotation required"
    else:
      var diff = 1
      for fparent in inheritanceChain(fs):
        if sameSymbol(fparent, a.symId):
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
        m.error InvalidMatch, f, a
      elif m.skippedMod == OutT:
        m.error UnavailableSubtypeRelation, f, a
  elif isConcept(fs):
    m.error NotImplementedConcept, f, a
  else:
    # fast check that works for aliases too:
    if a.kind == Symbol and sameSymbol(a.symId, fs):
      discard "perfect match"
    else:
      var impl = typeImpl(fs)
      if impl.kind == ParLe and impl.tagId == ErrT:
        m.error InvalidMatch, f, a
      else:
        if impl.typeKind in {EnumT, HoleyEnumT}:
          #echo "ENUM: ", f.toString(false), " ", arg.n.toString(false)
          m.error InvalidMatch, f, a
        else:
          singleArgImpl(m, impl, arg)

proc checkIntLitRange(context: ptr SemContext; f: Cursor; intLit: Cursor): bool =
  if f.typeKind == FloatT:
    result = true
  else:
    let i = createXint(pool.integers[intLit.intId])
    result = i >= firstOrd(context[], f) and i <= lastOrd(context[], f)

proc matchIntegralType(m: var Match; f: var Cursor; arg: Item) =
  var a = skipModifier(arg.typ)
  let isIntLit = f.typeKind != CharT and
    arg.n.kind == IntLit and sameTrees(a, m.context.types.intType)
  let sameKind = f.tag == a.tag
  if sameKind or isIntLit:
    inc a
  else:
    m.error InvalidMatch, f, a
    return
  let forig = f
  inc f
  let cmp = cmpTypeBits(m.context, f, a)
  if cmp == 0 and sameKind:
    discard "same types"
  elif cmp > 0 or (isIntLit and checkIntLitRange(m.context, forig, arg.n)):
    # f has more bits than a, great!
    if m.skippedMod in {MutT, OutT}:
      m.error ImplicitConversionNotMutable, forig, forig
    else:
      m.args.addParLe HconvX, m.argInfo
      m.args.addSubtree forig
      if isIntLit:
        if f.typeKind == FloatT:
          inc m.convCosts
        else:
          inc m.intLitCosts
      else:
        # sameKind is true
        inc m.intConvCosts
      inc m.opened
  else:
    m.error InvalidMatch, f, a
  inc f
  while f.pragmaKind in {ImportcP, ImportcppP}:
    skip f

proc matchArrayType(m: var Match; f: var Cursor; a: var Cursor) =
  if a.typeKind == ArrayT:
    var a1 = a
    var f1 = f
    inc a1
    inc f1
    skip a1
    skip f1
    let fLen = lengthOrd(m.context[], f1)
    let aLen = lengthOrd(m.context[], a1)
    if fLen.isNaN or aLen.isNaN:
      # match typevars
      linearMatch m, f, a
    elif fLen == aLen:
      inc f
      inc a
      linearMatch m, f, a
      skip f
      expectParRi m, f
    else:
      m.error InvalidMatch, f, a
  else:
    m.error InvalidMatch, f, a

proc isSomeSeqType*(a: Cursor, elemType: var Cursor): bool =
  # check that `a` is either an instantiation of seq or an invocation to it
  result = false
  var a = a
  var i = 0
  while a.kind == Symbol:
    let decl = getTypeSection(a.symId)
    if decl.kind == TypeY:
      if decl.body.kind == Symbol:
        a = decl.body
      else:
        a = decl.typevars
    else:
      return false
    inc i
    if i == 20: break
  if a.typeKind == InvokeT:
    inc a # tag
    result = a.kind == Symbol and pool.syms[a.symId] == "seq.0." & SystemModuleSuffix
    if result:
      inc a
      elemType = a

proc isSomeSeqType*(a: Cursor): bool {.inline.} =
  var dummy = default(Cursor)
  result = isSomeSeqType(a, dummy)

proc singleArgImpl(m: var Match; f: var Cursor; arg: Item) =
  case f.kind
  of Symbol:
    matchSymbol m, f, arg
    inc f
  of ParLe:
    let fk = f.typeKind
    case fk
    of MutT, OutT, SinkT, LentT:
      var a = arg.typ
      if a.typeKind in {MutT, OutT, SinkT, LentT}:
        inc a
      else:
        m.skippedMod = f.typeKind
      inc f
      singleArgImpl m, f, Item(n: arg.n, typ: a)
      expectParRi m, f
    of IntT, UIntT, FloatT, CharT:
      matchIntegralType m, f, arg
      expectParRi m, f
    of BoolT:
      var a = skipModifier(arg.typ)
      if a.typeKind != fk:
        m.error InvalidMatch, f, a
      inc f
      expectParRi m, f
    of InvokeT:
      # handled in linearMatch
      # XXX except for inheritance
      var a = skipModifier(arg.typ)
      linearMatch m, f, a
    of RangetypeT:
      # for now acts the same as base type
      var a = skipModifier(arg.typ)
      inc f # skip to base type
      linearMatch m, f, a
      skip f
      skip f
      expectParRi m, f
    of ArrayT:
      var a = skipModifier(arg.typ)
      matchArrayType m, f, a
    of SetT, UarrayT:
      var a = skipModifier(arg.typ)
      linearMatch m, f, a
    of CstringT:
      var a = skipModifier(arg.typ)
      if a.typeKind == NiltT:
        discard "ok"
        inc f
        expectParRi m, f
      elif isStringType(a) and arg.n.kind == StringLit:
        m.args.addParLe HconvX, m.argInfo
        m.args.addSubtree f
        inc m.opened
        inc m.convCosts
        inc f
        expectParRi m, f
      else:
        linearMatch m, f, a
    of PointerT:
      var a = skipModifier(arg.typ)
      case a.typeKind
      of NiltT:
        discard "ok"
        inc f
        expectParRi m, f
      of PtrT, CstringT:
        m.args.addParLe HconvX, m.argInfo
        m.args.addSubtree f
        inc m.opened
        inc m.convCosts
        inc f
        expectParRi m, f
      else:
        linearMatch m, f, a
    of PtrT, RefT:
      var a = skipModifier(arg.typ)
      case a.typeKind
      of NiltT:
        discard "ok"
        inc f
        skip f
        expectParRi m, f
      else:
        linearMatch m, f, a
    of TypedescT:
      # do not skip modifier
      var a = arg.typ
      linearMatch m, f, a, {ExactBits}
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
      let aOrig = skipModifier(arg.typ)
      var a = aOrig
      if a.typeKind != TupleT:
        m.error InvalidMatch, fOrig, aOrig
        skip f
      else:
        # skip tags:
        inc f
        inc a
        while f.kind != ParRi:
          if a.kind == ParRi:
            # len(f) > len(a)
            m.error InvalidMatch, fOrig, aOrig
          # only the type of the field is important:
          var ffld = getTupleFieldType(f)
          var afld = getTupleFieldType(a)
          linearMatch m, ffld, afld
          # skip fields:
          skip f
          skip a
        if a.kind != ParRi:
          # len(a) > len(f)
          m.error InvalidMatch, fOrig, aOrig
    of ProctypeT, ParamsT:
      var a = skipModifier(arg.typ)
      case a.typeKind
      of NiltT:
        discard "ok"
        skip f
      else:
        procTypeMatch m, f, a
    of NoType, ErrT, ObjectT, EnumT, HoleyEnumT, VoidT, NiltT, OrT, AndT, NotT,
        ConceptT, DistinctT, StaticT, IteratorT, ItertypeT, AutoT, SymKindT, TypeKindT, OrdinalT:
      m.error UnhandledTypeBug, f, f
  else:
    m.error MismatchBug, f, arg.typ

proc isEmptyLiteral*(n: Cursor): bool =
  result = n.exprKind in {AconstrX, SetConstrX}
  if result:
    var n = n
    inc n # tag
    skip n # type
    result = n.kind == ParRi

proc isEmptyCall*(n: Cursor): bool =
  # input needs to be semchecked, possibly in AllowEmpty context
  if n.exprKind notin CallKinds:
    return false
  var n = n
  inc n
  # overload of `@` with empty array param:
  result = n.kind == Symbol and pool.syms[n.symId] == "@.1." & SystemModuleSuffix
  inc n
  if not isEmptyLiteral(n):
    return false
  skip n
  if n.kind != ParRi:
    return false

proc isEmptyContainer*(n: Cursor): bool =
  result = isEmptyLiteral(n) or isEmptyCall(n)

proc addEmptyRangeType(buf: var TokenBuf; c: ptr SemContext; info: PackedLineInfo) =
  buf.addParLe(RangetypeT, info)
  buf.addSubtree c.types.intType
  buf.addIntLit(0, info)
  buf.addIntLit(-1, info)
  buf.addParRi()

proc matchEmptyContainer(m: var Match; f: var Cursor; arg: Item) =
  if (arg.n.exprKind == AconstrX and f.typeKind == ArrayT) or
      (arg.n.exprKind == SetConstrX and f.typeKind == SetT):
    # could also handle case where `f` is a typevar
    if arg.n.exprKind == AconstrX:
      # need to match index type
      var fIndex = f
      inc fIndex # skip tag
      skip fIndex # skip element type
      let fLen = lengthOrd(m.context[], fIndex)
      if fLen.isNaN:
        # create index type to match to
        var buf = createTokenBuf(8)
        let info = arg.n.info
        addEmptyRangeType(buf, m.context, info)
        # hoist it in case it gets inferred:
        var aIndex = typeToCursor(m.context[], buf, 0)
        linearMatch(m, fIndex, aIndex)
      elif fLen != zero():
        m.error(InvalidMatch, f, arg.typ)
    inc m.inheritanceCosts
    if not m.err:
      if containsGenericParams(f): # maybe restrict to params of this routine
        # element type needs to be instantiated:
        m.checkEmptyArg = true
      m.args.add arg.n.load # copy tag
      m.args.takeTree f
      m.args.addParRi()
  else:
    var elemType = default(Cursor)
    if (arg.n.exprKind in CallKinds and isSomeSeqType(f, elemType)):
      inc m.inheritanceCosts
      if not m.err:
        # call to `@` needs to be instantiated/template expanded,
        # also the element type needs to be instantiated if generic:
        m.checkEmptyArg = true
        # keep the call to `@` but give the array constructor the element type:
        var call = arg.n
        m.args.add call.load # copy call tag
        inc call
        m.args.add call.load # copy symbol
        inc call
        assert call.exprKind == AconstrX
        m.args.add call.load # copy array tag
        inc call
        # build our own array type:
        m.args.addParLe(ArrayT, call.info)
        m.args.addSubtree elemType
        addEmptyRangeType(m.args, m.context, call.info)
        m.args.addParRi()
        skip call
        takeParRi m.args, call # array constructor
        takeParRi m.args, call # call
    else:
      # match against `auto`, untyped/varargs should still match
      singleArgImpl(m, f, arg)

proc singleArg(m: var Match; f: var Cursor; arg: Item) =
  if arg.typ.typeKind == AutoT and isEmptyContainer(arg.n):
    matchEmptyContainer(m, f, arg)
    return
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
    IntLitMatch
    IntConvMatch
    ConvertibleMatch
    SubtypeMatch
    GenericMatch
    EqualMatch

proc usesConversion*(m: Match): bool {.inline.} =
  result = abs(m.inheritanceCosts) + m.intLitCosts + m.intConvCosts + m.convCosts > 0

proc classifyMatch*(m: Match): TypeRelation {.inline.} =
  if m.err:
    return NoMatch
  if m.convCosts != 0:
    return ConvertibleMatch
  if m.intConvCosts != 0:
    return IntConvMatch
  if m.intLitCosts != 0:
    return IntLitMatch
  if m.inheritanceCosts != 0:
    return SubtypeMatch
  if m.inferred.len != 0:
    # maybe a better way to track this
    return GenericMatch
  result = EqualMatch

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
    if ftyp.tagEnum != VarargsTagId:
      if i >= args.len: break
      skip f
    else:
      isVarargs = true
      if i >= args.len: break
    if args[i].n.kind == DotToken:
      # default parameter
      assert param.val.kind != DotToken
      assert not isVarargs
      m.args.add dotToken(param.val.info)
    else:
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
    if c.substructureKind == TypevarsU:
      inc c
      while c.kind != ParRi:
        if c.symKind == TypeVarY:
          var tv = c
          inc tv
          yield tv.symId
        skip c

proc collectDefaultValues(m: var Match; f: Cursor): seq[Item] =
  var f = f
  result = @[]
  while f.symKind == ParamY:
    let param = asLocal(f)
    if param.val.kind == DotToken: break
    m.insertedParam = true
    # add dot token
    result.add Item(n: emptyNode(m.context[]), typ: m.context.types.autoType)
    skip f

proc matchTypevars*(m: var Match; fn: FnCandidate; explicitTypeVars: Cursor) =
  m.tvars = initHashSet[SymId]()
  if fn.kind in RoutineKinds:
    var e = explicitTypeVars
    for v in typeVars(fn.sym):
      m.tvars.incl v
      if e.kind == DotToken: discard
      elif e.kind == ParRi:
        m.error0Typevar MissingExplicitGenericParameter, v
        break
      else:
        if matchesConstraint(m, v, e):
          m.inferred[v] = e
        else:
          let res = tryLoadSym(v)
          assert res.status == LacksNothing
          var typevar = asTypevar(res.decl)
          assert typevar.kind == TypevarY
          m.error ConstraintMismatch, typevar.typ, e
        skip e
    if e.kind != DotToken and e.kind != ParRi:
      m.error0 ExtraGenericParameter
  elif explicitTypeVars.kind != DotToken:
    # aka there are explicit type vars
    if m.tvars.len == 0:
      m.error0 RoutineIsNotGeneric
      return

proc sigmatch*(m: var Match; fn: FnCandidate; args: openArray[Item];
               explicitTypeVars: Cursor) =
  assert fn.kind != NoSym or fn.sym == SymId(0)
  m.fn = fn
  matchTypevars m, fn, explicitTypeVars

  var f = fn.typ
  assert f.isParamsTag
  inc f # "params"
  sigmatchLoop m, f, args

  if m.pos < args.len:
    # not all arguments where used, error:
    m.error0 TooManyArguments
  elif f.kind != ParRi:
    # use default values for these parameters
    let moreArgs = collectDefaultValues(m, f)
    sigmatchLoop m, f, moreArgs
    if f.kind != ParRi:
      m.error0 TooFewArguments

  if f.kind == ParRi:
    inc f
    m.returnType = f # return type follows the parameters in the token stream

proc buildTypeArgs*(m: var Match) =
  # check all type vars have a value:
  if not m.err and m.fn.kind in RoutineKinds:
    for v in typeVars(m.fn.sym):
      let inf = m.inferred.getOrDefault(v)
      if inf == default(Cursor):
        m.error0Typevar CouldNotInferTypeVar, v
        break
      m.typeArgs.addSubtree inf

type
  DisambiguationResult* = enum
    NobodyWins,
    FirstWins,
    SecondWins

proc mutualGenericMatch(a, b: Match): DisambiguationResult =
  # same goal as `checkGeneric` in old compiler
  result = NobodyWins
  let c = a.context
  var aParams = a.fn.typ
  var bParams = b.fn.typ
  assert aParams.typeKind == ParamsT
  assert bParams.typeKind == ParamsT
  inc aParams
  inc bParams
  while aParams.kind != ParRi and bParams.kind != ParRi:
    let aParam = takeLocal(aParams, SkipFinalParRi)
    let bParam = takeLocal(bParams, SkipFinalParRi)
    var aFormal = aParam.typ
    var bFormal = bParam.typ
    var ma = createMatch(c)
    singleArg ma, aFormal, Item(n: emptyNode(c[]), typ: bParam.typ)
    var mb = createMatch(c)
    singleArg mb, bFormal, Item(n: emptyNode(c[]), typ: aParam.typ)
    let aMatch = classifyMatch(ma)
    let bMatch = classifyMatch(mb)
    if aMatch == GenericMatch and bMatch == NoMatch:
      # b is more specific
      if result == FirstWins: return NobodyWins
      result = SecondWins
    if bMatch == GenericMatch and aMatch == NoMatch:
      # a is more specific
      if result == SecondWins: return NobodyWins
      result = FirstWins

proc cmpMatches*(a, b: Match): DisambiguationResult =
  assert not a.err
  assert not b.err
  if a.convCosts < b.convCosts:
    result = FirstWins
  elif a.convCosts > b.convCosts:
    result = SecondWins
  elif a.intConvCosts < b.intConvCosts:
    result = FirstWins
  elif a.intConvCosts > b.intConvCosts:
    result = SecondWins
  elif a.intLitCosts < b.intLitCosts:
    result = FirstWins
  elif a.intLitCosts > b.intLitCosts:
    result = SecondWins
  elif a.inheritanceCosts < b.inheritanceCosts:
    result = FirstWins
  elif a.inheritanceCosts > b.inheritanceCosts:
    result = SecondWins
  else:
    let diff = a.inferred.len - b.inferred.len
    if diff < 0:
      result = FirstWins
    elif diff > 0:
      result = SecondWins
    else:
      if a.fn.typ.typeKind == ParamsT and b.fn.typ.typeKind == ParamsT:
        result = mutualGenericMatch(a, b)
      else:
        result = NobodyWins

# How to implement named parameters: In a preprocessing step
# The signature is matched against the named parameters. The
# call is then reordered to `f`'s needs. This keeps the common case fast
# where no named parameters are used at all.

