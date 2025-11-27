#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [sets, tables, assertions]

import bitabs, nifreader, nifstreams, nifcursors, lineinfos

import nimony_model, decls, programs, semdata, typeprops, xints, builtintypes, renderer, symparser, asthelpers
import ".." / models / tags

type
  Item* = object
    n*, typ*: Cursor
    kind*: SymKind

  CallArg* = object
    n*, typ*: Cursor
    orig*: Cursor ## original tree before semchecking, used for untyped args

  FnCandidate* = object
    kind*: SymKind
    sym*: SymId
    typ*: Cursor
    fromConcept*: bool

  MatchErrorKind* = enum
    InvalidMatch
    InvalidRematch
    ConstraintMismatch
    FormalTypeNotAtEndBug
    FormalParamsMismatch
    CallConvMismatch
    RaisesMismatch
    ClosureMismatch
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
    NameNotFound
    ParamAlreadyGiven

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
    hasError: bool # mark that error message was set
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
  m.err = true
  if m.hasError: return # first error is the important one
  m.hasError = true
  m.error = MatchError(info: m.argInfo, kind: k,
                       expected: expected, got: got, pos: m.pos+1)
  #writeStackTrace()
  #echo "ERROR: ", typeToString(m.error.expected)

proc error0(m: var Match; k: MatchErrorKind) =
  m.err = true
  if m.hasError: return # first error is the important one
  m.hasError = true
  m.error = MatchError(info: m.argInfo, kind: k, pos: m.pos+1)

proc errorTypevar(m: var Match; k: MatchErrorKind; expected, got: Cursor; typevar: SymId) =
  m.err = true
  if m.hasError: return # first error is the important one
  m.hasError = true
  m.error = MatchError(info: m.argInfo, kind: k,
                       typeVar: typevar,
                       expected: expected, got: got, pos: m.pos+1)

proc error0Typevar(m: var Match; k: MatchErrorKind; typevar: SymId) =
  m.err = true
  if m.hasError: return # first error is the important one
  m.hasError = true
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
  of RaisesMismatch:
    "`.raises` mismatch"
  of ClosureMismatch:
    "`.closure` mismatch"
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
  of NameNotFound:
    "named argument not found"
  of ParamAlreadyGiven:
    "parameter already given"

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

proc getProcDecl*(s: SymId): Routine =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  result = asRoutine(res.decl, SkipInclBody)

proc isObjectType*(s: SymId): bool =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  var n = res.decl
  if n.stmtKind == TypeS:
    inc n # skip ParLe
    for i in 1..4:
      skip(n) # name, export marker, pragmas, generic parameter
    if n.typeKind in {RefT, PtrT}:
      inc n
    result = n.typeKind == ObjectT
  else:
    result = false

proc isObjectType(n: Cursor): bool =
  var n = n
  if n.typeKind == InvokeT:
    inc n
  if n.kind == Symbol:
    result = isObjectType(n.symId)
  else:
    result = false

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

type LinearMatchFlag = enum
  ExactBits ## do not normalize bits

proc linearMatch(m: var Match; f, a: var Cursor; flags: set[LinearMatchFlag] = {})

proc tryLinearMatch(m: var Match; f, a: var Cursor; flags: set[LinearMatchFlag] = {}): bool {.inline.} =
  let oldErr = m.err
  let oldHasError = m.hasError
  m.err = false
  m.hasError = false
  linearMatch m, f, a, flags
  result = not m.err
  m.err = oldErr
  m.hasError = oldHasError

proc matchesConstraint*(m: var Match; f: var Cursor; a: Cursor): bool

proc matchSymbolConstraint(m: var Match; f: var Cursor; a: Cursor): bool =
  result = false
  let fOrig = f
  let fs = f.symId
  inc f
  let res = tryLoadSym(fs)
  assert res.status == LacksNothing
  var typeImpl = asTypeDecl(res.decl)
  # check if symbol has typeclass behavior:
  if typeImpl.kind == TypeY:
    if typeImpl.body.typeKind == ConceptT:
      return matchesConstraint(m, typeImpl.body, a)
    if typeImpl.typevars.substructureKind == TypevarsU:
      # matching generic base symbol, acts as typeclass
      # XXX does not consider inheritance
      var inst = a
      if a.kind == Symbol:
        if fs == a.symId:
          return true
        let resa = tryLoadSym(a.symId)
        assert resa.status == LacksNothing
        var aDecl = asTypeDecl(resa.decl)
        if aDecl.typevars.typeKind == InvokeT:
          inst = aDecl.typevars
      if inst.typeKind == InvokeT:
        inc inst
        assert inst.kind == Symbol
        if fs == inst.symId:
          return true
  # otherwise, match symbol as a regular type (includes typevar case):
  # XXX typevars inferred to have typevar values will try to match individual constraints here
  f = fOrig
  var a = a
  # XXX this means conversions are not allowed, i.e. T: cstring cannot match "abc"
  result = tryLinearMatch(m, f, a)

proc matchTypeConstraint(m: var Match; f: var Cursor; a: Cursor): bool =
  result = false
  case f.typeKind
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
    case a.typeKind
    of OrdinalT:
      result = true
    of TypeKindT:
      var aTag = a
      inc aTag
      result = isOrdinalTypeKind(aTag.typeKind)
    else:
      result = isOrdinalType(a)
    skip f
  else:
    # match as a regular type:
    var a = a
    # XXX this means conversions are not allowed, i.e. T: cstring cannot match "abc"
    result = tryLinearMatch(m, f, a)

proc matchSingleConstraint(m: var Match; f: var Cursor; a: Cursor): bool {.inline.} =
  if f.kind == Symbol:
    result = matchSymbolConstraint(m, f, a)
  else:
    result = matchTypeConstraint(m, f, a)

proc matchConstraintSplitAnd(m: var Match; f: var Cursor; a: Cursor): bool =
  if a.typeKind == AndT:
    # an argument with `and` type is not understood by typeclasses
    # consider at least one branch the `and` type can take enough to match the constraint
    # since we need to consider every branch, this has to be done last
    result = false
    var a = a
    inc a
    var nested = 1
    while nested != 0:
      if a.typeKind == AndT:
        inc a
        inc nested
      elif a.kind == ParRi:
        inc a
        dec nested
      else:
        var f2 = f
        # XXX `a` can be an `or` type again here which will not match properly
        # a fix might be to split `a` into sum of products form before matching, i.e.
        # (A or B) and (C or D) becomes (A and C) or (A and D) or (B and C) or (B and D)
        # same for `not`
        result = matchSingleConstraint(m, f2, a)
        if result: break
        skip a
    skip f
  else:
    result = matchSingleConstraint(m, f, a)

proc matchBooleanConstraint(m: var Match; f: var Cursor; a: Cursor): bool =
  result = false
  case f.typeKind
  of AndT:
    inc f
    result = true
    while f.kind != ParRi:
      var f2 = f
      if not matchBooleanConstraint(m, f2, a):
        result = false
        break
      skip f
    skipToEnd f
  of OrT:
    inc f
    while f.kind != ParRi:
      var f2 = f
      if matchBooleanConstraint(m, f2, a):
        result = true
        break
      skip f
    skipToEnd f
  of NotT:
    # XXX handle not/not case somehow
    inc f
    result = not matchBooleanConstraint(m, f, a)
    skipParRi f
  else:
    # standalone typeclass
    result = matchConstraintSplitAnd(m, f, a)

proc matchConstraintSplitOr(m: var Match; f: var Cursor; a: Cursor): bool =
  if a.typeKind == OrT:
    # an argument with `or` type is not understood by typeclasses
    # each possible branch the `or` type can take needs to match the constraint independently,
    # so we split it before matching any typeclasses
    result = false
    var a = a
    inc a
    var nested = 1
    while nested != 0:
      if a.typeKind == OrT:
        inc a
        inc nested
      elif a.kind == ParRi:
        inc a
        dec nested
      else:
        var f2 = f
        result = matchBooleanConstraint(m, f2, a)
        if not result: break
        skip a
    skip f
  else:
    result = matchBooleanConstraint(m, f, a)

proc matchesConstraintAux(m: var Match; f: var Cursor; a: Cursor): bool =
  if a.typeKind in {OrT, AndT, NotT}:
    # typeclass matching typeclass, might need to be reordered to match properly:
    if isSumOfProducts(a):
      result = matchConstraintSplitOr(m, f, a)
    else:
      var reorderBuf = createTokenBuf(32)
      var a = a
      reorderSumOfProducts(reorderBuf, a)
      var reordered = beginRead(reorderBuf)
      result = matchConstraintSplitOr(m, f, reordered)
  else:
    result = matchBooleanConstraint(m, f, a)

proc matchesConstraint*(m: var Match; f: var Cursor; a: Cursor): bool =
  result = false
  if f.kind == DotToken:
    inc f
    return a.typeKind != AutoT
  if a.kind == Symbol:
    let res = tryLoadSym(a.symId)
    assert res.status == LacksNothing
    if res.decl.symKind == TypevarY:
      var typevar = asTypevar(res.decl)
      return matchesConstraint(m, f, typevar.typ)
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

proc expectPtrParRi(m: var Match; f: var Cursor) =
  if f.kind != ParRi: skip f # skip nil/not nil annotation
  if f.kind == ParRi:
    inc f
  else:
    m.error FormalTypeNotAtEndBug, f, f

proc procTypeMatch(m: var Match; f, a: var Cursor)

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
        of RoutineTypes:
          if a.typeKind notin RoutineTypes:
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
          if f.kind != ParRi:
            # importc part
            while f.pragmaKind in {ImportcP, ImportcppP, HeaderP}:
              skip f
          if a.kind != ParRi:
            # importc part
            while a.pragmaKind in {ImportcP, ImportcppP, HeaderP}:
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

type
  ProcProperties* = object
    cc*: CallConv
    usesRaises*: bool
    usesClosure*: bool

proc extractProcProps*(c: var Cursor): ProcProperties =
  result = ProcProperties(cc: Nimcall, usesRaises: false, usesClosure: false)
  if c.substructureKind == PragmasU:
    inc c
    while c.kind != ParRi:
      let res = callConvKind(c)
      if res != NoCallConv:
        result.cc = res
      elif c.pragmaKind == RaisesP:
        result.usesRaises = true
      elif c.pragmaKind == ClosureP:
        result.usesClosure = true
      skip c
    inc c
  elif c.kind == DotToken:
    inc c
  else:
    bug "No pragmas found"

proc procTypeMatch(m: var Match; f, a: var Cursor) =
  assert f.typeKind in RoutineTypes
  inc f
  for i in 1..4: skip f
  assert a.typeKind in RoutineTypes
  inc a
  for i in 1..4: skip a
  var hasParams = 0
  if f.substructureKind == ParamsU:
    inc f
    if f.kind != ParRi: inc hasParams
  if a.substructureKind == ParamsU:
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
  let fcc = extractProcProps(f)
  let acc = extractProcProps(a)
  if fcc.cc != acc.cc:
    m.error CallConvMismatch, f, a
  elif fcc.usesRaises != acc.usesRaises:
    m.error RaisesMismatch, f, a
  elif fcc.usesClosure != acc.usesClosure:
    m.error ClosureMismatch, f, a
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

proc useArg(m: var Match; arg: CallArg; f: Cursor) =
  if f.typeKind == UntypedT and not cursorIsNil(arg.orig):
    # pass arg tree before semchecking to untyped args:
    m.args.addSubtree arg.orig
  else:
    m.args.addSubtree arg.n

proc singleArgImpl(m: var Match; f: var Cursor; arg: CallArg)

proc matchObjectInheritance(m: var Match; f, a: Cursor; fsym, asym: SymId; ptrKind: TypeKind) =
  let fbase = skipTypeInstSym(fsym)
  var diff = 1
  var objbody = objtypeImpl(asym)
  while true:
    let od = asObjectDecl(objbody)
    if od.kind != ObjectT:
      m.error InvalidMatch, f, a
      return
    var parent = od.parentType
    if parent.typeKind in {RefT, PtrT}:
      inc parent

    var psym = SymId(0)
    var pbase = SymId(0)
    if parent.typeKind == InvokeT:
      var base = parent
      inc base
      psym = base.symId
      pbase = psym
    elif parent.kind == Symbol:
      psym = parent.symId
      pbase = skipTypeInstSym(psym)
    else:
      break

    if sameSymbol(fbase, pbase):
      if f.typeKind == InvokeT:
        # infer generic params
        # XXX might need to use something like `bindSubsInvokeArgs` here
        # if `parent` contains generic parameters of the object type
        var f2 = f
        var p2 = parent
        linearMatch m, f2, p2

      m.args.addParLe BaseobjX, m.argInfo
      if m.flipped:
        if ptrKind != NoType: m.args.addParLe(ptrKind, a.info)
        m.args.addSubtree a
        if ptrKind != NoType: m.args.addParRi()
        m.args.addIntLit -diff, m.argInfo
        dec m.inheritanceCosts, diff
      else:
        if ptrKind != NoType: m.args.addParLe(ptrKind, f.info)
        m.args.addSubtree f
        if containsGenericParams(f):
          # needs to be instantiated, reuse genericConverter
          m.genericConverter = true
        if ptrKind != NoType: m.args.addParRi()
        m.args.addIntLit diff, m.argInfo
        inc m.inheritanceCosts, diff
      inc m.opened
      diff = 0 # mark as success
      break
    inc diff
    objbody = objtypeImpl(psym)
  if diff != 0:
    m.error InvalidMatch, f, a
  elif m.skippedMod == OutT:
    m.error UnavailableSubtypeRelation, f, a

proc matchObjectTypes(m: var Match; f: var Cursor, a: Cursor; ptrKind: TypeKind) =
  if f.kind == Symbol:
    # consider object sym as instantiated, can only match another object sym
    # (generic base sym case handled in `matchesConstraint`)
    if a.kind != Symbol:
      m.error InvalidMatch, f, a
    elif sameSymbol(f.symId, a.symId):
      discard "direct match, no annotation required"
    elif not isObjectType(a.symId):
      m.error InvalidMatch, f, a
    else:
      matchObjectInheritance m, f, a, f.symId, a.symId, ptrKind
    inc f
  elif f.typeKind == InvokeT:
    # check if the types are compatible first before checking for inheritance
    var aInvoke = a
    if a.kind == Symbol:
      let ad = getTypeSection(a.symId)
      if ad.kind == TypeY and ad.typevars.typeKind == InvokeT:
        aInvoke = ad.typevars
    var fBase = f
    inc fBase
    if aInvoke.typeKind == InvokeT:
      var aBase = aInvoke
      inc aBase
      if sameSymbol(fBase.symId, aBase.symId):
        linearMatch m, f, aInvoke
      else:
        let fsym = fBase.symId
        let asym = if a.kind == Symbol: a.symId else: aBase.symId
        matchObjectInheritance m, f, a, fsym, asym, ptrKind
        skip f
    else:
      # already checked that this is an object type
      assert a.kind == Symbol
      let fsym = fBase.symId
      let asym = a.symId
      matchObjectInheritance m, f, a, fsym, asym, ptrKind
      skip f

proc matchSymbol(m: var Match; f: Cursor; arg: CallArg) =
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
    var f = f
    matchObjectTypes m, f, a, NoType
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

proc skipExpr*(n: Cursor): Cursor =
  result = n
  while result.exprKind in {ExprX, ParX}:
    inc result
    var next = result
    while next.kind != ParRi:
      result = next
      skip next

proc matchIntegralType(m: var Match; f: var Cursor; arg: CallArg) =
  var a = skipModifier(arg.typ)
  if a.typeKind == RangetypeT:
    inc a # skip to base type
  let ex = skipExpr(arg.n)
  let isIntLit = f.typeKind != CharT and
    ex.kind == IntLit and sameTrees(a, m.context.types.intType)
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
  elif cmp > 0 or (isIntLit and checkIntLitRange(m.context, forig, ex)):
    # f has more bits than a, great!
    if m.skippedMod in {MutT, OutT}:
      m.error ImplicitConversionNotMutable, forig, forig
    else:
      m.args.addParLe HconvX, m.argInfo
      m.args.addSubtree forig
      if isIntLit:
        if forig.typeKind == FloatT:
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
  while f.pragmaKind in {ImportcP, ImportcppP, HeaderP}:
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

proc tryTypeSymbolBase(a: var Cursor): bool =
  # returns false if non-type symbol declaration was found
  result = false
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
  result = true

proc isSomeSeqType*(a: Cursor, elemType: var Cursor): bool =
  # check that `a` is either an instantiation of seq or an invocation to it
  result = false
  var a = a
  if not tryTypeSymbolBase(a):
    return false
  if a.typeKind == InvokeT:
    inc a # tag
    result = a.kind == Symbol and pool.syms[a.symId] == "seq.0." & SystemModuleSuffix
    if result:
      inc a
      elemType = a

proc isSomeSeqType*(a: Cursor): bool {.inline.} =
  var dummy = default(Cursor)
  result = isSomeSeqType(a, dummy)

proc isSomeOpenArrayType*(a: Cursor, elemType: var Cursor): bool =
  # check that `a` is either an instantiation of openArray or an invocation to it
  result = false
  var a = a
  if not tryTypeSymbolBase(a):
    return false
  if a.typeKind == InvokeT:
    inc a # tag
    result = a.kind == Symbol and pool.syms[a.symId] == "openArray.0." & SystemModuleSuffix
    if result:
      inc a
      elemType = a

proc isSomeOpenArrayType*(a: Cursor): bool {.inline.} =
  var dummy = default(Cursor)
  result = isSomeOpenArrayType(a, dummy)

proc getTupleFieldTypeSkipTypedesc(c: Cursor): Cursor =
  result = getTupleFieldType(c)
  if result.typeKind == TypedescT:
    inc result

proc singleArgImpl(m: var Match; f: var Cursor; arg: CallArg) =
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
      singleArgImpl m, f, CallArg(n: arg.n, typ: a, orig: arg.orig)
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
      var a = skipModifier(arg.typ)
      if isObjectType(f) and isObjectType(a):
        # specialized to handle inheritance
        matchObjectTypes m, f, a, NoType
      else:
        # handled in linearMatch
        linearMatch m, f, a
    of RangetypeT:
      # for now acts the same as base type
      var a = skipModifier(arg.typ)
      if a.typeKind == RangetypeT:
        linearMatch m, f, a
      else:
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
      elif isStringType(a) and skipExpr(arg.n).kind == StringLit:
        m.args.addParLe HconvX, m.argInfo
        m.args.addSubtree f
        inc m.opened
        inc m.convCosts
        inc f
        expectParRi m, f
      elif a.typeKind == CstringT:
        inc f
        inc a
        expectPtrParRi m, f
        expectPtrParRi m, a
      else:
        m.error InvalidMatch, f, a
    of PointerT:
      var a = skipModifier(arg.typ)
      case a.typeKind
      of NiltT:
        discard "ok"
        inc f
        expectParRi m, f
      of PtrT, CstringT, RoutineTypes:
        m.args.addParLe HconvX, m.argInfo
        m.args.addSubtree f
        inc m.opened
        inc m.convCosts
        inc f
        expectPtrParRi m, f
      of PointerT:
        inc f
        inc a
        expectPtrParRi m, f
        expectPtrParRi m, a
      else:
        m.error InvalidMatch, f, a
    of PtrT, RefT:
      var a = skipModifier(arg.typ)
      let ak = a.typeKind
      if ak == NiltT:
        discard "ok"
        inc f
        skip f
        expectPtrParRi m, f
      elif ak == fk:
        inc f
        inc a
        if isObjectType(f) and isObjectType(a):
          # handle inheritance
          matchObjectTypes m, f, a, fk
        else:
          linearMatch m, f, a
        expectPtrParRi m, f
      else:
        m.error InvalidMatch, f, a
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
    of VoidT:
      inc f
      expectPtrParRi m, f
      var a = arg.typ
      if not isVoidType(a):
        m.error InvalidMatch, f, a
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
          var ffld = getTupleFieldTypeSkipTypedesc(f)
          var afld = getTupleFieldTypeSkipTypedesc(a)
          linearMatch m, ffld, afld
          # skip fields:
          skip f
          skip a
        if a.kind != ParRi:
          # len(a) > len(f)
          m.error InvalidMatch, fOrig, aOrig
    of RoutineTypes:
      var a = skipModifier(arg.typ)
      case a.typeKind
      of NiltT:
        if procHasPragma(f, ClosureP):
          m.args.addParLe NilX, m.argInfo
          m.args.addSubtree f
          inc m.opened
        skip f
      of RoutineTypes:
        procTypeMatch m, f, a
      else:
        m.error InvalidMatch, f, a
    of NoType, ErrT, ObjectT, EnumT, HoleyEnumT, NiltT, OrT, AndT, NotT,
        ConceptT, DistinctT, StaticT, ItertypeT, AutoT, SymKindT, TypeKindT, OrdinalT:
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

proc isEmptyOpenArrayCall*(n: Cursor): bool =
  if n.exprKind notin CallKinds:
    return false
  var n = n
  inc n
  result = n.kind == Symbol and
    # normal overload of `toOpenArray` for arrays:
    (pool.syms[n.symId] == "toOpenArray.0." & SystemModuleSuffix or
      # normal overload of `toOpenArray` for seqs:
      pool.syms[n.symId] == "toOpenArray.1." & SystemModuleSuffix)
  inc n
  if not isEmptyContainer(n):
    return false
  skip n
  if n.kind != ParRi:
    return false

proc addEmptyRangeType(buf: var TokenBuf; c: ptr SemContext; info: PackedLineInfo) =
  buf.addParLe(RangetypeT, info)
  buf.addSubtree c.types.intType
  buf.addIntLit(0, info)
  buf.addIntLit(-1, info)
  buf.addParRi()

proc matchEmptyContainer(m: var Match; f: var Cursor; arg: CallArg) =
  # XXX handle empty containers nested inside (expr)
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
      let fOrig = f
      singleArgImpl(m, f, arg)
      if not m.err:
        m.useArg arg, fOrig # since it was a match, copy it
        while m.opened > 0:
          m.args.addParRi()
          dec m.opened

proc singleArg(m: var Match; f: var Cursor; arg: CallArg) =
  if arg.typ.typeKind == AutoT and isEmptyContainer(arg.n):
    matchEmptyContainer(m, f, arg)
    return
  if arg.typ.typeKind == AutoT and isEmptyOpenArrayCall(arg.n):
    if isSomeOpenArrayType(f):
      # always match generated empty openarray converter call
      # argument will be instantiated after the call matches
      if not m.err:
        m.args.addSubtree arg.n
      return
    else:
      # should not happen, but still match as normal to give proper error
      discard
  let fOrig = f
  singleArgImpl(m, f, arg)
  if not m.err:
    m.useArg arg, fOrig # since it was a match, copy it
    while m.opened > 0:
      m.args.addParRi()
      dec m.opened

proc typematch*(m: var Match; formal: Cursor; arg: Item) =
  m.argInfo = arg.n.info
  var f = formal
  singleArg m, f, CallArg(n: arg.n, typ: arg.typ)

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

proc sigmatchLoop(m: var Match; f: var Cursor; args: openArray[CallArg]) =
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
      if param.val.kind != DotToken:
        m.args.add dotToken(param.val.info)
      else:
        # can end up here after named param ordering which doesn't check if params have default values
        # XXX error message should include param name
        m.error0 TooFewArguments
        break
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

proc collectDefaultValues(m: var Match; f: Cursor): seq[CallArg] =
  var f = f
  result = @[]
  while f.symKind == ParamY:
    let param = asLocal(f)
    if param.val.kind == DotToken: break
    m.insertedParam = true
    # add dot token
    result.add CallArg(n: emptyNode(m.context[]), typ: m.context.types.autoType)
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

proc sigmatch*(m: var Match; fn: FnCandidate; args: openArray[CallArg];
               explicitTypeVars: Cursor) =
  assert fn.kind != NoSym or fn.sym == SymId(0)
  m.fn = fn
  matchTypevars m, fn, explicitTypeVars

  var f = fn.typ
  if f.typeKind in RoutineTypes:
    inc f # skip ParLe
    for i in 1..4: skip f
  assert f.substructureKind == ParamsU
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
  inc aParams
  for i in 1..4: skip aParams
  assert aParams.substructureKind == ParamsU
  inc bParams
  for i in 1..4: skip bParams
  assert bParams.substructureKind == ParamsU
  inc aParams
  inc bParams
  while aParams.kind != ParRi and bParams.kind != ParRi:
    let aParam = takeLocal(aParams, SkipFinalParRi)
    let bParam = takeLocal(bParams, SkipFinalParRi)
    var aFormal = aParam.typ
    var bFormal = bParam.typ
    var ma = createMatch(c)
    singleArg ma, aFormal, CallArg(n: emptyNode(c[]), typ: bParam.typ)
    var mb = createMatch(c)
    singleArg mb, bFormal, CallArg(n: emptyNode(c[]), typ: aParam.typ)
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

proc cmpMatches*(a, b: Match; preferIterators = false): DisambiguationResult =
  assert not a.err
  assert not b.err
  if a.fn.typ.typeKind == IteratorT and b.fn.typ.typeKind != IteratorT:
    if preferIterators:
      result = FirstWins
    else:
      result = SecondWins
  elif b.fn.typ.typeKind == IteratorT and a.fn.typ.typeKind != IteratorT:
    if preferIterators:
      result = SecondWins
    else:
      result = FirstWins
  elif a.convCosts < b.convCosts:
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
      if a.fn.typ.typeKind in RoutineTypes and b.fn.typ.typeKind in RoutineTypes:
        result = mutualGenericMatch(a, b)
      else:
        result = NobodyWins

type
  ParamsInfo = object
    len: int
    names: Table[StrId, int]
    isVarargs: seq[bool] # could also use a set or store the decls and check after

proc buildParamsInfo(params: Cursor): ParamsInfo =
  result = ParamsInfo(names: initTable[StrId, int](), len: 0)
  var f = params
  assert f.isParamsTag
  inc f # "params"
  while f.kind != ParRi:
    assert f.symKind == ParamY
    var param = takeLocal(f, SkipFinalParRi)
    let isVarargs = param.typ.tagEnum == VarargsTagId
    result.isVarargs.add isVarargs
    let name = getIdent(param.name)
    result.names[name] = result.len
    inc result.len

proc orderArgs*(m: var Match; paramsCursor: Cursor; args: openArray[CallArg]): seq[CallArg] =
  let params = buildParamsInfo(paramsCursor)
  var positions = newSeq[int](params.len)
  for i in 0 ..< positions.len: positions[i] = -1
  var cont: seq[bool] = @[] # could be a set but uses less memory for most common arg counts
  var inVarargs = false
  var fi = 0
  var ai = 0
  while ai < args.len:
    # original nim uses this for next positional argument regardless of named arg:
    let nextFi = fi + 1
    var n = args[ai].n
    if n.substructureKind == VvU:
      inc n
      let name = getIdent(n)
      if name in params.names:
        fi = params.names[name]
        inVarargs = false
      else:
        swap m.pos, ai
        m.error0 NameNotFound
        swap m.pos, ai
        return
    elif fi >= params.len:
      swap m.pos, ai
      m.error0 TooManyArguments
      swap m.pos, ai
      return

    if inVarargs:
      if cont.len == 0:
        cont = newSeq[bool](args.len)
      assert ai != 0
      cont[ai - 1] = true
    elif positions[fi] < 0:
      positions[fi] = ai
    else:
      swap m.pos, ai
      m.error0 ParamAlreadyGiven
      swap m.pos, ai
      return

    if not params.isVarargs[fi]:
      fi = nextFi # will be checked on the next arg if it went over
    else:
      inVarargs = true
    inc ai

  result = newSeqOfCap[CallArg](args.len)
  fi = 0
  while fi < params.len:
    ai = positions[fi]
    if ai < 0:
      # does not fail early here for missing default value
      m.insertedParam = true
      result.add CallArg(n: emptyNode(m.context[]), typ: m.context.types.autoType)
    else:
      while true:
        var arg = args[ai]
        # remove name:
        if arg.n.substructureKind == VvU:
          inc arg.n
          skip arg.n
        result.add arg
        if cont.len != 0 and cont[ai]:
          inc ai
          assert ai < args.len
        else:
          break
    inc fi

proc sigmatchNamedArgs*(m: var Match; fn: FnCandidate; args: openArray[CallArg];
                        explicitTypeVars: Cursor;
                        hasNamedArgs: bool) =
  if hasNamedArgs:
    var params = fn.typ
    if params.typeKind in RoutineTypes:
      inc params
      for i in 1..4: skip params
    assert params.substructureKind == ParamsU
    sigmatch m, fn, orderArgs(m, params, args), explicitTypeVars
  else:
    sigmatch m, fn, args, explicitTypeVars
