##[

attempt at combining generic and template untyped prepasses
important distinctions in the original compiler are:

1. generics do not gensym, instead every declaration is added as
  an skUnknown symbol which inject the identifier.
  templates introduce symbols with the same symbol kind and add injects
  to a set of identifiers.

  these are combined by using the inject set for generics,
  but keeping the inject set scoped,
  i.e. injects introduced in a scope are not injected after exiting the scope
  this changes behavior for templates but could also be disabled for them
2. generics turns param symbols etc back into identifiers,
  this does not need to be done anymore as `subs` refreshes them
3. generics have special behavior for calls (e..g macro expansion),
  not done here

mostly based on templates so there might be more inconsistencies

]##

import std/[assertions, sets]
include nifprelude
import nimony_model, decls, programs, semdata, sembasics, asthelpers, symtabs

proc semBindStmt(c: var SemContext; n: var Cursor; toBind: var HashSet[SymId]) =
  takeToken c, n
  while n.kind != ParRi:
    # If 'a' is an overloaded symbol, we used to use the first symbol
    # as a 'witness' and use the fact that subsequent lookups will yield
    # the same symbol!
    # This is however not true anymore for hygienic templates as semantic
    # processing for them changes the symbol table...
    let name = takeIdent(n)
    if name == StrId(0):
      c.buildErr n.info, "invalid identifier"
    var symsBuf = createTokenBuf(4)
    swap c.dest, symsBuf
    discard buildSymChoice(c, name, n.info, FindAll)
    swap c.dest, symsBuf
    var syms = cursorAt(symsBuf, 0)
    case syms.kind
    of Ident:
      c.buildErr n.info, "undeclared identifier: " & pool.strings[syms.litId]
    of Symbol:
      c.dest.add syms
    else:
      if syms.exprKind in {OchoiceX, CchoiceX}:
        inc syms
        while syms.kind != ParRi:
          c.dest.add syms
      else:
        bug("unreachable")
  takeParRi c, n

proc semMixinStmt(c: var SemContext; n: var Cursor; toMixin: var HashSet[StrId]) =
  takeToken c, n
  while n.kind != ParRi:
    let name = takeIdent(n)
    if name == StrId(0):
      c.buildErr n.info, "invalid identifier"
    toMixin.incl(name)
    discard buildSymChoice(c, name, n.info, FindAll)
  takeParRi c, n

type
  UntypedMode* = enum
    UntypedTemplate
    UntypedGeneric
    UntypedForwardGeneric
  UntypedCtx* = object
    c: ptr SemContext
    mode: UntypedMode
    toBind: HashSet[SymId]
    toMixin: HashSet[StrId]
    scopeIntroducedInjects: seq[HashSet[StrId]]
    currentInjects: HashSet[StrId]
    params, gensyms: HashSet[SymId]
    inNestedRoutine: int
    noGenSym: int
    inTemplateHeader: int

proc createUntypedContext*(c: ptr SemContext; mode: UntypedMode): UntypedCtx =
  UntypedCtx(c: c, mode: mode)

proc addParams*(c: var UntypedCtx; paramsStart: int) =
  var read = cursorAt(c.c.dest, paramsStart)
  if read.substructureKind in {ParamsU, TypevarsU}:
    inc read # skip tag
    while read.kind != ParRi:
      let param = asLocal(read)
      incl c.params, param.name.symId
      skip read
  endRead(c.c.dest)

proc openScope(c: var UntypedCtx) =
  openScope c.c[]
  c.scopeIntroducedInjects.add(initHashSet[StrId]())

proc closeScope(c: var UntypedCtx) =
  closeScope c.c[]
  var last = c.scopeIntroducedInjects.pop()
  for prevInject in last:
    c.currentInjects.excl prevInject

proc inject(c: var UntypedCtx, s: StrId) =
  if not c.currentInjects.containsOrIncl(s):
    if c.scopeIntroducedInjects.len != 0:
      c.scopeIntroducedInjects[^1].incl s

proc isInjected(c: var UntypedCtx, s: StrId): bool {.inline.} =
  result = s in c.currentInjects

proc isTemplParam(c: UntypedCtx, s: SymId): bool {.inline.} =
  result = s in c.params

proc getIdentReplaceParams(c: var UntypedCtx, n: var Cursor): bool =
  case n.kind
  of Ident:
    result = false
    var symsBuf = createTokenBuf(4)
    swap c.c.dest, symsBuf
    discard buildSymChoice(c.c[], n.litId, n.info, InnerMost)
    swap c.c.dest, symsBuf
    var sym = cursorAt(symsBuf, 0)
    if sym.kind == Symbol and isTemplParam(c, sym.symId):
      c.c.dest.add sym
      result = true
    else:
      takeToken c.c[], n
  of Symbol:
    result = isTemplParam(c, n.symId)
    takeToken c.c[], n
  of SymbolDef:
    result = false
    takeToken c.c[], n
  of ParLe:
    if n.exprKind == QuotedX:
      takeToken c.c[], n
      result = false
      while n.kind != ParRi:
        let hasParam = getIdentReplaceParams(c, n)
        if hasParam:
          result = true
      takeParRi c.c[], n
    else:
      result = false
      c.c[].buildErr n.info, "illformed AST"
  else:
    result = false
    c.c[].buildErr n.info, "illformed AST"

type
  TSymBinding = enum
    spNone, spGenSym, spInject

proc symBinding(n: Cursor): TSymBinding =
  result = spNone
  var n = n
  inc n
  while n.kind != ParRi:
    case n.pragmaKind
    of GensymP: return spGenSym
    of InjectP: return spInject
    else: discard
    skip n

proc addDecl(c: var UntypedCtx; name, pragmas: Cursor; k: SymKind; nameStart, declStart: int) =
  var name = name
  if c.mode == UntypedTemplate:
    # locals default to 'gensym', fields default to 'inject':
    if (pragmas.kind != DotToken and symBinding(pragmas) == spInject) or
        k == FldY:
      # even if injected, don't produce a sym choice here:
      #n = semTemplBody(c, n)
      var newNameBuf = createTokenBuf(4)
      swap c.c.dest, newNameBuf
      let hasParam = getIdentReplaceParams(c, name)
      swap c.c.dest, newNameBuf
      var newName = cursorAt(newNameBuf, 0)
      if not hasParam:
        if k != FldY:
          let ident = takeIdent(newName)
          c.inject(ident)
      else:
        c.c.dest.replace(newName, nameStart)
    else:
      var newNameBuf = createTokenBuf(4)
      swap c.c.dest, newNameBuf
      let hasParam = getIdentReplaceParams(c, name)
      swap c.c.dest, newNameBuf
      var newName = cursorAt(newNameBuf, 0)
      if not hasParam:
        let info = newName.info
        if newName.kind != Symbol and not (newName.kind == Ident and pool.strings[newName.litId] == "_"):
          var ident = pool.strings[takeIdent(newName)]
          var symName = ident
          makeLocalSym(c.c[], symName)
          let s = Sym(kind: k, name: pool.syms.getOrIncl(symName),
                      pos: nameStart)
          let delayed = DelayedSym(status: OkNew, lit: pool.strings.getOrIncl(ident), s: s, info: info)
          c.c[].addSym(delayed)
          newNameBuf = createTokenBuf(1)
          newNameBuf.add symdefToken(s.name, info)
          c.c.dest.replace(cursorAt(newNameBuf, 0), nameStart)
          if k == ParamY and c.inTemplateHeader > 0:
            c.params.incl s.name
          c.gensyms.incl s.name
          publish c.c[], delayed.s.name, declStart
      else:
        c.c.dest.replace(newName, nameStart)
  else:
    let ident = getIdent(name)
    c.inject(ident)

proc addBareDecl(c: var UntypedCtx, n: Cursor, k: SymKind, nameStart, declStart: int) =
  if c.mode == UntypedTemplate:
    var newNameBuf = createTokenBuf(4)
    swap c.c.dest, newNameBuf
    var name = n
    let hasParam = getIdentReplaceParams(c, name)
    swap c.c.dest, newNameBuf
    var newName = cursorAt(newNameBuf, 0)
    if not hasParam:
      let info = newName.info
      if newName.kind != Symbol and not (newName.kind == Ident and pool.strings[newName.litId] == "_"):
        var ident = pool.strings[takeIdent(newName)]
        makeLocalSym(c.c[], ident)
        let s = Sym(kind: k, name: pool.syms.getOrIncl(ident),
                    pos: c.c.dest.len)
        let delayed = DelayedSym(status: OkNew, lit: pool.strings.getOrIncl(ident), s: s, info: info)
        c.c[].addSym(delayed)
        newNameBuf = createTokenBuf(1)
        newNameBuf.add symdefToken(s.name, info)
        c.c.dest.replace(cursorAt(newNameBuf, 0), nameStart)
        c.gensyms.incl s.name
        publish c.c[], delayed.s.name, declStart
    else:
      c.c.dest.replace(newName, nameStart)
  else:
    let ident = getIdent(n)
    c.inject(ident)

proc semTemplSymbol(c: var UntypedCtx; n: var Cursor; firstSym: SymId; count: int; start: int) =
  # handle symchoice that was produced
  assert count != 0
  let isField = c.noGenSym > 0
  if count == 1:
    c.c.dest.shrink start
    if isField and firstSym in c.gensyms:
      # change to identifier
      c.c.dest.add n
    else:
      c.c.dest.add symToken(firstSym, n.info)
  else:
    if isField:
      var withoutGensyms = createTokenBuf(16)
      var choice = c.c.dest.cursorAt(start)
      withoutGensyms.add choice # add tag
      inc choice
      while choice.kind != ParRi:
        let sym = choice.symId
        if sym notin c.gensyms:
          withoutGensyms.add choice
        inc choice
      takeParRi withoutGensyms, choice
      c.c.dest.endRead()
      c.c.dest.shrink start
      c.c.dest.add withoutGensyms

proc semTemplBody*(c: var UntypedCtx; n: var Cursor)

proc semTemplBodySons(c: var UntypedCtx; n: var Cursor) =
  takeToken c.c[], n
  while n.kind != ParRi:
    semTemplBody c, n
  takeParRi c.c[], n

proc semTemplPragmas(c: var UntypedCtx; n: var Cursor) =
  # XXX should call `semTemplBody` but ignore valid pragma identifiers
  takeTree c.c[], n

proc semTemplGenericParams(c: var UntypedCtx; n: var Cursor) =
  semTemplBodySons c, n

proc semTemplType(c: var UntypedCtx; n: var Cursor) =
  case n.typeKind
  of VoidT:
    takeToken c.c[], n
  of IntT, FloatT, CharT, BoolT, UIntT, NiltT, AutoT,
      SymKindT, UntypedT, TypedT, CstringT, PointerT, TypeKindT, OrdinalT,
      PtrT, RefT, MutT, OutT, LentT, SinkT, NotT, UarrayT,
      StaticT, TypedescT, SetT, OrT, AndT, TupleT, ArrayT, RangetypeT, VarargsT,
      InvokeT, ErrT:
    semTemplBodySons c, n
  of ObjectT:
    # open scope for fields
    openScope(c)
    semTemplBodySons c, n
    closeScope(c)
  of EnumT, HoleyEnumT:
    # no new scope
    semTemplBodySons c, n
  of ConceptT:
    # open scope for proc decls
    openScope(c)
    semTemplBodySons c, n
    closeScope(c)
  of DistinctT:
    semTemplBodySons c, n
  of RoutineTypes:
    # open scope for param decls
    openScope(c)
    semTemplBodySons c, n
    closeScope(c)
  of ItertypeT:
    semTemplBodySons c, n
  of NoType:
    if n.kind == Ident:
      semTemplBody c, n
    else:
      bug("unreachable")

proc semTemplTypeDecl(c: var UntypedCtx; n: var Cursor) =
  let orig = n
  let decl = asTypeDecl(orig)
  let declStart = c.c.dest.len
  takeToken c.c[], n
  let nameStart = c.c.dest.len
  takeTree c.c[], n # name
  addDecl(c, decl.name, decl.pragmas, TypeY, nameStart, declStart)
  takeTree c.c[], n # exported
  let isGeneric = n.kind != DotToken
  if isGeneric:
    openScope c
    semTemplGenericParams c, n
  else:
    takeToken c.c[], n
  semTemplPragmas c, n # pragmas
  semTemplType c, n # body
  takeParRi c.c[], n
  if isGeneric:
    closeScope c

proc semTemplLocal(c: var UntypedCtx; n: var Cursor; k: SymKind) =
  let local = asLocal(n)
  let declStart = c.c.dest.len
  takeToken c.c[], n
  let nameStart = c.c.dest.len
  takeTree c.c[], n # name
  addDecl(c, local.name, local.pragmas, k, nameStart, declStart)
  takeTree c.c[], n # exported
  semTemplPragmas c, n # pragmas
  semTemplType c, n # type
  semTemplBody c, n # value
  takeParRi c.c[], n

proc semTemplRoutineDecl(c: var UntypedCtx; n: var Cursor; k: SymKind) =
  # XXX todo
  raiseAssert("unimplemented")

proc semTemplBody*(c: var UntypedCtx; n: var Cursor) =
  case n.kind
  of Ident:
    if isInjected(c, n.litId):
      # skUnknown case for generics
      c.c.dest.add n
      inc n
      return
    let start = c.c.dest.len
    let count = buildSymChoice(c.c[], n.litId, n.info, FindOverloads)
    if count != 0:
      var firstSymN = cursorAt(c.c.dest, start)
      if firstSymN.kind == ParLe: inc firstSymN
      assert firstSymN.kind == Symbol
      let firstSym = firstSymN.symId
      endRead(c.c.dest)
      if firstSym in c.params:
        assert count == 1
        c.c.dest.shrink start
        c.c.dest.add symToken(firstSym, n.info)
      elif c.mode == UntypedForwardGeneric:
        # leave as ident if not typevar
        c.c.dest.shrink start
        c.c.dest.add n
      elif contains(c.toBind, firstSym):
        if count == 1:
          c.c.dest.shrink start
          c.c.dest.add symToken(firstSym, n.info)
        else:
          let tag = c.c.dest[start]
          assert tag.kind == ParLe
          c.c.dest[start] = parLeToken(CchoiceX, tag.info)
      elif contains(c.toMixin, n.litId):
        if count == 1:
          c.c.dest.shrink start
          discard buildSymChoice(c.c[], n.litId, n.info, FindAll)
      elif firstSym in c.gensyms and c.noGenSym == 0:
        # template tmp[T](x: var seq[T]) =
        # var yz: T
        c.c.dest.shrink start
        c.c.dest.add symToken(firstSym, n.info)
      else:
        semTemplSymbol(c, n, firstSym, count, start)
    inc n
  of Symbol, IntLit, UIntLit, CharLit, StringLit, FloatLit,
      DotToken, UnknownToken, EofToken, ParRi, SymbolDef: # ?
    takeToken c.c[], n
  of ParLe:
    case n.exprKind
    of NoExpr:
      case n.stmtKind
      of BindS:
        semBindStmt c.c[], n, c.toBind
      of MixinS:
        if c.inNestedRoutine > 0:
          takeToken c.c[], n
          while n.kind != ParRi:
            semTemplBody c, n
          takeParRi c.c[], n
        else:
          semMixinStmt c.c[], n, c.toMixin
      of IfS:
        takeToken c.c[], n
        while n.kind != ParRi:
          case n.substructureKind
          of ElifU:
            takeToken c.c[], n
            openScope c
            semTemplBody c, n
            semTemplBody c, n
            closeScope c
            takeParRi c.c[], n
          of ElseU:
            takeToken c.c[], n
            openScope c
            semTemplBody c, n
            closeScope c
            takeParRi c.c[], n
          else:
            error "illformed AST", n
        takeParRi c.c[], n
      of WhileS:
        takeToken c.c[], n
        semTemplBody c, n
        openScope c
        semTemplBody c, n
        closeScope c
        takeParRi c.c[], n
      of CaseS:
        takeToken c.c[], n
        openScope c
        semTemplBody c, n
        while n.kind != ParRi:
          case n.substructureKind
          of OfU:
            takeToken c.c[], n
            semTemplBody c, n
            openScope c
            semTemplBody c, n
            closeScope c
            takeParRi c.c[], n
          of ElifU:
            takeToken c.c[], n
            openScope c
            semTemplBody c, n
            semTemplBody c, n
            closeScope c
            takeParRi c.c[], n
          of ElseU:
            takeToken c.c[], n
            openScope c
            semTemplBody c, n
            closeScope c
            takeParRi c.c[], n
          else:
            error "illformed AST", n
        closeScope c
        takeParRi c.c[], n
      of ForS:
        takeToken c.c[], n
        openScope c
        semTemplBody c, n
        case n.substructureKind
        of UnpackFlatU, UnpackTupU:
          semTemplBodySons c, n
        else:
          error "illformed AST", n
        openScope c
        semTemplBody c, n
        closeScope c
        closeScope c
        takeParRi c.c[], n
      of BlockS:
        let orig = n
        let declStart = c.c.dest.len
        takeToken c.c[], n
        openScope c
        if n.kind == DotToken:
          takeToken c.c[], n
        else:
          let nameStart = c.c.dest.len
          takeTree c.c[], n
          addBareDecl(c, orig, BlockY, nameStart, declStart)
        semTemplBody c, n
        closeScope c
        takeParRi c.c[], n
      of VarS: semTemplLocal(c, n, VarY)
      of LetS: semTemplLocal(c, n, LetY)
      of ConstS: semTemplLocal(c, n, ConstY)
      of TypeS: semTemplTypeDecl(c, n)
      of ProcS: semTemplRoutineDecl(c, n, ProcY)
      of FuncS: semTemplRoutineDecl(c, n, FuncY)
      of IteratorS: semTemplRoutineDecl(c, n, IteratorY)
      of ConverterS: semTemplRoutineDecl(c, n, ConverterY)
      of MethodS: semTemplRoutineDecl(c, n, MethodY)
      of TemplateS: semTemplRoutineDecl(c, n, TemplateY)
      of MacroS: semTemplRoutineDecl(c, n, MacroY)
      of AsgnS:
        # XXX generate `[]=`/`{}=` symchoices
        semTemplBodySons c, n
      of NoStmt:
        case n.typeKind
        of NoType:
          semTemplBodySons c, n
        else:
          semTemplType c, n
      else:
        semTemplBodySons c, n
    of AtX:
      # XXX generate `[]`/`{}` symchoice
      semTemplBodySons c, n
    of DotX:
      # XXX qualified symbols not special cased here, not tested if this works
      takeToken c.c[], n
      semTemplBody c, n
      # XXX unsure if this is 1 to 1 with `fuzzyLookup`
      inc c.noGenSym
      semTemplBody c, n
      dec c.noGenSym
      if n.kind != ParRi:
        # annoying inheritance depth:
        takeTree c.c[], n
      takeParRi c.c[], n
    of QuotedX:
      let ident = getIdent(n)
      # emulate `qualifiedLookUp(n) != nil`:
      if isDeclared(c.c[], ident):
        # consider identifier
        var identBuf = createTokenBuf(4)
        identBuf.add identToken(ident, n.info)
        var identRead = beginRead(identBuf)
        let start = c.c.dest.len
        semTemplBody c, identRead
        if c.c.dest[start].kind == Ident:
          # stayed as ident for some reason, convert back to original AST
          c.c.dest.shrink start
          takeTree c.c.dest, n
        else:
          skip n
      else:
        semTemplBodySons c, n
    else:
      semTemplBodySons c, n
