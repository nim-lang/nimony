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

import std/[assertions, sets, tables, hashes]
include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, decls, programs, semdata, sembasics, asthelpers, symtabs

proc semBindStmt(c: var SemContext; dest: var TokenBuf; n: var Cursor; toBind: var HashSet[SymId]) =
  dest.addParLe(n.tag, n.info)
  n.into:  # (bind …)
    while n.hasMore:
      # If 'a' is an overloaded symbol, we used to use the first symbol
      # as a 'witness' and use the fact that subsequent lookups will yield
      # the same symbol!
      # This is however not true anymore for hygienic templates as semantic
      # processing for them changes the symbol table...
      # Capture the identifier's info up front: `takeIdent` consumes it, so
      # afterwards `n` may sit on the (virtual) ParRi where `n.info` asserts.
      let info = n.info
      let name = takeIdent(n)
      if name == StrId(0):
        c.buildErr dest, info, "invalid identifier"
      var symsBuf = createTokenBuf(4)
      discard buildSymChoice(c, symsBuf, name, info, FindAll)
      var syms = cursorAt(symsBuf, 0)
      case syms.kind
      of Ident:
        c.buildErr dest, info, "undeclared identifier: " & pool.strings[syms.litId]
      of Symbol:
        dest.addParLe(syms.tag, syms.info)
      else:
        if syms.exprKind in {OchoiceX, CchoiceX}:
          syms.into:
            while syms.hasMore:
              dest.addSubtree syms
              inc syms, AnyExpr
        else:
          bug("unreachable")
  dest.addParRi()

proc semMixinStmt(c: var SemContext; dest: var TokenBuf; n: var Cursor; toMixin: var HashSet[StrId]) =
  dest.addParLe(n.tag, n.info)
  n.into:  # (mixin …)
    while n.hasMore:
      # Capture the identifier's info up front: `takeIdent` consumes it, so
      # afterwards `n` may sit on the (virtual) ParRi where `n.info` asserts.
      let info = n.info
      let name = takeIdent(n)
      if name == StrId(0):
        c.buildErr dest, info, "invalid identifier"
      toMixin.incl(name)
      discard buildSymChoice(c, dest, name, info, FindAll)
  dest.addParRi()

type
  UntypedMode* = enum
    UntypedTemplate
    UntypedGeneric
    UntypedForwardGeneric
  UntypedCtx* = object
    c: ptr SemContext
    mode: UntypedMode
    dirty: bool
    toBind: HashSet[SymId]
    toMixin: HashSet[StrId]
    scopeIntroducedInjects: seq[HashSet[StrId]]
    currentInjects: HashSet[StrId]
    params, gensyms: HashSet[SymId]
    inNestedRoutine: int
    noGenSym: int
    inTemplateHeader: int

proc createUntypedContext*(c: ptr SemContext; mode: UntypedMode; dirty = false): UntypedCtx =
  UntypedCtx(c: c, mode: mode, dirty: dirty)

proc addParams*(c: var UntypedCtx; dest: var TokenBuf; paramsStart: int) =
  var read = cursorAt(dest, paramsStart)
  if read.substructureKind in {ParamsU, TypevarsU}:
    read.into:
      while read.hasMore:
        let param = asLocal(read)
        incl c.params, param.name.symId
        skip read
  endRead(dest)

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

proc getIdentReplaceParams(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor): bool =
  case n.kind
  of Ident:
    result = false
    var symsBuf = createTokenBuf(4)
    discard buildSymChoice(c.c[], symsBuf, n.litId, n.info, InnerMost)
    var sym = cursorAt(symsBuf, 0)
    if sym.isSymbol and isTemplParam(c, sym.symId):
      dest.addSubtree sym
      result = true
    else:
      takeToken dest, n
  of Symbol:
    result = isTemplParam(c, n.symId)
    takeToken dest, n
  of SymbolDef:
    result = false
    takeToken dest, n
  of OpenTagKind:
    if n.exprKind == QuotedX:
      dest.addParLe(n.tag, n.info)
      result = false
      n.into QuotedX:
        while n.hasMore:
          let hasParam = getIdentReplaceParams(c, dest, n)
          if hasParam:
            result = true
      dest.addParRi()
    else:
      result = false
      c.c[].buildErr dest, n.info, "illformed AST"
  else:
    result = false
    c.c[].buildErr dest, n.info, "illformed AST"

type
  TSymBinding = enum
    spNone, spGenSym, spInject

proc symBinding(n: Cursor): TSymBinding =
  result = spNone
  var n = n
  n.into:  # (pragmas …)
    while n.hasMore:
      case n.pragmaKind
      of GensymP: return spGenSym
      of InjectP: return spInject
      else: discard
      skip n

proc addDecl(c: var UntypedCtx; dest: var TokenBuf; name, pragmas: Cursor; k: SymKind; nameStart, declStart: int) =
  var name = name
  if c.mode == UntypedTemplate:
    # locals default to 'gensym', fields default to 'inject';
    # `dirty` templates treat every non-param decl as inject:
    if (not pragmas.isDotToken and symBinding(pragmas) == spInject) or
        k in {FldY, GfldY} or (c.dirty and k != ParamY):
      # even if injected, don't produce a sym choice here:
      #n = semTemplBody(c, n)
      var newNameBuf = createTokenBuf(4)
      let hasParam = getIdentReplaceParams(c, newNameBuf, name)
      var newName = cursorAt(newNameBuf, 0)
      if not hasParam:
        if k notin {FldY, GfldY}:
          let ident = takeIdent(newName)
          c.inject(ident)
      else:
        dest.replace(newName, nameStart)
    else:
      var newNameBuf = createTokenBuf(4)
      let hasParam = getIdentReplaceParams(c, newNameBuf, name)
      var newName = cursorAt(newNameBuf, 0)
      if not hasParam:
        let info = newName.info
        if newName.kind != Symbol and not (newName.isIdent and pool.strings[newName.litId] == "_"):
          var ident = pool.strings[takeIdent(newName)]
          var symName = ident
          # Templates need cross-module-unique sym names; `makeLocalSym`
          # only guarantees per-module uniqueness via `c.locals`, so a
          # template's `var x` would collide with another module's local
          # `x.0` once both end up in the global `pool.syms`.
          makeTemplateSym(c.c[], symName)
          let s = Sym(kind: k, name: pool.syms.getOrIncl(symName),
                      pos: nameStart)
          let delayed = DelayedSym(status: OkNew, lit: pool.strings.getOrIncl(ident), s: s, info: info)
          c.c[].addSym(dest, delayed)
          newNameBuf = createTokenBuf(1)
          newNameBuf.addSymDef(s.name, info)
          dest.replace(cursorAt(newNameBuf, 0), nameStart)
          if k == ParamY and c.inTemplateHeader > 0:
            c.params.incl s.name
          c.gensyms.incl s.name
          publish c.c[], dest, delayed.s.name, declStart
      else:
        dest.replace(newName, nameStart)
  else:
    let ident = getIdent(name)
    c.inject(ident)

proc addBareDecl(c: var UntypedCtx; dest: var TokenBuf, n: Cursor, k: SymKind, nameStart, declStart: int) =
  if c.mode == UntypedTemplate:
    var newNameBuf = createTokenBuf(4)
    var name = n
    let hasParam = getIdentReplaceParams(c, newNameBuf, name)
    var newName = cursorAt(newNameBuf, 0)
    if not hasParam:
      let info = newName.info
      if newName.kind != Symbol and not (newName.isIdent and pool.strings[newName.litId] == "_"):
        var ident = pool.strings[takeIdent(newName)]
        # See addDecl above: templates need cross-module-unique sym names.
        makeTemplateSym(c.c[], ident)
        let s = Sym(kind: k, name: pool.syms.getOrIncl(ident),
                    pos: dest.len)
        let delayed = DelayedSym(status: OkNew, lit: pool.strings.getOrIncl(ident), s: s, info: info)
        c.c[].addSym(dest, delayed)
        newNameBuf = createTokenBuf(1)
        newNameBuf.addSymDef(s.name, info)
        dest.replace(cursorAt(newNameBuf, 0), nameStart)
        c.gensyms.incl s.name
        publish c.c[], dest, delayed.s.name, declStart
    else:
      dest.replace(newName, nameStart)
  else:
    let ident = getIdent(n)
    c.inject(ident)

proc semTemplSymbol(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor; firstSym: SymId; count: int; start: int) =
  # handle symchoice that was produced
  assert count != 0
  let isField = c.noGenSym > 0
  if count == 1:
    dest.shrink start
    if isField and firstSym in c.gensyms:
      # change to identifier
      dest.addSubtree n
    else:
      dest.addSymUse(firstSym, n.info)
  else:
    if isField:
      var withoutGensyms = createTokenBuf(16)
      var choice = dest.cursorAt(start)
      withoutGensyms.addParLe(choice.tag, choice.info) # add tag
      choice.into:
        while choice.hasMore:
          let sym = choice.symId
          if sym notin c.gensyms:
            withoutGensyms.addSubtree choice
          inc choice, AnyExpr
      withoutGensyms.addParRi()
      dest.endRead()
      dest.shrink start
      dest.add withoutGensyms

proc semTemplBody*(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor)

proc semTemplBodySons(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor) =
  dest.addParLe(n.tag, n.info)
  n.into:
    while n.hasMore:
      semTemplBody c, dest, n
  dest.addParRi()

proc semTemplPragmas(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor) =
  # XXX should call `semTemplBody` but ignore valid pragma identifiers
  takeTree dest, n

proc semTemplGenericParams(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor) =
  semTemplBodySons c, dest, n

proc semTemplType(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor) =
  case n.typeKind
  of VoidT:
    takeToken dest, n
  of IntT, FloatT, CharT, BoolT, UIntT, NiltT, AutoT,
      SymkindT, UntypedT, TypedT, CstringT, PointerT, TypekindT, OrdinalT,
      PtrT, RefT, MutT, OutT, LentT, SinkT, NotT, UarrayT,
      StaticT, TypedescT, SetT, OrT, AndT, TupleT, ArrayT, RangetypeT, VarargsT,
      InvokeT, ErrT:
    semTemplBodySons c, dest, n
  of ObjectT:
    # open scope for fields
    openScope(c)
    semTemplBodySons c, dest, n
    closeScope(c)
  of EnumT, HoleyEnumT, AnumT:
    # no new scope
    semTemplBodySons c, dest, n
  of ConceptT:
    # open scope for proc decls
    openScope(c)
    semTemplBodySons c, dest, n
    closeScope(c)
  of DistinctT:
    semTemplBodySons c, dest, n
  of RoutineTypes:
    # open scope for param decls (ItertypeT also handled here — no params
    # have decl-scope effect for type-form routine literals but the same
    # walk is correct).
    openScope(c)
    semTemplBodySons c, dest, n
    closeScope(c)
  of NoType:
    case n.kind
    of Ident:
      semTemplBody c, dest, n
    of Symbol, SymbolDef, DotToken:
      # already resolved, e.g. when re-processing a concept body whose
      # `Self` typevar was declared by an earlier pass.
      takeToken dest, n
    else:
      bug("unreachable")

proc semTemplTypeDecl(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor) =
  let orig = n
  let decl = asTypeDecl(orig)
  let declStart = dest.len
  copyInto dest, n:
    let nameStart = dest.len
    takeTree dest, n # name
    addDecl(c, dest, decl.name, decl.pragmas, TypeY, nameStart, declStart)
    takeTree dest, n # exported
    let isGeneric = n.substructureKind == TypevarsU
    if isGeneric:
      openScope c
      semTemplGenericParams c, dest, n
    else:
      takeToken dest, n
    semTemplPragmas c, dest, n # pragmas
    semTemplType c, dest, n # body
    if isGeneric:
      closeScope c

proc semTemplLocal(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor; k: SymKind) =
  let local = asLocal(n)
  let declStart = dest.len
  copyInto dest, n:
    let nameStart = dest.len
    takeTree dest, n # name
    addDecl(c, dest, local.name, local.pragmas, k, nameStart, declStart)
    takeTree dest, n # exported
    semTemplPragmas c, dest, n # pragmas
    if k == ConstY and n.isDotToken:
      # const without explicit type: emit (auto) so the type slot is non-empty
      # in the stored generic/template body; the actual type is inferred
      # at instantiation time when semLocal processes the (auto) placeholder.
      dest.addParLe(AutoT, n.info)
      dest.addParRi()
      inc n
    else:
      semTemplType c, dest, n # type
    semTemplBody c, dest, n # value

proc semTemplRoutineDecl(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor; k: SymKind) =
  let orig = n
  let routine = asRoutine(orig)
  let declStart = dest.len
  copyInto dest, n: # proc/func/etc tag
    let nameStart = dest.len
    takeTree dest, n # name
    addDecl(c, dest, routine.name, routine.pragmas, k, nameStart, declStart)
    takeTree dest, n # exported
    semTemplBody c, dest, n # pattern
    let isGeneric = n.substructureKind == TypevarsU
    if isGeneric:
      openScope c
      semTemplGenericParams c, dest, n
    else:
      takeToken dest, n
    # params open a scope
    openScope c
    inc c.inNestedRoutine
    inc c.inTemplateHeader
    if n.substructureKind == ParamsU:
      dest.addParLe(n.tag, n.info)
      n.into ParamsU:
        while n.hasMore:
          semTemplLocal(c, dest, n, ParamY)
      dest.addParRi()
    else:
      takeToken dest, n # dot
    dec c.inTemplateHeader
    semTemplType c, dest, n # return type
    semTemplPragmas c, dest, n # pragmas
    semTemplBody c, dest, n # effects
    semTemplBody c, dest, n # body
    dec c.inNestedRoutine
    closeScope c
    if isGeneric:
      closeScope c

proc semTemplBody*(c: var UntypedCtx; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Ident:
    if isInjected(c, n.litId):
      # skUnknown case for generics
      dest.addSubtree n
      inc n
      return
    let start = dest.len
    let count = buildSymChoice(c.c[], dest, n.litId, n.info, FindOverloads)
    if count != 0:
      var firstSymN = cursorAt(dest, start)
      if firstSymN.isTagLit: inc firstSymN
      assert firstSymN.isSymbol
      let firstSym = firstSymN.symId
      endRead(dest)
      if firstSym in c.params:
        assert count == 1
        dest.shrink start
        dest.addSymUse(firstSym, n.info)
      elif c.mode == UntypedForwardGeneric:
        # leave as ident if not typevar
        dest.shrink start
        dest.addSubtree n
      elif contains(c.toBind, firstSym):
        if count == 1:
          dest.shrink start
          dest.addSymUse(firstSym, n.info)
        else:
          assert dest[start].kind == OpenTagKind
          var ctok = dest[start]
          setTag(ctok, TagId(CchoiceX))
          dest[start] = ctok
      elif contains(c.toMixin, n.litId):
        if count == 1:
          dest.shrink start
          discard buildSymChoice(c.c[], dest, n.litId, n.info, FindAll)
      elif firstSym in c.gensyms and c.noGenSym == 0:
        # template tmp[T](x: var seq[T]) =
        # var yz: T
        dest.shrink start
        dest.addSymUse(firstSym, n.info)
      else:
        semTemplSymbol(c, dest, n, firstSym, count, start)
    inc n
  of OpenTagKind:
    case n.exprKind
    of NoExpr:
      case n.stmtKind
      of BindS:
        semBindStmt c.c[], dest, n, c.toBind
      of MixinS:
        if c.inNestedRoutine > 0:
          dest.addParLe(n.tag, n.info)
          n.into MixinS:
            while n.hasMore:
              semTemplBody c, dest, n
          dest.addParRi()
        else:
          semMixinStmt c.c[], dest, n, c.toMixin
      of IfS:
        dest.addParLe(n.tag, n.info)
        n.into IfS:
          while n.hasMore:
            case n.substructureKind
            of ElifU:
              dest.addParLe(n.tag, n.info)
              n.into ElifU:
                openScope c
                semTemplBody c, dest, n
                semTemplBody c, dest, n
                closeScope c
              dest.addParRi()
            of ElseU:
              dest.addParLe(n.tag, n.info)
              n.into ElseU:
                openScope c
                semTemplBody c, dest, n
                closeScope c
              dest.addParRi()
            else:
              error "illformed AST", n
              skip n  # avoid infinite loop on illformed
        dest.addParRi()
      of WhileS:
        copyInto dest, n:
          semTemplBody c, dest, n
          openScope c
          semTemplBody c, dest, n
          closeScope c
      of CaseS:
        dest.addParLe(n.tag, n.info)
        openScope c
        n.into CaseS:
          semTemplBody c, dest, n
          while n.hasMore:
            case n.substructureKind
            of OfU:
              dest.addParLe(n.tag, n.info)
              n.into OfU:
                semTemplBody c, dest, n
                openScope c
                semTemplBody c, dest, n
                closeScope c
              dest.addParRi()
            of ElifU:
              dest.addParLe(n.tag, n.info)
              n.into ElifU:
                openScope c
                semTemplBody c, dest, n
                semTemplBody c, dest, n
                closeScope c
              dest.addParRi()
            of ElseU:
              dest.addParLe(n.tag, n.info)
              n.into ElseU:
                openScope c
                semTemplBody c, dest, n
                closeScope c
              dest.addParRi()
            else:
              error "illformed AST", n
              skip n  # avoid infinite loop
        closeScope c
        dest.addParRi()
      of ForS:
        copyInto dest, n:
          openScope c
          semTemplBody c, dest, n
          case n.substructureKind
          of UnpackflatU, UnpacktupU:
            semTemplBodySons c, dest, n
          else:
            error "illformed AST", n
          openScope c
          semTemplBody c, dest, n
          closeScope c
          closeScope c
      of BlockS:
        let orig = n
        let declStart = dest.len
        copyInto dest, n:
          openScope c
          if n.isDotToken:
            takeToken dest, n
          else:
            let nameStart = dest.len
            takeTree dest, n
            addBareDecl(c, dest, orig, BlockY, nameStart, declStart)
          semTemplBody c, dest, n
          closeScope c
      of VarS: semTemplLocal(c, dest, n, VarY)
      of LetS: semTemplLocal(c, dest, n, LetY)
      of ConstS: semTemplLocal(c, dest, n, ConstY)
      of TypeS: semTemplTypeDecl(c, dest, n)
      of ProcS: semTemplRoutineDecl(c, dest, n, ProcY)
      of FuncS: semTemplRoutineDecl(c, dest, n, FuncY)
      of IteratorS: semTemplRoutineDecl(c, dest, n, IteratorY)
      of ConverterS: semTemplRoutineDecl(c, dest, n, ConverterY)
      of MethodS: semTemplRoutineDecl(c, dest, n, MethodY)
      of TemplateS: semTemplRoutineDecl(c, dest, n, TemplateY)
      of MacroS: semTemplRoutineDecl(c, dest, n, MacroY)
      of AsgnS:
        # XXX generate `[]=`/`{}=` symchoices
        semTemplBodySons c, dest, n
      of NoStmt:
        case n.typeKind
        of NoType:
          semTemplBodySons c, dest, n
        else:
          semTemplType c, dest, n
      else:
        semTemplBodySons c, dest, n
    of AtX, CurlyatX:
      # XXX generate `[]`/`{}` symchoice
      semTemplBodySons c, dest, n
    of DotX:
      # XXX qualified symbols not special cased here, not tested if this works
      copyInto dest, n:
        semTemplBody c, dest, n
        # XXX unsure if this is 1 to 1 with `fuzzyLookup`
        inc c.noGenSym
        semTemplBody c, dest, n
        dec c.noGenSym
        if n.hasMore:
          # annoying inheritance depth:
          takeTree dest, n
    of QuotedX:
      let ident = getIdent(n)
      # emulate `qualifiedLookUp(n) != nil`:
      if isDeclared(c.c[], ident):
        # consider identifier
        var identBuf = createTokenBuf(4)
        identBuf.addIdent(ident, n.info)
        var identRead = beginRead(identBuf)
        let start = dest.len
        semTemplBody c, dest, identRead
        if dest[start].kind == Ident:
          # stayed as ident for some reason, convert back to original AST
          dest.shrink start
          takeTree dest, n
        else:
          skip n
      else:
        semTemplBodySons c, dest, n
    else:
      semTemplBodySons c, dest, n
  else:
    # atoms (Symbol/IntLit/…/SymbolDef) plus, in the classic model, a real
    # ParRi scope-close: copy the token verbatim.
    takeToken dest, n
