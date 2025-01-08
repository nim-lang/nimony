## attempt at combining generic and template untyped prepasses
## important distinctions are:
## 
## 1. generics do not gensym and introduce skUnknown symbols which act as injects,
##   templates introduce symbols with the same symbol kind
##   - aim is to use the inject mechanism for generics but with scope behavior
##     mixing this with the template behavior doesn't really make sense
##     (i.e. real symbol - inject - real symbol, injects would be checked first)
##     but would be backwards compatible
## 2. the symbol binding rules seem slightly different (can't tell if these are mostly just special rules for dot fields)

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
    let name = getIdent(n)
    if name == StrId(0):
      c.buildErr n.info, "invalid identifier"
    var symsBuf = createTokenBuf(4)
    swap c.dest, symsBuf
    discard buildSymChoice(c, name, n.info, FindAll)
    swap c.dest, symsBuf
    var syms = cursorAt(symsBuf, 0)
    case syms.kind
    of Ident:
      c.buildErr n.info, "undeclared identifier"
    of Symbol:
      c.dest.add syms
    else:
      if syms.exprKind in {OchoiceX, CchoiceX}:
        inc syms
        while syms.kind != ParRi:
          c.dest.add syms
      else:
        raiseAssert("unreachable")
  wantParRi c, n

proc semMixinStmt(c: var SemContext; n: var Cursor; toMixin: var HashSet[StrId]) =
  takeToken c, n
  while n.kind != ParRi:
    let name = getIdent(n)
    if name == StrId(0):
      c.buildErr n.info, "invalid identifier"
    toMixin.incl(name)
    discard buildSymChoice(c, name, n.info, FindAll)
  wantParRi c, n

type
  UntypedCtxMode = enum
    UntypedTemplate
    UntypedGeneric
  UntypedCtx = object
    c: ptr SemContext
    mode: UntypedCtxMode
    toBind: HashSet[SymId]
    toMixin: HashSet[StrId]
    scopeIntroducedInjects: seq[HashSet[StrId]]
    currentInjects: HashSet[StrId]
    params, gensyms: HashSet[SymId]
    cursorInBody: bool # only for nimsuggest
    scopeN: int
    noGenSym: int
    inTemplateHeader: int

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
  of Symbol, SymbolDef:
    result = isTemplParam(c, n.symId)
    takeToken c.c[], n
  of ParLe:
    if n == $QuotedX:
      takeToken c.c[], n
      result = false
      while n.kind != ParRi:
        let hasParam = getIdentReplaceParams(c, n)
        if hasParam:
          result = true
      wantParRi c.c[], n
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
    of Gensym: return spGenSym
    of Inject: return spInject
    else: discard
    skip n

proc addLocalDecl(c: var UntypedCtx, n: Cursor, k: SymKind; nameStart: int) =
  let local = asLocal(n)
  if c.mode == UntypedTemplate:
    # locals default to 'gensym', fields default to 'inject':
    var pragmas = local.pragmas
    if (pragmas.kind != DotToken and symBinding(pragmas) == spInject) or
        k == FldY:
      # even if injected, don't produce a sym choice here:
      #n = semTemplBody(c, n)
      var name = local.name
      var newNameBuf = createTokenBuf(4)
      swap c.c.dest, newNameBuf
      let hasParam = getIdentReplaceParams(c, name)
      swap c.c.dest, newNameBuf
      var newName = cursorAt(newNameBuf, 0)
      if not hasParam:
        if k != FldY:
          let ident = getIdent(newName)
          c.inject(ident)
      else:
        c.c.dest.replace(newName, nameStart)
    else:
      if pragmas.kind != DotToken:
        inc pragmas
        when false:
          while pragmas.kind != ParRi:
            # see D20210801T100514
            var found = false
            if ni.kind == nkIdent:
              for a in templatePragmas:
                if ni.ident.id == ord(a):
                  found = true
                  break
            if not found:
              openScope(c)
              pragmaNode[i] = semTemplBody(c, pragmaNode[i])
              closeScope(c)
      var name = local.name
      var newNameBuf = createTokenBuf(4)
      swap c.c.dest, newNameBuf
      let hasParam = getIdentReplaceParams(c, name)
      swap c.c.dest, newNameBuf
      var newName = cursorAt(newNameBuf, 0)
      if not hasParam:
        let info = newName.info
        if newName.kind != Symbol and not (newName.kind == Ident and pool.strings[newName.litId] == "_"):
          var ident = pool.strings[getIdent(newName)]
          makeLocalSym(c.c[], ident)
          let s = Sym(kind: k, name: pool.syms.getOrIncl(ident),
                      pos: c.c.dest.len)
          let delayed = DelayedSym(status: OkNew, lit: pool.strings.getOrIncl(ident), s: s, info: info)
          c.c[].addSym(delayed)
          newNameBuf = createTokenBuf(1)
          newNameBuf.add symdefToken(s.name, info)
          c.c.dest.replace(cursorAt(newNameBuf, 0), nameStart)
          if k == ParamY and c.inTemplateHeader > 0:
            c.params.incl s.name
      else:
        c.c.dest.replace(newName, nameStart)
  else:
    var name = asLocal(n).name
    let ident = getIdent(name)
    c.inject(ident)

when false:
  proc semTemplSymbol(c: var TemplCtx, n: var Cursor, s: SymId; isField, isAmbiguous: bool) =
    case s.kind
    #of skUnknown:
    #  # Introduced in this pass! Leave it as an identifier.
    #  result = n
    of OverloadableSyms:
      result = symChoice(c.c, n, s, scOpen, isField)
      if not isField and result.kind in {nkSym, nkOpenSymChoice}:
        if openSym in c.c.features:
          if result.kind == nkSym:
            result = newOpenSym(result)
          else:
            result.typ() = nil
        else:
          result.flags.incl nfDisabledOpenSym
          result.typ() = nil
    of skGenericParam:
      if isField and sfGenSym in s.flags: result = n
      else:
        result = newSymNodeTypeDesc(s, c.c.idgen, n.info)
        if not isField and s.owner != c.owner:
          if openSym in c.c.features:
            result = newOpenSym(result)
          else:
            result.flags.incl nfDisabledOpenSym
            result.typ() = nil
    of skParam:
      result = n
    of skType:
      if isField and sfGenSym in s.flags: result = n
      else:
        if isAmbiguous:
          # ambiguous types should be symchoices since lookup behaves
          # differently for them in regular expressions
          result = symChoice(c.c, n, s, scOpen, isField)
        else: result = newSymNodeTypeDesc(s, c.c.idgen, n.info)
        if not isField and not (s.owner == c.owner and
            s.typ != nil and s.typ.kind == tyGenericParam) and
            result.kind in {nkSym, nkOpenSymChoice}:
          if openSym in c.c.features:
            if result.kind == nkSym:
              result = newOpenSym(result)
            else:
              result.typ() = nil
          else:
            result.flags.incl nfDisabledOpenSym
            result.typ() = nil
    else:
      if isField and sfGenSym in s.flags: result = n
      else:
        result = newSymNode(s, n.info)
        if not isField:
          if openSym in c.c.features:
            result = newOpenSym(result)
          else:
            result.flags.incl nfDisabledOpenSym
            result.typ() = nil
      # Issue #12832
      when defined(nimsuggest):
        suggestSym(c.c.graph, n.info, s, c.c.graph.usageSym, false)
      # field access (dot expr) will be handled by builtinFieldAccess
      if not isField:
        styleCheckUse(c.c, n.info, s)

  proc semRoutineInTemplName(c: var TemplCtx, n: PNode, explicitInject: bool): PNode =
    result = n
    if n.kind == nkIdent:
      let s = qualifiedLookUp(c.c, n, {})
      if s != nil:
        if s.owner == c.owner and (s.kind == skParam or
            (sfGenSym in s.flags and not explicitInject)):
          incl(s.flags, sfUsed)
          result = newSymNode(s, n.info)
          onUse(n.info, s)
    else:
      for i in 0..<n.safeLen:
        result[i] = semRoutineInTemplName(c, n[i], explicitInject)

  proc semRoutineInTemplBody(c: var TemplCtx, n: PNode, k: TSymKind): PNode =
    result = n
    checkSonsLen(n, bodyPos + 1, c.c.config)
    if n.kind notin nkLambdaKinds:
      # routines default to 'inject':
      let binding = symBinding(n[pragmasPos])
      if binding == spGenSym:
        let (ident, hasParam) = getIdentReplaceParams(c, n[namePos])
        if not hasParam:
          var s = newGenSym(k, ident, c)
          s.ast = n
          addPrelimDecl(c.c, s)
          styleCheckDef(c.c, n.info, s)
          onDef(n.info, s)
          n[namePos] = newSymNode(s, n[namePos].info)
        else:
          n[namePos] = ident
      else:
        n[namePos] = semRoutineInTemplName(c, n[namePos], binding == spInject)
    # open scope for parameters
    openScope(c)
    for i in patternPos..paramsPos-1:
      n[i] = semTemplBody(c, n[i])

    if k == skTemplate: inc(c.inTemplateHeader)
    n[paramsPos] = semTemplBody(c, n[paramsPos])
    if k == skTemplate: dec(c.inTemplateHeader)

    for i in paramsPos+1..miscPos:
      n[i] = semTemplBody(c, n[i])
    # open scope for locals
    inc c.scopeN
    openScope(c)
    n[bodyPos] = semTemplBody(c, n[bodyPos])
    # close scope for locals
    closeScope(c)
    dec c.scopeN
    # close scope for parameters
    closeScope(c)

proc semTemplLocal(c: var UntypedCtx; n: var Cursor; k: SymKind) =
  raiseAssert("unimplemented")

proc semTemplTypeDecl(c: var UntypedCtx; n: var Cursor) =
  raiseAssert("unimplemented")

proc semTemplRoutineDecl(c: var UntypedCtx; n: var Cursor; k: SymKind) =
  raiseAssert("unimplemented")

proc semTemplBody(c: var UntypedCtx; n: var Cursor) =
  case n.kind
  of Ident:
    if isInjected(c, n.litId):
      # skUnknown case for generics
      c.c.dest.add n
      return
    #var choiceBuf = createTokenBuf(4)
    #swap c.c.dest, choiceBuf
    let start = c.c.dest.len
    let count = buildSymChoice(c.c[], n.litId, n.info, InnerMost)
    #swap c.c.dest, choiceBuf
    if count != 0:
      var firstSymN = cursorAt(c.c.dest, start)
      if firstSymN.kind == ParLe: inc firstSymN
      if firstSymN.kind != Symbol:
        # error maybe
        return
      let firstSym = firstSymN.symId
      endRead(c.c.dest)
      if firstSym in c.params:
        assert count == 1
      elif contains(c.toBind, firstSym):
        let tag = c.c.dest[start]
        if tag.kind == ParLe and pool.tags[tag.tagId] == $OchoiceX:
          c.c.dest[start] = parLeToken(CchoiceX, tag.info)
      elif contains(c.toMixin, n.litId):
        let tag = c.c.dest[start]
        if tag.kind == Symbol:
          var ochoiceBuf = createTokenBuf(4)
          ochoiceBuf.addParLe(OchoiceX, tag.info)
          ochoiceBuf.add tag
          ochoicebuf.addParRi()
          replace(c.c.dest, cursorAt(ochoiceBuf, 0), start)
        elif tag.kind == ParLe and pool.tags[tag.tagId] == $CchoiceX:
          c.c.dest[start] = parLeToken(OchoiceX, tag.info)
      elif firstSym in c.gensyms and c.noGenSym == 0:
        # template tmp[T](x: var seq[T]) =
        # var yz: T
        discard
      else:
        when false:
          if s.kind in {skVar, skLet, skConst}:
            discard qualifiedLookUp(c.c, n, {checkAmbiguity, checkModule})
          result = semTemplSymbol(c, n, s, c.noGenSym > 0, c.c.isAmbiguous)
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
        if c.scopeN > 0:
          takeToken c.c[], n
          while n.kind != ParRi:
            semTemplBody c, n
          wantParRi c.c[], n
        else:
          semMixinStmt c.c[], n, c.toMixin
      of IfS:
        takeToken c.c[], n
        while n.kind != ParRi:
          case n.substructureKind
          of ElifS:
            openScope c
            semTemplBody c, n
            semTemplBody c, n
            closeScope c
          of ElseS:
            openScope c
            semTemplBody c, n
            closeScope c
          else: raiseAssert("illformed AST")
        wantParRi c.c[], n
      of WhileS:
        takeToken c.c[], n
        openScope c
        semTemplBody c, n
        closeScope c
        wantParRi c.c[], n
      of CaseS:
        takeToken c.c[], n
        openScope c
        semTemplBody c, n
        while n.kind != ParRi:
          case n.substructureKind
          of OfS:
            semTemplBody c, n
            openScope c
            semTemplBody c, n
            closeScope c
          of ElifS:
            openScope c
            semTemplBody c, n
            semTemplBody c, n
            closeScope c
          of ElseS:
            openScope c
            semTemplBody c, n
            closeScope c
          else: raiseAssert("illformed AST")
        closeScope c
        wantParRi c.c[], n
      of ForS:
        takeToken c.c[], n
        openScope c
        case n.substructureKind
        of UnpackFlatS, UnpackTupS:
          takeToken c.c[], n
          while n.kind != ParRi:
            semTemplBody c, n
          wantParRi c.c[], n
        else: raiseAssert("illformed AST")
        semTemplBody c, n
        openScope c
        semTemplBody c, n
        closeScope c
        closeScope c
        wantParRi c.c[], n
      of BlockS:
        takeToken c.c[], n
        openScope c
        if n.kind == DotToken:
          takeToken c.c[], n
        else:
          raiseAssert("unimplemented")
        semTemplBody c, n
        closeScope c
        wantParRi c.c[], n
      of VarS: semTemplLocal(c, n, VarY)
      of LetS: semTemplLocal(c, n, LetY)
      of ConstS: semTemplLocal(c, n, ConstY)
      of TypeS: semTemplTypeDecl(c, n)
      of ProcS: semTemplRoutineDecl(c, n, ProcY)
      of FuncS: semTemplRoutineDecl(c, n, FuncY)
      of IterS: semTemplRoutineDecl(c, n, IterY)
      of ConverterS: semTemplRoutineDecl(c, n, ConverterY)
      of MethodS: semTemplRoutineDecl(c, n, MethodY)
      of TemplateS: semTemplRoutineDecl(c, n, TemplateY)
      of MacroS: semTemplRoutineDecl(c, n, MacroY)
      of AsgnS:
        when false:
          checkSonsLen(n, 2, c.c.config)
          let a = n[0]
          let b = n[1]

          let k = a.kind
          case k
          of nkBracketExpr:
            if a.typ == nil:
              # see nkBracketExpr case above for explanation
              result = newNodeI(nkCall, n.info)
              result.add newIdentNode(getIdent(c.c.cache, "[]="), n.info)
              for i in 0..<a.len: result.add(a[i])
              result.add(b)
            let a0 = semTemplBody(c, a[0])
            result = semTemplBodySons(c, result)
          of nkCurlyExpr:
            if a.typ == nil:
              # see nkBracketExpr case above for explanation
              result = newNodeI(nkCall, n.info)
              result.add newIdentNode(getIdent(c.c.cache, "{}="), n.info)
              for i in 0..<a.len: result.add(a[i])
              result.add(b)
            result = semTemplBodySons(c, result)
          else:
            result = semTemplBodySons(c, n)
      else:
        # XXX type expressions
        raiseAssert("unimplemented")
    of NilX:
      takeTree c.c[], n
    of AtX:
      when false:
        if n.typ == nil:
          # if a[b] is nested inside a typed expression, don't convert it
          # back to `[]`(a, b), prepareOperand will not typecheck it again
          # and so `[]` will not be resolved
          # checking if a[b] is typed should be enough to cover this case
          result = newNodeI(nkCall, n.info)
          result.add newIdentNode(getIdent(c.c.cache, "[]"), n.info)
          for i in 0..<n.len: result.add(n[i])
        result = semTemplBodySons(c, result)
    of DotX, QuotedX:
      when false:
        # dotExpr is ambiguous: note that we explicitly allow 'x.TemplateParam',
        # so we use the generic code for nkDotExpr too
        c.c.isAmbiguous = false
        let s = qualifiedLookUp(c.c, n, {})
        if s != nil:
          # mirror the nkIdent case
          # do not symchoice a quoted template parameter (bug #2390):
          if s.owner == c.owner and s.kind == skParam and
              n.kind == nkAccQuoted and n.len == 1:
            incl(s.flags, sfUsed)
            onUse(n.info, s)
            return newSymNode(s, n.info)
          elif contains(c.toBind, s.id):
            return symChoice(c.c, n, s, scClosed, c.noGenSym > 0)
          elif contains(c.toMixin, s.name.id):
            return symChoice(c.c, n, s, scForceOpen, c.noGenSym > 0)
          else:
            if s.kind in {skVar, skLet, skConst}:
              discard qualifiedLookUp(c.c, n, {checkAmbiguity, checkModule})
            return semTemplSymbol(c, n, s, c.noGenSym > 0, c.c.isAmbiguous)
        if n.kind == nkDotExpr:
          result = n
          result[0] = semTemplBody(c, n[0])
          inc c.noGenSym
          result[1] = semTemplBody(c, n[1])
          dec c.noGenSym
          if result[1].kind == nkSym and result[1].sym.kind in routineKinds:
            # prevent `dotTransformation` from rewriting this node to `nkIdent`
            # by making it a symchoice
            # in generics this becomes `nkClosedSymChoice` but this breaks code
            # as the old behavior here was that this became `nkIdent`
            var choice = newNodeIT(nkOpenSymChoice, n[1].info, newTypeS(tyNone, c.c))
            choice.add result[1]
            result[1] = choice
        else:
          result = semTemplBodySons(c, n)
    else:
      takeToken c.c[], n
      while n.kind != ParRi:
        semTemplBody c, n
      wantParRi c.c[], n
