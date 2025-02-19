## attempt at combining generic and template untyped prepasses
## important distinctions are:
## 
## 1. generics do not gensym and introduce skUnknown symbols which act as injects,
##    templates introduce symbols with the same symbol kind
##  - aim is to use the inject mechanism for generics but with scope behavior
##    mixing this with the template behavior doesn't really make sense
##    (i.e. real symbol - inject - real symbol, injects would be checked first)
##    but would be backwards compatible
##    - injected symbols could be handled in scope object
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
  takeParRi c, n

proc semMixinStmt(c: var SemContext; n: var Cursor; toMixin: var HashSet[StrId]) =
  takeToken c, n
  while n.kind != ParRi:
    let name = getIdent(n)
    if name == StrId(0):
      c.buildErr n.info, "invalid identifier"
    toMixin.incl(name)
    discard buildSymChoice(c, name, n.info, FindAll)
  takeParRi c, n

type
  UntypedMode = enum
    UntypedTemplate
    UntypedGeneric
  UntypedCtx = object
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
    var name = local.name
    let ident = getIdent(name)
    c.inject(ident)

proc addBareDecl(c: var UntypedCtx, n: Cursor, k: SymKind, nameStart: int) =
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
        var ident = pool.strings[getIdent(newName)]
        makeLocalSym(c.c[], ident)
        let s = Sym(kind: k, name: pool.syms.getOrIncl(ident),
                    pos: c.c.dest.len)
        let delayed = DelayedSym(status: OkNew, lit: pool.strings.getOrIncl(ident), s: s, info: info)
        c.c[].addSym(delayed)
        newNameBuf = createTokenBuf(1)
        newNameBuf.add symdefToken(s.name, info)
        c.c.dest.replace(cursorAt(newNameBuf, 0), nameStart)
    else:
      c.c.dest.replace(newName, nameStart)
  else:
    var name = n
    let ident = getIdent(name)
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
    inc c.inNestedRoutine
    openScope(c)
    n[bodyPos] = semTemplBody(c, n[bodyPos])
    # close scope for locals
    closeScope(c)
    dec c.inNestedRoutine
    # close scope for parameters
    closeScope(c)

proc semTemplBody(c: var UntypedCtx; n: var Cursor)

proc semTemplBodySons(c: var UntypedCtx; n: var Cursor) =
  takeToken c.c[], n
  while n.kind != ParRi:
    semTemplBody c, n
  takeParRi c.c[], n

proc semTemplPragmas(c: var UntypedCtx; n: var Cursor) =
  takeTree c.c[], n

proc semTemplType(c: var UntypedCtx; n: var Cursor) =
  raiseAssert("unimplemented")

proc semTemplTypeDecl(c: var UntypedCtx; n: var Cursor) =
  raiseAssert("unimplemented")

proc semTemplLocal(c: var UntypedCtx; n: var Cursor; k: SymKind) =
  let orig = n
  takeToken c.c[], n
  let nameStart = c.c.dest.len
  takeTree c.c[], n # name
  addLocalDecl(c, orig, k, nameStart)
  takeTree c.c[], n # exported
  semTemplPragmas c, n # pragmas
  semTemplType c, n # type
  semTemplBody c, n # value
  takeParRi c.c[], n

proc semTemplRoutineDecl(c: var UntypedCtx; n: var Cursor; k: SymKind) =
  raiseAssert("unimplemented")

proc semTemplBody(c: var UntypedCtx; n: var Cursor) =
  case n.kind
  of Ident:
    if isInjected(c, n.litId):
      # skUnknown case for generics
      c.c.dest.add n
      return
    let start = c.c.dest.len
    let count = buildSymChoice(c.c[], n.litId, n.info, FindOverloads)
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
        c.c.dest.shrink start
        c.c.dest.add symToken(firstSym, n.info)
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
        when false:
          if s.kind in {skVar, skLet, skConst}:
            discard qualifiedLookUp(c.c, n, {checkAmbiguity, checkModule})
          result = semTemplSymbol(c, n, s, c.noGenSym > 0, c.c.isAmbiguous)
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
            openScope c
            semTemplBody c, n
            semTemplBody c, n
            closeScope c
          of ElseU:
            openScope c
            semTemplBody c, n
            closeScope c
          else: raiseAssert("illformed AST")
        takeParRi c.c[], n
      of WhileS:
        takeToken c.c[], n
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
            semTemplBody c, n
            openScope c
            semTemplBody c, n
            closeScope c
          of ElifU:
            openScope c
            semTemplBody c, n
            semTemplBody c, n
            closeScope c
          of ElseU:
            openScope c
            semTemplBody c, n
            closeScope c
          else: raiseAssert("illformed AST")
        closeScope c
        takeParRi c.c[], n
      of ForS:
        takeToken c.c[], n
        openScope c
        case n.substructureKind
        of UnpackFlatU, UnpackTupU:
          semTemplBodySons c, n
        else: raiseAssert("illformed AST")
        semTemplBody c, n
        openScope c
        semTemplBody c, n
        closeScope c
        closeScope c
        takeParRi c.c[], n
      of BlockS:
        let orig = n
        takeToken c.c[], n
        openScope c
        if n.kind == DotToken:
          takeToken c.c[], n
        else:
          let nameStart = c.c.dest.len
          takeTree c.c[], n
          addBareDecl(c, orig, BlockY, nameStart)
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
          semTemplBodySons c, n
      of NoStmt:
        # XXX type expressions
        discard
      else:
        semTemplBodySons c, n
    of AtX:
      when false:
        # if a[b] is nested inside a typed expression, don't convert it
        # back to `[]`(a, b), prepareOperand will not typecheck it again
        # and so `[]` will not be resolved
        # checking if a[b] is typed should be enough to cover this case
        result = newNodeI(nkCall, n.info)
        result.add newIdentNode(getIdent(c.c.cache, "[]"), n.info)
        for i in 0..<n.len: result.add(n[i])
        result = semTemplBodySons(c, result)
      else:
        semTemplBodySons c, n
    of DotX:
      # for now don't handle qualified symbols here
      takeToken c.c[], n
      semTemplBody c, n
      inc c.noGenSym
      semTemplBody c, n
      dec c.noGenSym
      takeParRi c.c[], n
    of QuotedX:
      var n2 = n
      let ident = getIdent(n2)
      if isDeclared(c.c[], ident):
        # consider identifier
        var identBuf = createTokenBuf(4)
        identBuf.add identToken(ident, n.info)
        var identRead = beginRead(identBuf)
        let start = c.c.dest.len
        semTemplBody c, identRead
        if c.c.dest[start].kind == Ident:
          c.c.dest.shrink start
          takeTree c.c.dest, n
        else:
          skip n
      else:
        semTemplBodySons c, n
    else:
      semTemplBodySons c, n
