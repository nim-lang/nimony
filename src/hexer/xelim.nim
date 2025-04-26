#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Eliminate eXpressions in complex situations. In other words turns
## `let x = if cond: 3 else: 4` into
## `let tmp; if cond: tmp = 3 else: temp = 4; let x = tmp`

import std / [assertions]
include nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof]

proc isComplex(n: Cursor): bool =
  var nested = 0
  var n = n
  while true:
    case n.kind
    of IntLit, UIntLit, FloatLit, StringLit, CharLit, UnknownToken, EofToken, Ident, Symbol, SymbolDef, DotToken:
      inc n
    of ParLe:
      if n.stmtKind in {IfS, CaseS, WhileS, AsgnS, LetS, VarS, CursorS, StmtsS, ResultS, GletS, TletS, GvarS, TvarS}:
        return true
      elif n.exprKind == ExprX:
        inc n
        let inner = n
        skip n
        if n.kind == ParRi:
          # ExprX with exactly one son might be harmless:
          if isComplex(inner):
            return true
        else:
          # More than one son is always complex:
          return true
        inc nested
      else:
        inc n
        inc nested
    of ParRi:
      inc n
      dec nested
    if nested == 0: break
  return false

type
  Mode = enum
    IsEmpty, IsAppend, IsIgnored
  Target = object
    m: Mode
    t: TokenBuf
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target)
proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)

proc declareTemp(c: var Context; dest: var TokenBuf; n: Cursor): SymId =
  let info = n.info
  let typ = getType(c.typeCache, n)
  let s = "`x." & $c.counter & "." & c.thisModuleSuffix
  inc c.counter
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, typ # type
    dest.addDotToken() # value

proc declareTempBool(c: var Context; dest: var TokenBuf; info: PackedLineInfo): SymId =
  let s = "`x." & $c.counter & "." & c.thisModuleSuffix
  inc c.counter
  result = pool.syms.getOrIncl(s)
  copyIntoKind dest, VarS, info:
    dest.addSymDef result, info
    dest.addDotToken() # export, pragmas
    dest.addDotToken()
    copyTree dest, c.typeCache.builtins.boolType # type
    dest.addDotToken() # value

proc add(dest: var TokenBuf; tar: Target) =
  dest.copyTree tar.t

proc trExprInto(c: var Context; dest: var TokenBuf; n: var Cursor; v: SymId) =
  var tar = Target(m: IsEmpty)
  trExpr c, dest, n, tar

  let info = n.info
  copyIntoKind dest, AsgnS, info:
    dest.addSymUse v, info
    dest.add tar

proc skipParRi(n: var Cursor) =
  if n.kind == ParRi:
    inc n
  else:
    error "expected ')', but got: ", n

proc trOr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if isComplex(n):
    # `x or y`  <=> `if x: true else: y` <=> `if x: tmp = true else: tmp = y`
    let info = n.info
    var tmp = declareTempBool(c, dest, info)
    inc n

    var aa = Target(m: IsEmpty)
    trExpr c, dest, n, aa
    copyIntoKind dest, IfS, info:
      copyIntoKind dest, ElifU, info:
        dest.add aa                # if x
        copyIntoKind dest, StmtsS, info:
          copyIntoKind dest, AsgnS, info: # tmp = true
            dest.addSymUse tmp, info
            copyIntoKind dest, TrueX, info: discard
      copyIntoKind dest, ElseU, info:
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp # tmp = y
    tar.t.addSymUse tmp, info
    skipParRi n
  else:
    copyInto tar.t, n:
      trExpr c, dest, n, tar
      trExpr c, dest, n, tar

proc trAnd(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  if isComplex(n):
    # `x and y` <=> `if x: y else: false` <=> `if x: tmp = y else: tmp = false`
    let info = n.info
    var tmp = declareTempBool(c, dest, info)
    inc n

    var aa = Target(m: IsEmpty)
    trExpr c, dest, n, aa
    copyIntoKind dest, IfS, info:
      copyIntoKind dest, ElifU, info:
        dest.add aa                # if x
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp # tmp = y
      copyIntoKind dest, ElseU, info:
        copyIntoKind dest, StmtsS, info:
          # tmp = false
          copyIntoKind dest, AsgnS, info:
            dest.addSymUse tmp, info
            copyIntoKind dest, FalseX, info: discard
    tar.t.addSymUse tmp, info
    skipParRi n
  else:
    copyInto tar.t, n:
      trExpr c, dest, n, tar
      trExpr c, dest, n, tar

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # if cond: a elif condB: b else: c
  # -->
  # if cond: a else: (if condB: b else: c)
  let info = n.info
  let head = n
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  var toClose = 0
  var ifs = 0
  inc n
  while n.kind != ParRi:
    if ifs >= 1:
      dest.addParLe ElseU, info
      dest.addParLe StmtsS, info
      inc toClose, 2

    let info = n.info
    case n.substructureKind
    of ElifU:
      var t0 = Target(m: IsEmpty)
      inc n
      trExpr c, dest, n, t0

      dest.add head
      inc toClose
      inc ifs

      copyIntoKind dest, ElifU, info:
        dest.add t0
        #copyIntoKind dest, StmtsS, info:
        if tar.m != IsIgnored:
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
      skipParRi n
    of ElseU:
      inc n
      if tar.m != IsIgnored:
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp
      else:
        trStmt c, dest, n
      skipParRi n
    else:
      # Bug: just copy the thing around
      takeTree dest, n
  skipParRi n

  while toClose > 0:
    dest.addParRi()
    dec toClose

  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trCase(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  var t0 = Target(m: IsEmpty)
  inc n
  trExpr c, dest, n, t0
  dest.addParLe CaseS, info
  dest.add t0
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      copyInto(dest, n):
        takeTree dest, n # choices
        if tar.m != IsIgnored:
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
    of ElseU:
      copyInto(dest, n):
        if tar.m != IsIgnored:
          copyIntoKind dest, StmtsS, info:
            trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
    else:
      # Bug: just copy the thing around
      takeTree dest, n
  takeParRi dest, n
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  copyInto(dest, n):
    if tar.m != IsIgnored:
      copyIntoKind dest, StmtsS, info:
        trExprInto c, dest, n, tmp
    else:
      trStmt c, dest, n

    while n.kind != ParRi:
      case n.substructureKind
      of ExceptU:
        copyInto(dest, n):
          takeTree dest, n # declarations
          if tar.m != IsIgnored:
            copyIntoKind dest, StmtsS, info:
              trExprInto c, dest, n, tmp
          else:
            trStmt c, dest, n
      of FinU:
        # The `finally` section never produces a value!
        copyInto(dest, n):
          trStmt c, dest, n
      else:
        # Bug: just copy the thing around
        takeTree dest, n
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trWhile(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.copyInto n:
    if isComplex(n):
      dest.copyIntoKind TrueX, info: discard
      copyIntoKind dest, StmtsS, info:
        var tar = Target(m: IsEmpty)
        trExpr c, dest, n, tar
        dest.copyIntoKind IfS, info:
          dest.copyIntoKind ElifU, info:
            dest.add tar
            trStmt c, dest, n
          dest.copyIntoKind ElseU, info:
            copyIntoKind dest, StmtsS, info:
              dest.copyIntoKind BreakS, info:
                dest.addDotToken()
    else:
      var tar = Target(m: IsEmpty)
      trExpr c, dest, n, tar
      dest.add tar
      trStmt c, dest, n

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var tmp = createTokenBuf(30)
  let kind = n.symKind
  copyInto tmp, n:
    let name = n.symId
    takeTree tmp, n # name
    takeTree tmp, n # export marker
    takeTree tmp, n # pragmas
    c.typeCache.registerLocal(name, kind, n)
    takeTree tmp, n # type
    var v = Target(m: IsEmpty)
    trExpr c, dest, n, v
    tmp.add v
  dest.add tmp

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  copyInto dest, n:
    let isConcrete = takeRoutineHeader(c.typeCache, dest, n)
    if isConcrete:
      trStmt c, dest, n
    else:
      takeTree dest, n
  c.typeCache.closeScope()

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  copyInto(dest, n):
    takeTree dest, n # label or DotToken
    if tar.m != IsIgnored:
      copyIntoKind dest, StmtsS, n.info:
        trExprInto c, dest, n, tmp
    else:
      trStmt c, dest, n


  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, n.info

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    assert n.kind != ParRi
    takeTree dest, n
  of IfS, WhenS:
    var tar = Target(m: IsIgnored)
    trIf c, dest, n, tar
  of CaseS:
    var tar = Target(m: IsIgnored)
    trCase c, dest, n, tar
  of TryS:
    var tar = Target(m: IsIgnored)
    trTry c, dest, n, tar

  of RetS, DiscardS, RaiseS, YldS:
    var tar = Target(m: IsEmpty)
    let head = n
    inc n
    trExpr c, dest, n, tar
    dest.add head
    dest.add tar
    dest.addParRi()
    skipParRi n

  of WhileS:
    trWhile c, dest, n
  of AsgnS, CallS, CmdS, InclS, ExclS, AsmS, DeferS:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    tar.t.copyInto n:
      while n.kind != ParRi:
        trExpr c, dest, n, tar
    dest.add tar
  of LocalDecls:
    trLocal c, dest, n
  of ProcS, FuncS, MacroS, MethodS, ConverterS:
    trProc c, dest, n
  of BlockS:
    var tar = Target(m: IsIgnored)
    trBlock c, dest, n, tar
  of IteratorS, TemplateS, TypeS, EmitS, BreakS, ContinueS,
     ForS, IncludeS, ImportS, FromimportS, ImportExceptS,
     ExportS, CommentS, AssumeS, AssertS,
     PragmasS, ImportasS, ExportexceptS, BindS, MixinS, UsingS:
    takeTree dest, n
  of ScopeS, StaticstmtS:
    c.typeCache.openScope()
    copyInto(dest, n):
      while n.kind != ParRi:
        trStmt c, dest, n
    c.typeCache.closeScope()
  of StmtsS, UnpackDeclS:
    copyInto(dest, n):
      while n.kind != ParRi:
        trStmt c, dest, n

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # can have the dangerous `Expr` node which is the whole
  # reason for xelim's existence.
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree tar.t, n
  of ParLe:
    case n.exprKind
    of ExprX:
      inc n
      while n.kind != ParRi:
        if not isLastSon(n):
          trStmt c, dest, n
        else:
          trExpr c, dest, n, tar
      skipParRi n
    of AndX:
      trAnd c, dest, n, tar
    of OrX:
      trOr c, dest, n, tar
    else:
      case n.stmtKind
      of IfS:
        trIf c, dest, n, tar
      of CaseS:
        trCase c, dest, n, tar
      of TryS:
        trTry c, dest, n, tar
      of BlockS:
        trBlock c, dest, n, tar
      else:
        copyInto tar.t, n:
          while n.kind != ParRi:
            trExpr c, dest, n, tar
  of ParRi:
    raiseAssert "unexpected ')' inside"

proc lowerExprs*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix)
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  assert n.stmtKind == StmtsS, $n.kind
  result.add n
  inc n
  while n.kind != ParRi:
    trStmt c, result, n
  result.addParRi()
  c.typeCache.closeScope()
  #echo "PRODUCED: ", result.toString(false)

when isMainModule:
  let n = setupProgram("debug.txt", "debug.out")
  let r = lowerExprs(n, "main")
  echo r.toString(false)
