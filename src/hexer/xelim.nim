#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
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
      if n.stmtKind in {IfS, CaseS, WhileS, AsgnS, LetS, VarS, CursorS, StmtsS}:
        return true
      elif n.exprKind == ExprX:
        return true
      inc nested
    of ParRi:
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
      copyIntoKind dest, ElifS, info:
        dest.add aa                # if x
        copyIntoKind dest, StmtsS, info:
          copyIntoKind dest, AsgnS, info: # tmp = true
            dest.addSymUse tmp, info
            copyIntoKind dest, TrueX, info: discard
      copyIntoKind dest, ElseS, info:
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp # tmp = y
    tar.t.addSymUse tmp, info
    skipParRi n
  else:
    copyInto dest, n:
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
      copyIntoKind dest, ElifS, info:
        dest.add aa                # if x
        copyIntoKind dest, StmtsS, info:
          trExprInto c, dest, n, tmp # tmp = y
      copyIntoKind dest, ElseS, info:
        copyIntoKind dest, StmtsS, info:
          # tmp = false
          copyIntoKind dest, AsgnS, info:
            dest.addSymUse tmp, info
            copyIntoKind dest, FalseX, info: discard
    tar.t.addSymUse tmp, info
    skipParRi n
  else:
    copyInto dest, n:
      trExpr c, dest, n, tar
      trExpr c, dest, n, tar

proc trIf(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # if cond: a elif condB: b else: c
  # -->
  # if cond: a else: (if condB: b else: c)
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  var toClose = 0
  var ifs = 0
  inc n
  while n.kind != ParRi:
    if ifs >= 1:
      dest.addParLe ElseS, info
      dest.addParLe StmtsS, info
      inc toClose, 2

    let info = n.info
    case n.substructureKind
    of ElifS:
      var t0 = Target(m: IsEmpty)
      trExpr c, dest, n, t0

      dest.addParLe IfS, info
      inc toClose
      inc ifs

      copyIntoKind dest, ElifS, info:
        dest.add t0
        copyIntoKind dest, StmtsS, info:
          if tar.m != IsIgnored:
            trExprInto c, dest, n, tmp
          else:
            trStmt c, dest, n
      skipParRi n
    of ElseS:
      inc n
      if tar.m != IsIgnored:
        trExprInto c, dest, n, tmp
      else:
        trStmt c, dest, n
      skipParRi n
    else:
      # Bug: just copy the thing around
      takeTree dest, n

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
    of OfS:
      copyInto(dest, n):
        takeTree dest, n # choices
        if tar.m != IsIgnored:
          trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
    of ElseS:
      copyInto(dest, n):
        if tar.m != IsIgnored:
          trExprInto c, dest, n, tmp
        else:
          trStmt c, dest, n
    else:
      # Bug: just copy the thing around
      takeTree dest, n
  skipParRi n
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  let info = n.info
  var tmp = SymId(0)

  if tar.m != IsIgnored:
    tmp = declareTemp(c, dest, n)

  copyInto(dest, n):
    if tar.m != IsIgnored:
      trExprInto c, dest, n, tmp
    else:
      trStmt c, dest, n

    while n.kind != ParRi:
      case n.substructureKind
      of ExceptS:
        copyInto(dest, n):
          for e in sons(t, ch):
            if isLastSon(t, ch, e):
              if tar.m != IsIgnored:
                trExprInto c, dest, t, e, tmp
              else:
                trStmt c, dest, t, e
            else:
              copyTree dest, n, e
      of FinallyS:
        # The `finally` section never produces a value!
        copyInto(dest, n):
          trStmt c, dest, n
      else:
        # Bug: just copy the thing around
        takeTree dest, n
  if tar.m != IsIgnored:
    tar.t.addSymUse tmp, info

proc trWhile(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let (cond, body) = sons2(t, n)
  let info = n.info
  dest.copyIntoFrom t, n:
    if isComplex(t, cond):
      dest.copyInto TrueX, info
      copyInto dest, StmtsX, info:
        var tar = Target(m: IsEmpty)
        trExpr c, dest, t, cond, tar
        dest.copyInto IfX, info:
          dest.copyInto ElifX, info:
            dest.copy tar
            trStmt c, dest, t, body
          dest.copyInto ElseX, info:
            dest.copyInto BreakX, info
    else:
      var tar = Target(m: IsEmpty)
      trExpr c, dest, t, cond, tar
      dest.copy tar
      trStmt c, dest, t, body

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    takeTree dest, n
  of IfS:
    var tar = Target(m: IsIgnored)
    trIf c, dest, n, tar
  of CaseS:
    var tar = Target(m: IsIgnored)
    trCase c, dest, n, tar
  of TryS:
    var tar = Target(m: IsIgnored)
    trTry c, dest, n, tar

  of RetS, DiscardS, RaiseS, YieldS:
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
  of AsgnX, CallX:
    # IMPORTANT: Stores into `tar` helper!
    var tar = Target(m: IsAppend)
    tar.t.copyIntoFrom t, n:
      for ch in sons(t, n):
        trExpr c, dest, t, ch, tar
    dest.copy tar
  of LetX, VarX, CursorX, ConstX:
    var tar = Target(m: IsAppend)
    for ch in wsons(tar.t, n):
      if isLastSon(t, n, ch):
        var v = Target(m: IsEmpty)
        trExpr c, dest, t, ch, v
        tar.t.copy v
      else:
        tar.t.copyTree t, ch
    dest.copy tar

  of ProcX:
    for ch in wsons(dest, n):
      if isLastSon(t, n, ch):
        trStmt c, dest, t, ch
      else:
        dest.copyTree t, ch
  of StmtsS:
    copyInto(dest, n):
      while n.kind != ParRi:
        trStmt c, dest, n

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor; tar: var Target) =
  # can have the dangerous `Expr` node which is the whole
  # reason for xelim's existence.
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree tar.t, n
  of ExprX:
    trStmt c, dest, n
    trExpr c, dest, n, tar
  of IfX:
    trIf c, dest, n, tar
  of CaseX:
    trCase c, dest, n, tar
  of TryX:
    trTry c, dest, n, tar
  of AndX:
    trAnd c, dest, n, tar
  of OrX:
    trOr c, dest, n, tar
  else:
    for ch in wsons(tar.t, n):
      trExpr c, dest, t, ch, tar

proc lowerExprs*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix)
  result = createTokenBuf(300)
  var n = n
  trStmt c, result, n

