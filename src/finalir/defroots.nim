#
#
#           Nimony Final IR: definitions and barriers
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Which variable a write defines, and whether the write is a *barrier*
## (doc/language.md, "Aliasing"). `finalir.nim` numbers the definitions of a
## variable with it and the contract prover counts them the same way, so the
## two must share this code: see doc/internals/multi_join_ir.md, "Versions".

import std / [assertions]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, typeprops]

type
  DefRoot* = object
    sym*: SymId     ## the variable the write defines; `NoSymId` if the written
                    ## location is not part of a named variable's value
    barrier*: bool  ## the write can change a location without naming it

proc isSharedVar*(tc: var TypeCache; s: SymId): bool =
  ## Can a write to the variable `s` be seen under another name? A global and
  ## a `var`, `out` or `lent` parameter can. A local cannot: two named
  ## variables never share storage.
  let li = getLocalInfo(tc, s)
  case li.kind
  of NoSym:
    # not declared in the scopes we are in: another module's global, or a
    # local of a body that is only being scanned ahead
    result = not isLocalName(pool.syms[s])
  of GvarY, TvarY, GletY, TletY:
    result = true
  of ParamY:
    result = li.typ.typeKind in {MutT, OutT, LentT}
  else:
    result = false

proc routineParams(tc: var TypeCache; fn: Cursor; tt: var Cursor): Cursor =
  ## The `(params …)` of the routine the callee `fn` names, `tt` its type; a
  ## nil cursor for a routine value, a method or a symbol that does not load.
  result = default(Cursor)
  if fn.isSymbol and (getLocalInfo(tc, fn.symId).kind != NoSym or
      tryLoadSym(fn.symId).status == LacksNothing):
    tt = getType(tc, fn)
    if tt.stmtKind in {ProcS, FuncS, IteratorS, ConverterS}:
      let p = skipProcTypeToParams(tt)
      if p.isParamsTag: result = p

proc isBarrierCallee*(tc: var TypeCache; fn: Cursor): bool =
  ## Is a call of `fn` a barrier? A `func` writes nothing but the variables it
  ## is handed as `var` (doc/language.md, "Func"); everything else may write a
  ## global or what lies behind a `ref`.
  var tt = default(Cursor)
  let params = routineParams(tc, fn, tt)
  if cursorIsNil(params):
    result = true
  else:
    var pragmas = params
    skip pragmas # params
    skip pragmas # return type
    result = calleeEffect(tt, pragmas) == HasSideEffect

proc defRoot*(tc: var TypeCache; loc: Cursor): DefRoot =
  ## The variable a write to the location `loc` defines. A field, a tuple
  ## field and an array element are part of their variable. So is what a
  ## routine returns as `var T` from its first argument: an element of a `seq`
  ## or a `string`. A location behind a `ptr`, a `ref` or a view is part of no
  ## variable; a write to it is a barrier.
  var n = loc
  var guard = 0
  result = DefRoot(sym: NoSymId, barrier: true)
  while guard < 64:
    inc guard
    if n.kind == Symbol:
      return DefRoot(sym: n.symId, barrier: isSharedVar(tc, n.symId))
    if not n.isTagLit: return
    case n.exprKind
    of DotX, TupatX, ArratX, AtX, HaddrX, HderefX:
      inc n
    of ConvKinds:
      inc n
      skip n # type
    of BaseobjX:
      inc n
      skip n # type
      skip n # intlit
    of CallKinds:
      inc n
      var tt = default(Cursor)
      var params = routineParams(tc, n, tt)
      if cursorIsNil(params): return
      params = sub(params)
      if not params.hasMore: return
      var t = takeLocal(params, SkipFinalParRi).typ
      if t.typeKind notin {MutT, OutT}:
        # by value: the result lies in what the argument owns, unless the
        # argument is itself only a pointer or a view
        if t.typeKind == SinkT: t = sub(t)
        if t.typeKind in {PtrT, RefT, PointerT, CstringT, LentT} or isViewType(t):
          return
      skip n # fn
      if not n.hasMore: return
    else:
      return

proc isRvalueBase*(loc: Cursor): bool =
  ## Is the location `loc` rooted in no variable at all, but in a temporary
  ## value (a constructor, a literal)? A write to it is seen by nobody.
  var n = loc
  var guard = 0
  result = false
  while guard < 64:
    inc guard
    if n.kind == Symbol: return false
    if not n.isTagLit: return true
    case n.exprKind
    of DotX, TupatX, ArratX, AtX, HaddrX, HderefX, DerefX, PatX, DdotX, AddrX:
      inc n
    of ConvKinds:
      inc n
      skip n
    of BaseobjX:
      inc n
      skip n
      skip n
    of CallKinds:
      return false
    else:
      return true

proc callDefs*(tc: var TypeCache; call: Cursor; defs: var seq[DefRoot];
               barrier: var bool) =
  ## What the call `call` writes: one entry per argument that is bound to a
  ## `var` or `out` parameter, in argument order. A parameter forwarded to
  ## another one carries no `(haddr …)`, so the formal decides. `barrier`:
  ## the callee is none of the routines that write nothing else.
  var n = call
  inc n
  var tt = default(Cursor)
  var params = routineParams(tc, n, tt)
  if cursorIsNil(params):
    barrier = true
    if n.isSymbol and getLocalInfo(tc, n.symId).kind != NoSym:
      # a routine value: its type still tells which arguments are `var`
      let p = skipProcTypeToParams(getType(tc, n))
      if p.isParamsTag: params = p
  else:
    var pragmas = params
    skip pragmas # params
    skip pragmas # return type
    barrier = calleeEffect(tt, pragmas) == HasSideEffect
  let known = not cursorIsNil(params)
  if known: params = sub(params)
  skip n # fn
  while n.hasMore:
    var isVar = n.exprKind == HaddrX
    if known and params.hasMore:
      let t = takeLocal(params, SkipFinalParRi).typ
      if t.typeKind in {MutT, OutT}: isVar = true
    if isVar and not isRvalueBase(n):
      defs.add defRoot(tc, n)
    skip n
