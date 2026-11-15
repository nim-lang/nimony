#
#
#           Nimony contract prover: aliasing
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Which variables the contract prover may keep facts about, and which
## writes can change a variable without naming it (doc/language.md,
## "Aliasing").
##
## A variable is *owned* when nothing but a write that names it can change
## it: a local, `result`, a parameter that is not `var`, `out` or `lent`.
## Every other variable is *shared*: a global, a `var`/`out`/`lent`
## parameter, a local whose address is taken, a local a nested routine names.
## What is known about a shared variable lasts until the next *barrier*:
##
## - a call of a routine that is not a `func` (a `func` writes nothing but
##   the variables it is handed as `var`, see doc/language.md, "Func");
## - a write through a `ptr`, a `ref` or a view;
## - a write to a global or a `var` parameter, which may be another name
##   of a different shared variable;
## - the destruction of a local whose `=destroy` is not a `func`.
##
## A barrier keeps what is known about the variables it writes by name: the
## write itself accounts for them.

import std / [assertions, tables, hashes, sets]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import nimony_model, decls, programs, typenav, typeprops

type
  WrittenRoot* = object
    sym*: SymId     ## the variable the write is to; `NoSymId` if the written
                    ## location is part of no variable (it lies behind a
                    ## `ptr`, a `ref` or a view)
    barrier*: bool  ## the write can change a variable without naming it

proc isSharedStorage*(tc: var TypeCache; s: SymId): bool =
  ## Can a write to the variable `s` be seen under another name? A global and
  ## a `var`, `out` or `lent` parameter can. A local cannot: two named
  ## variables never share storage, and an address taken of a local only
  ## makes a pointer, which no fact is about.
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
  ## nil cursor for a routine value or a symbol that does not load.
  result = default(Cursor)
  if fn.isSymbol and (getLocalInfo(tc, fn.symId).kind != NoSym or
      tryLoadSym(fn.symId).status == LacksNothing):
    tt = getType(tc, fn)
    if tt.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS}:
      let p = skipProcTypeToParams(tt)
      if p.isParamsTag: result = p

proc isBarrierCallee*(tc: var TypeCache; fn: Cursor): bool =
  ## Is a call of `fn` a barrier? Only a `func` is not: everything else may
  ## write a global or what lies behind a `ref`. A method runs an override
  ## that is not known here, and a routine value any routine.
  var tt = default(Cursor)
  let params = routineParams(tc, fn, tt)
  if cursorIsNil(params) or tt.stmtKind == MethodS:
    result = true
  else:
    var pragmas = params
    skip pragmas # params
    skip pragmas # return type
    result = calleeEffect(tt, pragmas) == HasSideEffect

proc resultOutsideFirstArg*(tc: var TypeCache; call: Cursor): bool =
  ## Does the call take its first argument by value? Then a `var T` it returns
  ## is no part of that argument's own storage, which is read-only to the
  ## callee: it lies behind a pointer the argument holds (`s[i]` for a `seq`).
  var n = call
  inc n
  var tt = default(Cursor)
  var params = routineParams(tc, n, tt)
  result = false
  if not cursorIsNil(params) and tt.stmtKind != MethodS:
    params = sub(params)
    if params.hasMore:
      result = takeLocal(params, SkipFinalParRi).typ.typeKind notin {MutT, OutT, LentT}

proc writtenRootOf*(tc: var TypeCache; loc: Cursor;
                    inits: Table[SymId, Cursor]): WrittenRoot =
  ## The variable a write to the location `loc` is to. A field, a tuple field
  ## and an array element are part of their variable. So is what a routine
  ## returns as `var T` from its first argument: an element of a `seq` or a
  ## `string`. An inline temp stands for the location it was bound to
  ## (`inits`). A location behind a `ptr`, a `ref` or a view is part of no
  ## variable; a write to it is a barrier.
  var n = loc
  var guard = 0
  result = WrittenRoot(sym: NoSymId, barrier: true)
  while guard < 64:
    inc guard
    if n.kind == Symbol:
      if n.symId in inits:
        n = inits.getOrQuit(n.symId)
        continue
      return WrittenRoot(sym: n.symId, barrier: isSharedStorage(tc, n.symId))
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
      # an override may return a location from anywhere
      if cursorIsNil(params) or tt.stmtKind == MethodS: return
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

proc isRvalueBase(loc: Cursor): bool =
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

proc isBarrierCall*(tc: var TypeCache; call: Cursor; inits: Table[SymId, Cursor];
                    named: var seq[SymId]): bool =
  ## Is the call `call` a barrier, by its callee or by what it is handed as
  ## `var`? `named` receives the variables it writes by name.
  var n = call
  inc n
  var tt = default(Cursor)
  var params = routineParams(tc, n, tt)
  result = isBarrierCallee(tc, n)
  if cursorIsNil(params) and n.isSymbol and getLocalInfo(tc, n.symId).kind != NoSym:
    # a routine value: its type still tells which arguments are `var`
    let p = skipProcTypeToParams(getType(tc, n))
    if p.isParamsTag: params = p
  let known = not cursorIsNil(params)
  if known: params = sub(params)
  skip n # fn
  while n.hasMore:
    var isVar = n.exprKind == HaddrX
    if known and params.hasMore:
      let prev = params
      let t = takeLocal(params, SkipFinalParRi).typ
      if t.typeKind in {MutT, OutT}: isVar = true
      elif t.typeKind == VarargsT: params = prev
    if isVar and not isRvalueBase(n):
      let w = writtenRootOf(tc, n, inits)
      if w.barrier: result = true
      if w.sym != NoSymId: named.add w.sym
    skip n

# --- The variables whose address is taken or that a nested routine names ---

proc addrRoot(tc: var TypeCache; n: Cursor; inits: Table[SymId, Cursor]): SymId =
  ## The variable whose own storage the operand `n` of an `addr` lies in. A
  ## call is followed into its first argument, an inline temp into what it
  ## stands for; a dereference ends the walk, and so does a call that takes
  ## its first argument by value: what it returns lies in the buffer that
  ## argument points to (`addr s[0]` for a `seq`).
  result = NoSymId
  var r = n
  var guard = 0
  while guard < 64:
    inc guard
    if r.isSymbol:
      if r.symId in inits:
        r = inits.getOrQuit(r.symId)
        continue
      return r.symId
    if not r.isTagLit: return NoSymId
    case r.exprKind
    of DotX, TupatX, ArratX, AtX, AddrX, HaddrX, HderefX:
      r = r.childCursor
    of ConvKinds:
      r = r.childCursor
      skip r # type
    of BaseobjX:
      r = r.childCursor
      skip r # type
      skip r # intlit
    of CallKinds:
      if resultOutsideFirstArg(tc, r): return NoSymId
      r = r.childCursor
      skip r # fn
      if not r.hasMore: return NoSymId
    else:
      return NoSymId

proc collectDefs(n: var Cursor; defs: var HashSet[SymId]) =
  if n.kind == SymbolDef:
    defs.incl n.symId
    inc n
  elif n.isTagLit:
    n.into:
      while n.hasMore: collectDefs(n, defs)
  else:
    inc n

proc markUses(n: var Cursor; defs: HashSet[SymId]; dest: var HashSet[SymId]) =
  if n.kind == Symbol:
    # a local name the routine does not declare is a local of an enclosing one
    if n.symId notin defs and isLocalName(pool.syms[n.symId]):
      dest.incl n.symId
    inc n
  elif n.isTagLit:
    n.into:
      while n.hasMore: markUses(n, defs, dest)
  else:
    inc n

proc collectAliased*(tc: var TypeCache; n: var Cursor;
                     inits: var Table[SymId, Cursor]; dest: var HashSet[SymId]) =
  ## Pre-pass over a routine body: the locals that are shared because their
  ## address is taken anywhere in it (`addr x`, `unsafeAddr x`) or because a
  ## nested routine names them. Flow-insensitive: a pointer taken late in a
  ## loop counts for the whole body. `inits` receives what the inline temps
  ## stand for.
  if not n.isTagLit or n.substructureKind == PragmasU:
    skip n
  elif n.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS}:
    var defs = initHashSet[SymId]()
    var r = n
    collectDefs(r, defs)
    r = n
    markUses(r, defs, dest)
    skip n
  else:
    if n.exprKind == AddrX:
      let r = addrRoot(tc, n.childCursor, inits)
      if r != NoSymId: dest.incl r
    elif n.symKind in {LetY, CursorY, VarY}:
      let local = asLocal(n)
      if local.name.kind == SymbolDef and hasPragma(local.pragmas, InlineP):
        inits[local.name.symId] = local.val
    n.into:
      while n.hasMore: collectAliased(tc, n, inits, dest)

proc collectAliasedToplevel*(tc: var TypeCache; n: var Cursor;
                             inits: var Table[SymId, Cursor];
                             dest: var HashSet[SymId]) =
  ## `collectAliased` for the module's own statements; its routines have
  ## their own.
  if n.stmtKind in {StmtsS, PragmaxS}:
    n.into:
      while n.hasMore: collectAliasedToplevel(tc, n, inits, dest)
  elif n.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS,
                      TemplateS, TypeS}:
    skip n
  else:
    collectAliased(tc, n, inits, dest)
