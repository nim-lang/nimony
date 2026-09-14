#
#
#           Nimony Final IR Pass
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Contract propositions extracted as `(assume …)` facts for the Final IR.
##
## Two places state a fact the contract analysis may take for granted rather
## than prove: a `{.assume: ….}` statement, and the `.ensures` of the iterator a
## `for` loop runs, read about the loop variable. Both come down to the same
## job — take the proposition apart into its conjuncts and re-emit every
## comparison the fact engine can model, with parameters replaced by what
## stands for them — and that job lives here, apart from the lowering itself.

import std / [tables, assertions]
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs]
import finalir_model

proc emitContractOperand(dest: var TokenBuf; n: Cursor; subst: Table[SymId, TokenBuf];
                         info: NifLineInfo; depth: int): bool

proc emitTypedBinary(dest: var TokenBuf; n: Cursor; subst: Table[SymId, TokenBuf];
                     info: NifLineInfo; depth: int): bool =
  ## `(k T a b)` — an arithmetic operation or a comparison — with both operands
  ## re-emitted. All or nothing: an operand outside the grammar takes the whole
  ## node back out of `dest`.
  let mark = dest.len
  var r = n
  r = sub(r)
  dest.addParLe(n.exprKind, info)
  dest.addSubtree r # the type operand
  skip r
  result = emitContractOperand(dest, r, subst, info, depth)
  if result:
    skip r
    result = emitContractOperand(dest, r, subst, info, depth)
  if result:
    dest.addParRi()
  else:
    dest.shrink mark

proc emitContractOperand(dest: var TokenBuf; n: Cursor; subst: Table[SymId, TokenBuf];
                         info: NifLineInfo; depth: int): bool =
  ## Re-emit one side of a contract comparison with the parameters substituted.
  ## Rebuilt node by node rather than copied: a copy would have to reproduce the
  ## source tag ids, and those are pool-relative (an id past `TagMask` is even
  ## stored escaped), so a verbatim token copy across buffers is not a copy at
  ## all. The grammar accepted here is the one a proposition may use anyway —
  ## `doc/language.md`: constants, parameters and `result`, plus arithmetic.
  result = false
  if depth <= 4:
    case n.kind
    of IntLit:
      dest.addIntLit(n.intVal, info)
      result = true
    of UIntLit:
      dest.addUIntLit(n.uintVal, info)
      result = true
    of Symbol:
      if subst.hasKey(n.symId):
        dest.add subst.getOrQuit(n.symId)
      else:
        dest.addSymUse(n.symId, info)
      result = true
    of TagLit:
      case n.exprKind
      of AddX, SubX:
        result = emitTypedBinary(dest, n, subst, info, depth+1)
      of HconvX, ConvX:
        var r = n
        r = sub(r)
        skip r # the target type
        result = emitContractOperand(dest, r, subst, info, depth+1)
      else:
        discard
    else:
      discard

proc emitAssumes*(dest: var TokenBuf; cond: Cursor; subst: Table[SymId, TokenBuf];
                  info: NifLineInfo) =
  ## One `(assume …)` per conjunct: the fact engine models a single comparison,
  ## and `a and b` as one opaque condition would contribute nothing. A conjunct
  ## that does not fit the grammar is simply dropped — an assumption is a
  ## statement of what holds, not an obligation, so losing one costs precision
  ## and nothing else.
  var cond = cond
  while cond.exprKind == ExprX:
    # sem wraps the right operand of `and` in an `(expr …)`; the proposition is
    # its last son.
    cond = sub(cond)
    while cond.hasMore and not isLastSon(cond): skip cond
  case cond.exprKind
  of AndX:
    var r = cond
    r = sub(r)
    emitAssumes dest, r, subst, info
    skip r
    emitAssumes dest, r, subst, info
  of LeX, LtX, EqX:
    let mark = dest.len
    dest.addParLe(AssumeV, info)
    if emitTypedBinary(dest, cond, subst, info, 0):
      dest.addParRi() # close `assume`
    else:
      dest.shrink mark
  else:
    discard

proc singleLoopVar(vars: Cursor): SymId =
  ## The loop variable of a `for` that binds exactly one plain local, else
  ## `NoSymId`.
  result = NoSymId
  if vars.substructureKind == UnpackflatU:
    var v = vars
    v = sub(v)
    if v.hasMore and isLocal(v.symKind):
      let name = asLocal(v).name.symId
      skip v
      if not v.hasMore: result = name

proc iteratorCall(iter: Cursor; callExprs: var Table[SymId, TokenBuf]; call: var Cursor): bool =
  ## The iterator call of a `for`, looked up in `callExprs` when xelim hoisted it
  ## into a temp.
  call = iter
  if call.isTagLit and call.exprKind in {HderefX, HaddrX}: inc call
  if call.isSymbol and callExprs.hasKey(call.symId):
    call = beginRead(callExprs.getOrQuit(call.symId))
  result = call.isTagLit and call.exprKind in CallKinds

proc iteratorEnsures(call: Cursor; routine: var Routine; ens: var Cursor): bool =
  ## The `.ensures` of the routine `call` invokes.
  result = false
  var fn = call
  fn = sub(fn)
  if fn.isSymbol:
    let sym = tryLoadSym(fn.symId)
    if sym.status == LacksNothing and isRoutine(sym.decl.symKind):
      routine = asRoutine(sym.decl)
      if not cursorIsNil(routine.pragmas):
        ens = extractPragma(routine.pragmas, EnsuresP)
        result = not cursorIsNil(ens) and ens.exprKind == ExprX

proc bindResult(ens: Cursor; loopVar: SymId; info: NifLineInfo;
                subst: var Table[SymId, TokenBuf]): Cursor =
  ## `ensures` is stored as `(expr (result :res . . T .) <cond>)`, and the
  ## wrapper nests: the generic declaration is sem-checked once and its instance
  ## again, each pass adding a layer. Peel them all, binding every `result`
  ## declared on the way to the loop variable — only the innermost is named by
  ## the condition, but binding them all cannot go stale. Returns the condition.
  result = ens
  var peeling = true
  while peeling and result.exprKind == ExprX:
    var inner = result
    inner = sub(inner)
    if inner.hasMore:
      if inner.symKind == ResultY:
        var loopVarBuf = createTokenBuf(2)
        loopVarBuf.addSymUse(loopVar, info)
        subst[asLocal(inner).name.symId] = loopVarBuf
      while inner.hasMore and not isLastSon(inner): skip inner
      result = inner
    else:
      peeling = false

proc bindParams(params, call: Cursor; subst: var Table[SymId, TokenBuf]): bool =
  ## Every parameter stands for the argument written at the call site. False
  ## when the call leaves a defaulted parameter out.
  result = false
  if not cursorIsNil(params) and params.isParamsTag:
    var p = params
    p = sub(p)
    var arg = call
    arg = sub(arg)
    skip arg # the callee
    while p.hasMore and arg.hasMore:
      let param = takeLocal(p, SkipFinalParRi)
      var argBuf = createTokenBuf(8)
      argBuf.addSubtree arg
      subst[param.name.symId] = argBuf
      skip arg
    result = not p.hasMore

proc forRangeAssumes*(dest: var TokenBuf; forStmt: ForStmt;
                      callExprs: var Table[SymId, TokenBuf]; info: NifLineInfo) =
  ## Turn the iterator's `.ensures` into an assumption about the loop variable.
  ##
  ## An *inline* iterator is not inlined until hexer's `elimForLoops`, long after
  ## contract analysis has run, so the Final IR's `(loop …)` says nothing
  ## whatsoever about the loop variable — not even where it was declared. The
  ## iterator's `.ensures` does say something, and it is exactly what the body
  ## needs: `iterator ..<[T](a, b: T): T {.ensures: (a <= result and result < b).}`
  ## means every value the loop variable takes satisfies that. Without this the
  ## most ordinary contract in the language, `s[i]` under `for i in 0 ..< s.len`,
  ## could not be discharged at compile time.
  let loopVar = singleLoopVar(forStmt.vars)
  var call = default(Cursor)
  var routine = default(Routine)
  var ens = default(Cursor)
  if loopVar != NoSymId and iteratorCall(forStmt.iter, callExprs, call) and
     iteratorEnsures(call, routine, ens):
    var subst = initTable[SymId, TokenBuf]()
    let cond = bindResult(ens, loopVar, info, subst)
    if subst.len > 0 and bindParams(routine.params, call, subst):
      emitAssumes dest, cond, subst, info
