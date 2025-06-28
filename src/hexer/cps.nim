#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
We need to transform:

  iterator it(x: int): int {.closure.} =
    for i in 0..<x:
      yield i

into:

  type
    ItCoroutine* = object of CoroutineBase
      x, i: int
      dest: ptr int

  proc itNext(coro: ptr ItCoroutine): Continuation =
    coro.i += 1
    result = Continuation(fn: itStart, env: coro)

  proc itStart(coro: ptr ItCoroutine): Continuation =
    if coro.i < coro.x:
      result = Continuation(fn: itNext, env: coro)
    else:
      result = Continuation(fn: nil, env: nil)

  proc createItCoroutine(this: ptr ItCoroutine; x: int; dest: ptr int; caller: Continuation): Continuation =
    this = ItCoroutine(x: x, i: 0, caller: caller, dest: dest)
    result = Continuation(fn: itStart, env: this)


1. Compile the AST/NIF to a CFG with goto instructions (src/nimony/controlflow does that already). Pay special
   attention that no implicit fall-through remains for code like (if cond: a); b().
2. Optimize the CFG so that unused labels are not generated.
3. Turn the target **labels** of the gotos into functions.
4. Move these inner functions to the top level ("lambda lifting"), compute the required environment objects.
5. The return value `result` remains and is passed out of the iterator like it is done for regular procs.
6. Turn `goto label` into function calls. Turn `yield` into `env.cont = nextState; return`.

Usage of the closure iterator in a for loop than becomes an easy trampoline:

  for forLoopVar in it(10): echo forLoopVar

Becomes:

  const StopContinuation = Continuation(fn: nil, env: nil)

  var forLoopVar: int
  var itCoroutine: ItCoroutine
  var it = createItCoroutine(addr itCoroutine, 10, addr forLoopVar, StopContinuation)
  while it.fn != nil:
    echo forLoopVar
    it = it.fn(it.env)

]##

import std / [assertions, sets, tables]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints,
  builtintypes, langmodes, renderer, reporters, controlflow]
import hexer_context

const
  RootObjName = "CoroutineBase.0." & SystemModuleSuffix
  EnvParamName = "`ep.0"

type
  EnvField = object
    objType: SymId
    field: SymId
    typ: Cursor

  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    localToEnv: Table[SymId, EnvField]

proc coroTypeForProc(c: var Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".coro." & c.thisModuleSuffix)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.kind != ParRi:
      tr(c, dest, n)

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # XXX To implement
  trSons(c, dest, n)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc treIteratorBody(c: var Context; dest: var TokenBuf; init: TokenBuf; iter: Cursor; sym: SymId) =
  var cf = toControlflow(iter, keepReturns = true)
  var n = beginRead(cf)


proc trCoroutine(c: var Context; dest: var TokenBuf; n: var Cursor; kind: SymKind) =
  var init = createTokenBuf(10)
  let iter = n
  copyInto dest, n:
    var isConcrete = true # assume it is concrete
    let sym = n.symId
    c.procStack.add(sym)
    let closureOwner = c.procStack[0]
    var isClosure = false
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(sym, n)
      elif i == ProcPragmasPos:
        if kind == IteratorY and hasPragma(n, ClosureP):
          isClosure = true
      elif i == TypevarsPos:
        isConcrete = n.substructureKind != TypevarsU
      takeTree dest, n

    if isConcrete and isClosure:
      treIteratorBody(c, dest, init, iter, sym)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of Symbol:
    takeTree dest, n
  of ParLe:
    case n.stmtKind
    of LocalDecls:
      trLocal c, dest, n
    of ProcS, FuncS, MacroS, MethodS, ConverterS:
      trCoroutine c, dest, n, NoSym
    of IteratorS:
      trCoroutine c, dest, n, IteratorY
    of TemplateS, TypeS, EmitS, BreakS, ContinueS,
      ForS, IncludeS, ImportS, FromimportS, ImportExceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of ScopeS:
      c.typeCache.openScope()
      trSons(c, dest, n)
      c.typeCache.closeScope()
    else:
      case n.exprKind
      of CallKinds:
        trCall c, dest, n
      of TypeofX:
        takeTree dest, n
      else:
        trSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc transformToCps*(n: var Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(thisModuleSuffix: moduleSuffix)
  c.typeCache.openScope()
  result = createTokenBuf()
  assert n.stmtKind == StmtsS
  result.takeToken n
  while n.kind != ParRi:
    tr(c, result, n)
  result.takeToken n # ParRi
  c.typeCache.closeScope()
