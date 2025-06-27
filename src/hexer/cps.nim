#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included by lambdalifting.nim

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

proc treIteratorBody(c: var Context; dest: var TokenBuf; init: TokenBuf; iter: Cursor) =
  var cf = toControlflow(iter, keepReturns = true)

proc treIterator(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var init = createTokenBuf(10)
  let iter = n
  copyInto dest, n:
    var isConcrete = true # assume it is concrete
    let sym = n.symId
    c.procStack.add(sym)
    let closureOwner = c.procStack[0]
    let needsHeap = c.escapes.contains(closureOwner)
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.openProcScope(sym, n)
        let envType = if needsHeap: SymId(0) else: c.envTypeForProc(closureOwner)
        treParams c, dest, init, n, c.closureProcs.contains(sym), envType
      else:
        if i == TypeVarsPos:
          isConcrete = n.substructureKind != TypevarsU
        takeTree dest, n

    if isConcrete:
      treIteratorBody(c, dest, init, iter, sym, needsHeap)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()


