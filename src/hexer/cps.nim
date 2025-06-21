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
    SpecializedBase* = object
      next: proc (x: int; env: ref SpecializedBase): int {.nimcall.}
    Env* = object of SpecializedBase
      i: int

  proc it(x: int; env: ref SpecializedBase): int =
    var env = cast[ref Env](env)
    if env.i < x:
      result = env.i
      env.i += 1
    else:
      result = 0
      env.next = nil

  proc create(x: int): ref Env =
    result = Env(i: 0, next: it)

But in general this transformation is much more complex as we need to create
a new proc for every state:


1. Compile the AST/NIF to a CFG with goto instructions (src/nimony/controlflow does that already). Pay special
   attention that no implicit fall-through remains for code like (if cond: a); b().
2. Optimize the CFG so that unused labels are not generated.
3. Turn the target **labels** of the gotos into functions.
4. Move these inner functions to the top level ("lambda lifting"), compute the required environment objects.
5. The return value `result` remains and is passed out of the iterator like it is done for regular procs.
6. Turn `goto label` into function calls. Turn `yield` into `env.cont = nextState; return`.

type
  Generator[T] = object
    cont: proc (a, b: int): Generator[T]
    value: T

iterator countup(a, b: int): int =         |  proc countup(): proc (a, b: int): Generator[int] {.closure.} =
  var x = a                                |    var x = a
                                           |    var state: proc (a, b: int): Generator[int] = l1
L1:                                        |    proc l1(a, b: int): Generator[int] =
  if x > b:                                |      if x > b:
    goto Lend                              |        return lend(a, b)
  else:                                    |      else:
    goto L2                                |        return l2(a, b)
L2:                                        |    proc l2(a, b: int): Generator[int] =
  yield x  # yield produces label L3       |      return Generator[int](cont: l3, value: x)
L3:                                        |    proc l3(a, b: int): Generator[int] =
  inc x                                    |      inc x
  goto L1                                  |      return l1(a, b)
Lend:                                      |    proc lend(a, b: int): Generator[int] =
  discard # return x?                      |      return Generator[int](cont: nil, value: x)

for i in countup(0, 10):                   |  let it = countup()
  echo i                                   |  while true:
                                           |    let i = it(0, 10)
                                           |    if i.cont == nil: break
                                           |    echo i.value

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


