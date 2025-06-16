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

]##

proc treIterator(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var init = createTokenBuf(10)
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
      treProcBody(c, dest, init, n, sym, needsHeap)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()


