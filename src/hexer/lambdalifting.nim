#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Lambda lifting uses multiple passes:

- Determine which local variables cross proc boundaries. Map these to an environment.
  An environment is a scope that is allocated on the heap.
- Each usage of such a local becomes `env.local` but the `env` is not always the same:
  The outer env is itself a local variable, the inner env is a proc parameter.
- Procs that use a local variable that crosses a proc boundary are marked as "closure"
  ("uses environment").
- **Usages** of closure procs are turned from `fn` to `(fn, env)` and these tuple calls are
  turned from `(fn, env)(args)` to `fn(args, env)`. The tuple creation/unpacking can
  be optimized further.
- If all usages of closure procs do not escape, the environment can be allocated on
  the stack. As an approximation, closure procs do not escape if they are only used
  as the `fn` value in a function call `fn(args)`.
- A single indirection might not be enough. Consider:

```nim
  proc outerA =
    var a, b: int
    proc outerB =
      proc innerA = use(a)
      proc innerB = use(b); innerA()
      innerB()
```

Here `outerB` is also a closure.

]##

import std / [assertions, sets, tables]
include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints,
  builtintypes, langmodes, renderer, reporters]
import hexer_context

type
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    procStack: seq[SymId]
    closureProcs: HashSet[SymId]
    localToEnv: Table[SymId, SymId]

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.kind != ParRi:
      tr(c, dest, n)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  copyInto dest, n:
    c.procStack.add(n.symId)
    let isConcrete = c.typeCache.takeRoutineHeader(dest, n)
    if isConcrete:
      tr(c, dest, n)
    else:
      takeTree dest, n
    discard c.procStack.pop()
  c.typeCache.closeScope()

proc envTypeForProc(c: var Context; procId: SymId): SymId =
  let s = extractVersionedBasename(pool.syms[procId])
  result = pool.syms.getOrIncl(s & ".env." & c.thisModuleSuffix)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, SymbolDef,
     IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of Symbol:
    let loc = c.typeCache.getLocalInfo(n.symId)
    if loc.kind in {ParamY, LetY, VarY, ResultY}:
      let cross = loc.crossedProc.int
      if cross > 0:
        for i in c.procStack.len - cross ..< c.procStack.len:
          c.closureProcs.incl(c.procStack[i])
        let destEnv = c.procStack[0] #c.procStack.len - cross]
        c.localToEnv[n.symId] = destEnv
        #[

        Problem:

          proc outerA =
            var a: int
            proc outerB =
              var b: int
              proc inner =
                use a # uses env from outerA
                use b # uses env from outerB

        But there is only one environment parameter that `inner` can use
        for the accesses to `a` and `b`. Luckily, we analyse the entire
        `outerA` in one go, so we can use `outerA`'s environment for `outerB`
        too.
        ]#
        dest.copyIntoKind EnvpX, n.info:
          dest.addSymUse c.envTypeForProc(destEnv), n.info
          dest.addSymUse n.symId, n.info
        inc n
      else:
        takeTree dest, n
    else:
      takeTree dest, n
  of ParLe:
    case n.stmtKind
    of LocalDecls:
      trLocal c, dest, n
    of ProcS, FuncS, MacroS, MethodS, ConverterS:
      trProc c, dest, n
    of IteratorS, TemplateS, TypeS, EmitS, BreakS, ContinueS,
      ForS, CmdS, IncludeS, ImportS, FromimportS, ImportExceptS,
      ExportS, CommentS,
      PragmasS:
      takeTree dest, n
    of ScopeS:
      c.typeCache.openScope()
      trSons(c, dest, n)
      c.typeCache.closeScope()
    else:
      if n.exprKind == TypeofX:
        takeTree dest, n
      else:
        trSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc elimLambdas*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix)
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  tr c, result, n
  c.typeCache.closeScope()
