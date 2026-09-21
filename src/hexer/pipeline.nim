#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std / [assertions, tables, hashes, sets, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2

import ".." / nimony / [nimony_model, programs, decls]
import hexer_context, iterinliner, desugar, xelim, duplifier, lifter, destroyer,
  constparams, vtables_backend, eraiser, lambdalifting, cps, passes,
  funcsummary, intramodinliner, arcopt
import ".." / finalir / finalir
# `arcopt` runs on the final NIFC (try/finally already lowered to explicit
# control flow). It is the BasicBlock-based pass ported from the battle-tested
# `nim/compiler/optimizer.nim`: a stack of basic blocks each owning a pending
# `=wasMoved` list, cleared on return/break/loop and intersected only at
# exhaustive joins. (The earlier tracker-based `shoggoth/arcopt.nim` unioned
# positions across joins and propagated moved-state into nested branches, which
# let a diverging branch's destroy elide a parent `=wasMoved` → double-free;
# it has been removed in favour of this one.)
when defined(verifyArc):
  import std / syncio
  import ".." / nimony / verify_arc

proc publishHooks*(n: var Cursor) =
  case n.kind
  of TagLit:
    case n.stmtKind
    of ProcS, FuncS, MacroS, MethodS, ConverterS:
      let decl = asRoutine(n)
      var dest = createTokenBuf()
      takeTree(dest, n)
      let sym = decl.name.symId
      publish sym, dest
    else:
      n.into:
        while n.hasMore: publishHooks(n)
  else:
    inc n

proc transform*(c: var EContext; n: Cursor; moduleSuffix: string; bits: int): TokenBuf =
  # The input is already the Final IR (`doc/final_ir.md`): nimsem lowers a
  # module before publishing it. Every pass here reads and writes that form.
  var input = createTokenBuf(300)
  input.addSubtree n
  var pass = initPass(ensureMove input, moduleSuffix, "iterinliner", bits)

  # Pass 1: inline the inline iterators
  var m = pass.n
  elimForLoops(c, pass.dest, m)

  # Pass 2: Desugar
  pass.prepareForNext("desugar")
  desugar(pass, c.activeChecks)

  # Pass 3: Lambda Lifting
  pass.prepareForNext("lambdalift")
  elimLambdas(pass)

  # Pass 5: Exception Handling — ALL of it. A raising call becomes a temp plus
  # a check, and the success tuple lands in the same pass: signatures, the
  # `result` slot, the temps, and every use projected onto its value half.
  # Emitted as STATEMENTS in front of the enclosing statement, so it does not
  # re-introduce the `(expr (stmts ...) tmp)` nesting that used to require a
  # second `xelim` run right after it.
  #
  # Everything downstream therefore sees a finished shape: the destroyer sees a
  # tuple-typed temp like any other local and `cps` sees a coroutine that
  # happens to return a tuple. The cost of deciding it here is the one
  # `eraiser.nim`'s header describes — the lifter synthesises a hook per
  # `(ErrorCode, T)` that only delegates to `T`'s, which the inliner prunes.
  #
  # The flip side: nothing lowers raises after this point, so a later pass that
  # needs to signal an error emits the finished form (the duplifier's
  # out-of-memory check, via `builtintypes.addRaisedCode`).
  pass.prepareForNext("eraiser")
  injectRaisingCalls(pass, c.bits div 8)

  # Pass 6: Inject Duplication Points. Like the eraiser it emits its owning
  # temps as statements (`bindToTemp` → `c.hoisted`), which is what removed the
  # `xelim2` run that used to sit between this pass and the destroyer.
  pass.prepareForNext("duplifier")
  injectDups(pass, c.liftingCtx)

  # Pass 6: Inject Destructors (RAII/Cleanup)
  pass.prepareForNext("destroyer")
  injectDestructors(pass, c.liftingCtx)

  # Special handling: Merge generated hooks. The destroyer left the root
  # `(stmts` open for us; append the hooks, then close it.
  # The lifter emits Nimony IR (nimsem runs it too), so its hooks are lowered
  # to the Final IR here.
  if c.liftingCtx[].dest.len > 0:
    var hooks = createTokenBuf(c.liftingCtx[].dest.len + 2)
    hooks.addParLe StmtsS, NoLineInfo
    hooks.add move(c.liftingCtx[].dest)
    hooks.addParRi()
    var hookPass = initPass(ensureMove hooks, moduleSuffix, "finalir_hooks", bits,
                            pass.nextTemp)
    toFinalIr(hookPass, analysisFacts = false)
    var hookReader = beginRead(hookPass.dest)
    hookReader.into:
      let first = hookReader
      while hookReader.hasMore:
        publishHooks hookReader
      hookReader = first
      while hookReader.hasMore:
        pass.dest.takeTree hookReader
  pass.dest.addParRi()

  when defined(verifyArc):
    # Verify RC-op consistency on the post-destroyer IR. Gated on
    # `-d:verifyArc`. Currently the verifier runs straight-line analysis
    # only — no CFG/NJVL meet across branches yet — so it false-positives
    # on multi-branch destroyer output. We dump findings to stderr so
    # they can be triaged without failing the build.
    block:
      var arcErrs = analyzeArc(pass.dest, pass.moduleSuffix)
      if arcErrs.len > 0:
        stderr.writeLine "verify_arc diagnostics for ", pass.moduleSuffix, ":"
        stderr.writeLine toString(arcErrs, false)

  # Pass 7: CPS transform (coroutines)
  pass.prepareForNext("cps")
  transformToCps(pass)

  # Pass 9: Transform VTables (Virtual Table Backend)
  var needsXelimAgain = false
  pass.prepareForNext("vtables")
  transformVTables(pass, needsXelimAgain)

  # Pass 10: Inject Const Param Dereferences
  pass.prepareForNext("constparams")
  injectConstParamDerefs(pass, c.bits div 8, needsXelimAgain)

  # Pass 11: the remaining REAL lowering step, not a repair pass: `LowerCasts`
  # unnests calls (the Final-IR "calls are unnested statements" rule) and binds
  # a cast's source and result to variables. `vtables`/`constparams` still emit
  # `(expr (stmts ...) v)` for their temps, so this run also flattens those —
  # converting them to the `hoisted` discipline is what would leave this pass
  # with nothing but its own two jobs. See `doc/final_ir.md`.
  pass.prepareForNext("xelim_final")
  lowerExprs(pass, LowerCasts)
  pass.finishPass()

  result = ensureMove(pass.dest)

proc optimizeLengOutput*(buf: var TokenBuf; moduleSuffix: string; bits: int) =
  ## Optimizations over the generated Lengc tree. These run after `lengcgen`
  ## has emitted the final Lengc module, so they never see pre-Leng constructs
  ## such as try/finally.
  runArcopt(buf, moduleSuffix, bits)
  annotateFunctionSummaries(buf)
  intraModuleInline(moduleSuffix, buf)
