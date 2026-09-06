#
#
#        NIFC Tree Optimizer — nifcore driver (in progress)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The `nifcore` port of `shoggoth.nim`'s optimize path. Reads a NIFC module,
## walks every `(proc … body)`, runs the per-body pipeline (`optimizeBody`) and
## rebuilds the module. Built on **nifcore** (via `nifcoreparse`) + `nifcdecl`
## — *not* the old `nifprelude`/`nifcursors` world the legacy `shoggoth.nim`
## still uses, so the two cannot share a module during the migration.
##
## Status: parse → walk → rebuild → write is complete and round-trips; the
## per-body passes (`induction_variables`, …) are still being ported and plug
## into `optimizeBody`, which is currently an identity stage.

import std / [os, assertions, strutils, syncio, sets]
import ".." / ".." / "lib" / nifcoreparse   # parse/serialize; re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # createLengTagPool, stmtKind, takeProcDecl
import induction_variables                     # runInductionVariables (live pass)
import cse                                     # runCSE + collectFunctionSummaries
import scalarizer                              # runScalarize (object → field scalars / SROA)
import copyprop                                # runCopyProp (copy prop + dead-store elim)
import unswitch                                # runUnswitch (loop unswitching)
import imi_bridge                             # runImi (inter-module inliner, via nifcursors)
import vectorizer                             # runVectorizer (map loops -> (instr ...))
export VecMode                                # the driver flag's type, for shoggoth.nim
import vmrewriter                              # the DFA rewrite engine (arith.rewrite.nif)
import tailcalls                              # runTailCalls (the tail-call encoding)
import ".." / nifmodules                      # MainModule + load (type context for aliasing)
import ".." / typenav                         # registerParams / scopes

const ArithRules = staticRead("rules/arith.rewrite.nif")

let disabledPasses = block:
  ## `SHOGGOTH_DISABLE=rewrite,cse,…` turns individual optimization passes off.
  ##
  ## This is a BISECTION tool, not a tuning knob. When an optimized build misbehaves
  ## the question is always "which pass", and the only way to answer it was to edit
  ## `optimizeBody` and rebuild — per candidate, on a machine where rebuilding the
  ## toolchain is minutes. One env var turns that into one run each, and it composes
  ## with the boot: `SHOGGOTH_DISABLE=cse hastur boot --boot-backend:native` either
  ## reaches a fixed point or does not. `SHOGGOTH_NO_VECTORIZE` was the same idea for
  ## one pass; it stays, spelled `vectorize` here as well.
  ##
  ## Names: imi, rewrite, ctorproj, scalarize, copyprop, unswitch, indvars, cse,
  ## vectorize, sinkret, tailcall.
  var res = initHashSet[string]()
  for part in getEnv("SHOGGOTH_DISABLE").split(','):
    let name = part.strip()
    if name.len > 0: res.incl name
  if getEnv("SHOGGOTH_NO_VECTORIZE").len > 0: res.incl "vectorize"
  res

template passOn(name: string): bool = name notin disabledPasses


type
  Stats* = object
    procs*, bodies*, intermodChanged*: int
    checksRemoved*: int
    vectorized*: int

proc extractModuleSuffix(filename: string): string =
  ## Pure copy of `nifreader.extractModuleSuffix` (basename up to the first
  ## `.`), reimplemented here so this module need not pull the nifcursors world
  ## in just for one string helper.
  result = ""
  var skip = false
  for c in filename:
    if c == '/' or c == '\\':
      result.setLen 0
      skip = false
    elif c == '.':
      skip = true
    elif not skip:
      result.add c

proc optimizeBody(buf: var TokenBuf; suffix: string; st: var Stats;
                  summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                  params: Cursor = default(Cursor); eng: Engine = nil;
                  vecMode = vecOff) =
  ## Per-body optimization pipeline. The nifcore passes plug in here as they
  ## are ported. The suffix is made unique per body (`st.bodies` is the body's
  ## index in the module): the passes name synthesized temps `<kind>.<n>.<suffix>`
  ## with a per-body counter, so without a per-body suffix two procs' first temps
  ## would collide on one module-pool symbol and the C codegen — which declares
  ## each symbol once — would leave later functions' uses undeclared.
  let bodySuffix = suffix & "." & $st.bodies
  # The structural rewriter runs FIRST so its `deref_addr` rules fold the
  # inliner's by-address residue — `inc i` splices as `(deref (haddr i))` — before
  # anything else looks at the body. Removing those `addr` nodes un-poisons the
  # locals for every later pass (SROA and copyprop treat address-taken locals as
  # untouchable) and for both backends' register allocation.
  if eng != nil and passOn("rewrite"):
    runRewritesFix(eng, buf)
  # SROA first: fold field projections off inline constructors (`T(f: a).f` → `a`),
  # then explode non-escaping local objects into per-field scalars; copy
  # propagation then cleans up the resulting scalar copies and dead stores, so the
  # later passes see simpler, scalar code.
  if passOn("ctorproj"): runConstructorProjection(buf)
  if passOn("scalarize"): runScalarize(buf, bodySuffix, m)
  if passOn("copyprop"): runCopyProp(buf, params, summaries, m)
  # Hoist loop-invariant `if` conditions out of small loops by duplicating them
  # (loop unswitching): an inlined string accessor's SSO test runs once instead
  # of per character. AFTER copyprop so propagated copies make structurally
  # identical conditions actually identical.
  if passOn("unswitch"): runUnswitch(buf, bodySuffix)
  # Copy-prop inlines symbol and literal bindings; re-run the rewriter so
  # `(add T x 0)` / `(mul T x 1)` / `(add T 1 2)` that only became foldable
  # after those substitutions actually fold. Cheap: the DFA walk is linear
  # and a miss is a no-op.
  if eng != nil and passOn("rewrite"):
    runRewritesFix(eng, buf)
  if passOn("indvars"): runInductionVariables(buf, bodySuffix, m)
  # CSE also deletes index checks a dominating identical check already made:
  # same expression keys, same invalidation, same walk (see `cse.guardCondition`).
  if passOn("cse"):
    st.checksRemoved += runCSE(buf, bodySuffix, summaries, m, params)
  # The vectorizer runs last of the passes that OPTIMIZE: its emitted
  # `(instr ...)` applications are final (selection-final by the tag's contract)
  # and no later pass needs to look at them; the scalar remainder loop it leaves
  # behind was already optimized by everything above. Only the encoding passes
  # below follow it, and they rewrite `ret`s, which it never emits.
  if vecMode != vecOff and passOn("vectorize"):
    if runVectorizer(buf, bodySuffix, "vec." & suffix):
      inc st.vectorized
  # The tail-call encoding runs after EVERYTHING, the vectorizer included.
  # `(ret (call …))` deliberately violates the Leng rule that calls are bound and
  # do not nest: it is a directive to the backend ("do the tail call"), not an
  # expression. Minting it last is what keeps every pass above from having to
  # tolerate a nested call, and it is also when the most tails exist — each pass
  # above can only expose more. Its two rewrites share a walk and a grammar but
  # answer different questions when a build breaks — sinking changes
  # control-flow shape, folding changes stack discipline — so they are one pass
  # under two disable names.
  var tailRules: set[TailRule] = {}
  if passOn("sinkret"): tailRules.incl trSink
  if passOn("tailcall"): tailRules.incl trFold
  if tailRules != {}: runTailCalls(buf, tailRules)

proc rebuildTree(dest: var TokenBuf; n: var Cursor; suffix: string; st: var Stats;
                 summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                 eng: Engine = nil; vecMode = vecOff) =
  ## Copy the tree/token at `n` into `dest`, replacing each proc body with its
  ## optimized version. `dest` shares `n`'s pool+tags, so `addSubtree` is a
  ## bulk, line-info-preserving copy; reopened tags re-stamp their own info.
  if n.kind == TagLit:
    if n.stmtKind == ProcS:
      inc st.procs
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      let d = takeProcDecl(n)            # advances n past the whole proc
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      dest.addSubtree d.pragmas
      if d.body.kind == TagLit:
        inc st.bodies
        # Open a typenav scope for this proc and register its params, so the
        # alias pass's `getType` can resolve param/local types (mirrors how the C
        # backend's `genProcDecl` drives the scopes).
        if m != nil:
          m[].openScope()
          m[].registerParams(d.params)
        var body = createTokenBuf(64, dest.pool, dest.tags)
        body.addSubtree d.body
        optimizeBody(body, suffix, st, summaries, m, d.params, eng, vecMode)
        if m != nil: m[].closeScope()
        var rb = body.beginRead()
        dest.addSubtree rb
      else:
        dest.addSubtree d.body           # forward decl / extern: empty body
      dest.closeTag()
    else:
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      n.into:
        while n.hasMore:
          rebuildTree(dest, n, suffix, st, summaries, m, eng, vecMode)
      dest.closeTag()
  else:
    dest.addSubtree n
    inc n

proc optimizeModule*(src: var TokenBuf; suffix: string; st: var Stats;
                     m: ptr MainModule = nil; eng: Engine = nil;
                     vecMode = vecOff): TokenBuf =
  ## Rebuild the single module-level root tree (`(stmts …)`), optimizing bodies.
  ## `m` is the module type context for the alias pass (nil ⇒ coarse aliasing);
  ## `eng` the structural rewrite engine (nil ⇒ the rewriter stage is skipped);
  ## `vecMode` enables the 128-bit loop vectorizer: `vecNeon` on native
  ## AArch64, `vecSse` on native x86-64. Both emit the same target-neutral rows
  ## today; the mode names the back end that lowers them.
  var summaries = collectFunctionSummaries(src)   # once per module; cse runs per body
  result = createTokenBuf(src.len + src.len div 8, src.pool, src.tags)
  var n = src.beginRead()
  if vecMode != vecOff and n.kind == TagLit and n.stmtKind == StmtsS:
    # Open the root by hand so the vectorizer's intrinsic declarations can be
    # appended INSIDE it once every body is processed.
    let tag = n.cursorTagId
    let li = rawLineInfo(n)
    result.openTag tag
    if li.isValid: result.appendLineInfo li
    n.into:
      while n.hasMore:
        rebuildTree(result, n, suffix, st, addr summaries, m, eng, vecMode)
    if st.vectorized > 0:
      addVecIntrinsicDecls(result, "vec." & suffix)
    result.closeTag()
  else:
    rebuildTree(result, n, suffix, st, addr summaries, m, eng, vecMode)

proc checkWellFormed(buf: var TokenBuf) =
  ## Drain every top-level tree to exhaustion; `skip` would crash on a
  ## malformed (jump-inconsistent) buffer.
  var n = buf.beginRead()
  while n.hasMore: skip n

proc processFile*(input, output: string; verify = false;
                  vecMode = vecOff): Stats =
  ## Optimize one NIFC file. Seeds the tag pool so `cursorTagId` aligns with the
  ## master NIFC tag ordinals (`stmtKind`/`takeProcDecl` rely on it).
  let suffix = extractModuleSuffix(input)
  var st = Stats()
  # 1. Whole-module inter-module inlining runs first, in the nifcursors world
  #    (via the bridge); the result comes back as a NIF string.
  var imiChanged = false
  let imiNif =
    if passOn("imi"): runImi(input, suffix, splitFile(input).dir, imiChanged)
    else: readFile(input)
  if imiChanged: inc st.intermodChanged
  # 2. Load the module as a typenav context (for type-precise aliasing), and
  #    reparse the (post-inlining) body into nifcore SHARING that context's pool
  #    so symbol ids line up between the type context and the optimization buffer.
  var typeCtx = load(input)
  var src = parseFromBuffer(imiNif, suffix, 4000,
                            sharedPool = typeCtx.pool, sharedTags = typeCtx.tags)
  # The rewrite engine shares the module's pool/tags so its compiled patterns'
  # tag ids coincide with the buffers it rewrites.
  var eng = newEngine(ArithRules, typeCtx.pool, typeCtx.tags)
  var optimized = optimizeModule(src, suffix, st, addr typeCtx, eng, vecMode)
  checkWellFormed(optimized)
  writeFile(output, toModuleString(optimized, "." & extractModuleSuffix(output)))
  if verify:
    var back = parseFromFile(output, 4000, sharedTags = createLengTagPool())
    checkWellFormed(back)
  result = st

when isMainModule:
  # Round-trip self-test: with `optimizeBody` an identity stage, a rebuilt
  # module must serialize byte-identically to the parsed original.
  proc origText(s: string): string =
    var b = parseFromBuffer(s, "t", 100, sharedTags = createLengTagPool())
    toString(b)
  proc rebuiltText(s: string): string =
    var b = parseFromBuffer(s, "t", 100, sharedTags = createLengTagPool())
    var st = Stats()
    var o = optimizeModule(b, "t", st)
    toString(o)

  for s in [
    "(stmts (call foo +42 \"hi\") (asgn x 3.14) (ret -7))",
    "(stmts (proc :f.0 (params) (i +32) . (stmts (ret +0))))",
    "(stmts (proc :g.0 (params (param x.1 (i +32))) (i +32) . " &
      "(stmts (asgn x.1 +1) (ret x.1))) (proc :h.0 (params) . . .))",
    "(nested (a (b (c (d .)))))"]:
    let a = origText(s)
    let b = rebuiltText(s)
    doAssert a == b, "round-trip MISMATCH\n  orig:    " & a & "\n  rebuilt: " & b
  echo "optdriver self-tests passed"
