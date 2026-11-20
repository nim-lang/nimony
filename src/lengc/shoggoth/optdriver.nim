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

import std / [os, assertions, strutils, syncio, sets, envvars]
import ".." / ".." / "lib" / nifcoreparse   # parse/serialize; re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # createLengTagPool, stmtKind, takeProcDecl
import ".." / ".." / "lib" / lengiface       # writeLengInterface (`.oc.idx.nif`)
import induction_variables                     # runInductionVariables (live pass)
import cse                                     # runCSE + collectFunctionSummaries
import scalarizer                              # runScalarize (object → field scalars / SROA)
import copyprop                                # runCopyProp (copy prop + dead-store elim)
import unswitch                                # runUnswitch (loop unswitching)
import imi_bridge                             # runImiOn/parseModule (inter-module inliner)
import vectorizer                             # runVectorizer (map loops -> (instr ...))
export VecMode                                # the driver flag's type, for shoggoth.nim
import vmrewriter                              # the DFA rewrite engine (arith.rewrite.nif)
import tailcalls                              # runTailCalls (the tail-call encoding)
import coldguards                             # runColdGuards (guard arms to the body's end)
import memidioms                              # runMemIdioms (zero-fill loops -> zeroMemIns)
import ".." / ".." / "lib" / intrinsics       # IntrinsicOp (Stats.memOps)
export MemOpts                                # the driver flags' type, for shoggoth.nim
import ".." / nifmodules                      # MainModule + load (type context for aliasing)
import ".." / typenav                         # registerParams / scopes

include ".." / ".." / "lib" / compat2          # onRaiseQuit

const ArithRules = slurp("rules/arith.rewrite.nif")

type
  Target* = object
    ## What the passes may assume about the machine the module is built for.
    ## `shoggoth opt` fills it from its command line (`--cpu:`, `--vectorize`, ...).
    vecMode*: VecMode         ## the 128-bit loop vectorizer's rows, or `vecOff`
    mem*: MemOpts             ## which memory idioms the back end lowers
    scaledIndex*: bool        ## `base + index*size` is one addressing mode, so
                              ## `a[i]` costs no more than `p[]` (see
                              ## `induction_variables`)

proc hasScaledIndex*(cpu: string): bool =
  ## x86 and ARM address `[base + index*size]` in one operand (Thumb-2 as
  ## `[rn, rm, lsl #n]`); RISC-V, MIPS, AVR and the rest add first.
  cpu in ["i386", "amd64", "arm32", "arm64"]


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
  ## memidioms (both stages), memloops (only the loop idioms before the
  ## inliner), vectorize, sinkret, tailcall, coldguards.
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
    memOps*: set[IntrinsicOp]                 ## the mem rows memidioms emitted

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

const
  MaxCleanupRounds = 3
    ## Bound of the cleanup fixpoint. Most bodies converge in two rounds (the
    ## second confirms); a third catches the chains the second exposes. More
    ## buys nothing measurable on the benchmarks and costs compile time.

type
  CleanupStage = enum
    ## The cleanup pipeline's stages, in round order.
    stRewrite, stCtorProj, stScalarize, stCopyProp, stUnswitch, stRefold, stCse

  Fixpoint = object
    ## State of one body's cleanup fixpoint.
    clean: set[CleanupStage]
      ## Stages whose last run is still current: nothing changed the body since.
    changed: bool             ## the current round changed the body
    sroa, us, iv, cse: int
      ## The temp counters of the passes that mint locals. A pass that runs
      ## again in a later round continues its own numbering, so it never
      ## declares a name an earlier round already declared.

const
  IdempotentStages = {stRewrite, stCtorProj, stScalarize, stUnswitch, stRefold, stCse}
    ## Stages whose run on their own output is a no-op, so they are skipped
    ## when nothing changed the body since. The rewriter and unswitching are
    ## fixpoints themselves. Copy propagation is the exception: one run deletes
    ## only the last link of a dead-store chain (`x = bytes and 255` over
    ## `bytes = b.bytes`), so it runs every round.

proc mutated(f: var Fixpoint) {.inline.} =
  ## The body changed: every stage has something new to look at.
  f.changed = true
  f.clean = {}

template stage(f: var Fixpoint; s: CleanupStage; enabled: bool; pass: untyped) =
  ## Runs `pass`, which returns whether it changed the body, unless the stage
  ## is idempotent and the body is still what its last run left behind.
  if enabled and s notin f.clean:
    if pass: mutated(f)
    if s in IdempotentStages: f.clean.incl s

proc runPipelineRound(buf: var TokenBuf; f: var Fixpoint; st: var Stats;
                      summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                      params: Cursor; eng: Engine) =
  ## One round of the cleanup pipeline. Each pass leaves work for the next:
  ## the rewriter's `deref_addr` folds un-poison locals for SROA, SROA's scalar
  ## copies are what copy propagation deletes, copy propagation makes the
  ## conditions unswitching compares structurally identical, the unswitched
  ## copies hand CSE and the rewriter known conditions, and CSE's temps are
  ## bindings copy propagation can see through on the next round.
  f.changed = false
  let rw = eng != nil and passOn("rewrite")
  stage(f, stRewrite, rw): runRewritesFix(eng, buf) > 0
  stage(f, stCtorProj, passOn("ctorproj")): runConstructorProjection(buf)
  stage(f, stScalarize, passOn("scalarize")): runScalarize(buf, m, f.sroa)
  stage(f, stCopyProp, passOn("copyprop")): runCopyProp(buf, params, summaries, m)
  stage(f, stUnswitch, passOn("unswitch")): runUnswitch(buf, f.us) > 0
  # Copy propagation and unswitching substitute values into expressions;
  # `(add T x 0)` and `(lt 14 14)` that only now became foldable fold here,
  # before CSE keys on them.
  stage(f, stRefold, rw): runRewritesFix(eng, buf) > 0
  stage(f, stCse, passOn("cse")):
    runCSE(buf, summaries, m, params, f.cse, st.checksRemoved)

proc cleanupFixpoint(buf: var TokenBuf; f: var Fixpoint; st: var Stats;
                     summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                     params: Cursor; eng: Engine) =
  for round in 1 .. MaxCleanupRounds:
    runPipelineRound(buf, f, st, summaries, m, params, eng)
    if not f.changed: break

proc optimizeBody(buf: var TokenBuf; suffix: string; st: var Stats;
                  summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                  params: Cursor = default(Cursor); eng: Engine = nil;
                  target = Target(mem: MemOpts(intrinsics: true));
                  isVoid = false) =
  ## Per-body optimization pipeline. The nifcore passes plug in here as they
  ## are ported.
  ##
  ## The temps these passes mint are locals of the body, so they are named like
  ## locals -- `` `sroa.1 ``, `` `cse.1 `` -- and two bodies' first temps do
  ## share a pool symbol. That is what a local name is: every proc in the module
  ## already has its own `result.0`, and both backends scope a local to its
  ## function. This used to paste a per-body `<module>.<n>` after the counter to
  ## avoid the sharing, which made the module suffix a middle segment of the
  ## symbol (nimony#2457) -- a global's spelling for something that is not one.
  # The structural rewriter runs first so its `deref_addr` rules fold the
  # inliner's by-address residue — `inc i` splices as `(deref (haddr i))` — before
  # anything else looks at the body. Removing those `addr` nodes un-poisons the
  # locals for every later pass (SROA and copyprop treat address-taken locals as
  # untouchable) and for both backends' register allocation.
  # The vectorizer runs first, on the inliner's output: there an element access
  # is still a pointer temp bound once per iteration, `(asgn p (haddr (pat a i)))`
  # read as `(deref p)`, which is the shape it matches and which copy
  # propagation folds away. Its `(instr ...)` rows are final (selection-final by
  # the tag's contract), and the cleanup below optimizes the scalar remainder
  # loop it leaves behind like any other loop.
  if target.vecMode != vecOff and passOn("vectorize"):
    if runVectorizer(buf, "vec." & suffix):
      inc st.vectorized
  var f = Fixpoint()
  cleanupFixpoint(buf, f, st, summaries, m, params, eng)
  # Strength reduction is a lowering, not a cleanup: it is not idempotent (a
  # second run reduces its own pointer IVs again), so it runs exactly once, on
  # the cleaned-up loops. What it leaves behind is cleanup work again —
  # `` `x = haddr (pat `iv.q `iv.j) `` is a binding copy propagation can
  # substitute into `(deref `x)` and the rewriter then folds — so the fixpoint
  # resumes when it changed anything.
  if passOn("indvars") and runInductionVariables(buf, m, target.scaledIndex, f.iv):
    mutated(f)
    cleanupFixpoint(buf, f, st, summaries, m, params, eng)
  # `target.mem.mergeWordBytes` enables merging adjacent constant field
  # stores, native back end only. The loop idioms ran before the inliner
  # (`recognizeMemLoops`), so a fill or copy loop is a `zeroMemIns`/`copyMemIns`
  # row by the time the vectorizer looks, not vector stores; what is left
  # to do here is merging adjacent constant field stores, which wants the
  # inlined, scalarized code.
  if target.mem.mergeWordBytes > 0 and passOn("memidioms"):
    var late = target.mem
    late.intrinsics = false
    discard runMemIdioms(buf, m, params, late, "mem." & suffix, st.memOps)
  # The tail-call encoding runs after everything.
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
  # Guards' failure arms move to the end of the body last of all: CSE and the
  # redundant-guard elimination read a guard as a fact, which its `jmp` form no
  # longer is, and the tail-call encoding reads the body's final statement.
  if passOn("coldguards"):
    discard runColdGuards(buf, summaries, m, params, isVoid)

proc isAssemblerProc(pragmas: Cursor): bool =
  ## An `{.assembler.}` body is a one-to-one transcription of machine code
  ## whose locals are pinned to registers, so the passes' view of it is wrong:
  ## copy propagation, for one, cannot see that a write to `rdx` clobbers
  ## another local pinned to `rdx`. Such bodies are left exactly as written.
  result = false
  if pragmas.kind != TagLit: return
  var p = pragmas
  p.loopInto:
    if p.kind == TagLit and p.pragmaKind == AssemblerP:
      result = true
    skip p

type
  Stage = enum
    stLoopIdioms   ## before the inliner: only the memory-loop idioms
    stOptimize     ## after it: the whole per-body pipeline

proc recognizeMemLoopsInBody(buf: var TokenBuf; suffix: string; st: var Stats;
                             m: ptr MainModule; params: Cursor; target: Target) =
  ## Zero-fill and copy loops become `zeroMemIns`/`copyMemIns` BEFORE the
  ## inliner gets to the module. It runs first so that no later pass can hide a
  ## loop from it: the induction-variable pass rewrites a counted loop into the
  ## pointer walk the matcher does not know. The price is the loops that only
  ## appear by inlining a callee, which stay loops; a fill or a copy is written
  ## where the data lives, rarely in a helper small enough to be spliced.
  var early = target.mem
  early.mergeWordBytes = 0
  discard runMemIdioms(buf, m, params, early, "mem." & suffix, st.memOps)

proc rebuildTree(dest: var TokenBuf; n: var Cursor; suffix: string; st: var Stats;
                 summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                 eng: Engine = nil; target = Target(mem: MemOpts(intrinsics: true));
                 stage = stOptimize) =
  ## Copy the tree/token at `n` into `dest`, replacing each proc body with its
  ## optimized version. `dest` shares `n`'s pool+tags, so `addSubtree` is a
  ## bulk, line-info-preserving copy; reopened tags re-stamp their own info.
  if n.kind == TagLit:
    if n.stmtKind == ProcS:
      if stage == stOptimize: inc st.procs
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      let d = takeProcDecl(n)            # advances n past the whole proc
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      dest.addSubtree d.pragmas
      if d.body.kind == TagLit and not isAssemblerProc(d.pragmas):
        if stage == stOptimize: inc st.bodies
        # Open a typenav scope for this proc and register its params, so the
        # alias pass's `getType` can resolve param/local types (mirrors how the C
        # backend's `genProcDecl` drives the scopes).
        if m != nil:
          m[].openScope()
          m[].registerParams(d.params)
        var body = createTokenBuf(64, dest.pool, dest.tags)
        body.addSubtree d.body
        case stage
        of stOptimize:
          optimizeBody(body, suffix, st, summaries, m, d.params, eng, target,
                       isVoid = d.returnType.kind == DotToken)
        of stLoopIdioms:
          recognizeMemLoopsInBody(body, suffix, st, m, d.params, target)
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
          rebuildTree(dest, n, suffix, st, summaries, m, eng, target, stage)
      dest.closeTag()
  else:
    dest.addSubtree n
    inc n

proc optimizeModule*(src: var TokenBuf; suffix: string; st: var Stats;
                     m: ptr MainModule = nil; eng: Engine = nil;
                     target = Target(mem: MemOpts(intrinsics: true))): TokenBuf =
  ## Rebuild the single module-level root tree (`(stmts …)`), optimizing bodies.
  ## `m` is the module type context for the alias pass (nil ⇒ coarse aliasing);
  ## `eng` the structural rewrite engine (nil ⇒ the rewriter stage is skipped);
  ## `target.vecMode` enables the 128-bit loop vectorizer: `vecNeon` on native
  ## AArch64, `vecSse` on native x86-64. Both emit the same target-neutral rows
  ## today; the mode names the back end that lowers them.
  var summaries = collectFunctionSummaries(src)   # once per module; cse runs per body
  result = createTokenBuf(src.len + src.len div 8, src.pool, src.tags)
  var n = src.beginRead()
  if n.kind == TagLit and n.stmtKind == StmtsS:
    # Open the root by hand so the intrinsic declarations the vectorizer and
    # memidioms need can be appended inside it once every body is processed.
    let tag = n.cursorTagId
    let li = rawLineInfo(n)
    result.openTag tag
    if li.isValid: result.appendLineInfo li
    n.into:
      while n.hasMore:
        rebuildTree(result, n, suffix, st, addr summaries, m, eng, target)
    if st.vectorized > 0:
      addVecIntrinsicDecls(result, "vec." & suffix)
    if ZeroMemOp in st.memOps: addMemIntrinsicDecl(result, "mem." & suffix, ZeroMemOp)
    if CopyMemOp in st.memOps: addMemIntrinsicDecl(result, "mem." & suffix, CopyMemOp)
    result.closeTag()
  else:
    rebuildTree(result, n, suffix, st, addr summaries, m, eng, target)

proc recognizeMemLoops(src: var TokenBuf; suffix: string; st: var Stats;
                       m: ptr MainModule; target: Target) =
  ## `recognizeMemLoopsInBody` over every proc of the module. The intrinsic
  ## declarations the rows need are added by `optimizeModule`, from `st.memOps`.
  var dest = createTokenBuf(src.len + src.len div 32, src.pool, src.tags)
  var noSummaries = default(FunctionSummaryTable)  # the stage reads none
  var n = src.beginRead()
  while n.hasMore:
    rebuildTree(dest, n, suffix, st, addr noSummaries, m, nil, target, stLoopIdioms)
  endRead n
  src = ensureMove(dest)

proc checkWellFormed(buf: var TokenBuf) =
  ## Drain every top-level tree to exhaustion; `skip` would crash on a
  ## malformed (jump-inconsistent) buffer.
  var n = buf.beginRead()
  while n.hasMore: skip n

proc processFile*(input, output: string; verify = false;
                  target = Target(mem: MemOpts(intrinsics: true))): Stats =
  ## Optimize one NIFC file. Seeds the tag pool so `cursorTagId` aligns with the
  ## master NIFC tag ordinals (`stmtKind`/`takeProcDecl` rely on it).
  let suffix = extractModuleSuffix(input)
  var st = Stats()
  # 1. Load the module as a typenav context (for type-precise aliasing), in the
  #    same pools as `src`, so symbol ids line up between the type context and
  #    the optimization buffer. This used to serialize the inliner's result to
  #    NIF text and re-parse it into the context's pools: on `sem.nim` that
  #    round trip was 9.7 % of the whole run.
  var src = parseModule(input)
  var typeCtx = load(input, src.pool, src.tags)
  # 2. Memory-loop idioms, before anything can reshape the loops they match
  #    (see `recognizeMemLoopsInBody`).
  if target.mem.intrinsics and passOn("memidioms") and passOn("memloops"):
    recognizeMemLoops(src, suffix, st, addr typeCtx, target)
  # 3. Whole-module inter-module inlining (via the bridge, which owns the
  #    nifpools surface the inliner is written against).
  if passOn("imi") and runImiOn(src, suffix, splitFile(input).dir):
    inc st.intermodChanged
  # The inliner works on dense line info; the per-body passes were calibrated
  # on the sparse form a plain parse stores (unswitch bounds loops in raw
  # tokens, suffixes included), and CSE's temp order follows token positions.
  # An in-memory copy keeps them seeing exactly the tokens they always saw.
  src = withSparseLineInfo(src)
  # The rewrite engine shares the module's pool/tags so its compiled patterns'
  # tag ids coincide with the buffers it rewrites.
  var eng = newEngine(ArithRules, typeCtx.pool, typeCtx.tags)
  # The vectorizer and the memory-loop idioms run before the cleanup passes,
  # which ask the type context about every call and `(instr …)` they see: their
  # intrinsics must be known by then. They are appended to the module only if
  # something used them (`optimizeModule`).
  var intrinsicDecls = createTokenBuf(256, src.pool, src.tags)
  if target.vecMode != vecOff:
    addVecIntrinsicDecls(intrinsicDecls, "vec." & suffix)
  if ZeroMemOp in st.memOps: addMemIntrinsicDecl(intrinsicDecls, "mem." & suffix, ZeroMemOp)
  if CopyMemOp in st.memOps: addMemIntrinsicDecl(intrinsicDecls, "mem." & suffix, CopyMemOp)
  var d = beginRead(intrinsicDecls)
  while d.hasMore:
    typeCtx.registerProcDecl(d)
    skip d
  var optimized = optimizeModule(src, suffix, st, addr typeCtx, eng, target)
  checkWellFormed(optimized)
  onRaiseQuit writeFile(output, toModuleString(optimized, "." & extractModuleSuffix(output)))
  onRaiseQuit writeLengInterface(optimized, output)
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
