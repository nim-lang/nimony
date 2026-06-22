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

import std / [os, assertions, strutils, syncio]
import ".." / ".." / "lib" / nifcoreparse   # parse/serialize; re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # createLengTagPool, stmtKind, takeProcDecl
import induction_variables                     # runInductionVariables (live pass)
import cse                                     # runCSE + collectFunctionSummaries
import scalarizer                              # runScalarize (object → field scalars / SROA)
import copyprop                                # runCopyProp (copy prop + dead-store elim)
import imi_bridge                             # runImi (inter-module inliner, via nifcursors)
import ".." / nifmodules                      # MainModule + load (type context for aliasing)
import ".." / typenav                         # registerParams / scopes

type
  Stats* = object
    procs*, bodies*, intermodChanged*: int

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
                  summaries: ptr FunctionSummaryTable; m: ptr MainModule) =
  ## Per-body optimization pipeline. The nifcore passes plug in here as they
  ## are ported. The suffix is made unique per body (`st.bodies` is the body's
  ## index in the module): the passes name synthesized temps `<kind>.<n>.<suffix>`
  ## with a per-body counter, so without a per-body suffix two procs' first temps
  ## would collide on one module-pool symbol and the C codegen — which declares
  ## each symbol once — would leave later functions' uses undeclared.
  let bodySuffix = suffix & "." & $st.bodies
  # SROA first: fold field projections off inline constructors (`T(f: a).f` → `a`),
  # then explode non-escaping local objects into per-field scalars; copy
  # propagation then cleans up the resulting scalar copies and dead stores, so the
  # later passes see simpler, scalar code.
  runConstructorProjection(buf)
  runScalarize(buf, bodySuffix)
  runCopyProp(buf)
  runInductionVariables(buf, bodySuffix)
  runCSE(buf, bodySuffix, summaries, m)

proc rebuildTree(dest: var TokenBuf; n: var Cursor; suffix: string; st: var Stats;
                 summaries: ptr FunctionSummaryTable; m: ptr MainModule) =
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
        optimizeBody(body, suffix, st, summaries, m)
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
          rebuildTree(dest, n, suffix, st, summaries, m)
      dest.closeTag()
  else:
    dest.addSubtree n
    inc n

proc optimizeModule*(src: var TokenBuf; suffix: string; st: var Stats;
                     m: ptr MainModule = nil): TokenBuf =
  ## Rebuild the single module-level root tree (`(stmts …)`), optimizing bodies.
  ## `m` is the module type context for the alias pass (nil ⇒ coarse aliasing).
  var summaries = collectFunctionSummaries(src)   # once per module; cse runs per body
  result = createTokenBuf(src.len + src.len div 8, src.pool, src.tags)
  var n = src.beginRead()
  rebuildTree(result, n, suffix, st, addr summaries, m)

proc checkWellFormed(buf: var TokenBuf) =
  ## Drain every top-level tree to exhaustion; `skip` would crash on a
  ## malformed (jump-inconsistent) buffer.
  var n = buf.beginRead()
  while n.hasMore: skip n

proc processFile*(input, output: string; verify = false): Stats =
  ## Optimize one NIFC file. Seeds the tag pool so `cursorTagId` aligns with the
  ## master NIFC tag ordinals (`stmtKind`/`takeProcDecl` rely on it).
  let suffix = extractModuleSuffix(input)
  var st = Stats()
  # 1. Whole-module inter-module inlining runs first, in the nifcursors world
  #    (via the bridge); the result comes back as a NIF string.
  var imiChanged = false
  let imiNif = runImi(input, suffix, splitFile(input).dir, imiChanged)
  if imiChanged: inc st.intermodChanged
  # 2. Load the module as a typenav context (for type-precise aliasing), and
  #    reparse the (post-inlining) body into nifcore SHARING that context's pool
  #    so symbol ids line up between the type context and the optimization buffer.
  var typeCtx = load(input)
  var src = parseFromBuffer(imiNif, suffix, 4000,
                            sharedPool = typeCtx.pool, sharedTags = typeCtx.tags)
  var optimized = optimizeModule(src, suffix, st, addr typeCtx)
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
