#
#
#           NIFC Tree Optimizer (driver)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## External tool that runs the NIFC tree-optimization passes over a module.
##
## The flow-sensitive passes (`copy_propagation`, `cse`) analyse a
## single procedure body, so the driver walks the module and applies the
## pipeline to every `(proc … body)` body in turn, leaving declarations,
## types and headers untouched. The whole-program passes (`constant_folding`,
## `induction_variables`) are run on the same body so the whole pipeline is a
## single per-body sequence:
##
##   copy_propagation → constant_folding → cse → induction_variables
##
## Usage:
##
##   optimizer [options] <input.nif> [<output.nif>]
##
## With no `<output>` the file is rewritten in place. Options:
##
##   --outdir:DIR   write each input to DIR/<basename> (for batch runs)
##   --verify       reload every output and assert it is well-formed
##   --stats        print per-module pass activity

import std / [os, assertions, strutils]
include "../../lib" / nifprelude
import nifstreams, nifcursors
import nifreader   # extractModuleSuffix
import ".." / nifc_model
import nifrender   # render (used to detect per-pass changes)
from ".." / ".." / hexer / funcsummary import FunctionSummaryTable,
  collectFunctionSummaries
import intermodinliner, copy_propagation, constant_folding, cse, induction_variables

type
  Stats = object
    procs, bodies: int
    changed: array[4, int]   # per-pass: how many bodies each pass altered
    intermodChanged: int

const PassNames = ["copy_propagation", "constant_folding", "cse",
                   "induction_variables"]

# DEBUG bisection scaffolding: `NIFC_OPT_DISABLE=cp,cf,cse,iv,imi` turns off
# individual rewrites (and the inter-module inliner). Remove before commit.
let disabled = block:
  var s: seq[string] = @[]
  for x in getEnv("NIFC_OPT_DISABLE").split(','):
    if x.len > 0: s.add x.strip
  s
proc off(name: string): bool = name in disabled

# `copy_propagation`, `constant_folding` and `cse` are DISABLED by default and
# left out of the live pipeline (their sources are kept for reference only):
#   * copy_propagation duplicates the inter-module inliner's dest-routing —
#     "a known value flows to a known location" is the inliner's job;
#   * constant_folding is a weaker, hand-rolled second copy of `evalexpr`;
#   * cse caches an *address* and re-derefs it later, which is unsound without
#     alias analysis (seq growth / realloc / variable reseating invalidate the
#     cached pointer; clobber-on-call heuristics only paper over it).
# Opt a pass back in for experiments with e.g. `NIFC_OPT_ENABLE=cp,cse`.
let enabledExtra = block:
  var s: seq[string] = @[]
  for x in getEnv("NIFC_OPT_ENABLE").split(','):
    if x.len > 0: s.add x.strip
  s
proc optIn(name: string): bool = name in enabledExtra

# DEBUG: `NIFC_OPT_MODULES=suf1,suf2` restricts ALL optimization to those
# module suffixes (empty = all). Used to pin a miscompile to one module.
let optModules = block:
  var s: seq[string] = @[]
  for x in getEnv("NIFC_OPT_MODULES").split(','):
    if x.len > 0: s.add x.strip
  s
proc optAllowed(suffix: string): bool =
  optModules.len == 0 or suffix in optModules

# ---- one body through the pipeline ----------------------------------------

proc optimizeBody(buf: var TokenBuf; suffix: string; st: var Stats;
                  summaries: ptr FunctionSummaryTable) =
  template stage(i: int; call: untyped) =
    let before = render(buf)
    call
    if render(buf) != before: inc st.changed[i]
  # cp/cf/cse: off by default (see note above), opt-in via NIFC_OPT_ENABLE.
  if optIn("cp")  and not off("cp"):  stage 0: runCopyPropagation buf
  if optIn("cf")  and not off("cf"):  stage 1: runConstantFolding buf
  if optIn("cse") and not off("cse"): stage 2: runCSE(buf, suffix, summaries)
  # iv/imi: live passes, on unless explicitly disabled for bisection.
  if not off("iv"):  stage 3: runInductionVariables(buf, suffix)

# ---- module rebuild --------------------------------------------------------

proc rebuildTree(dest: var TokenBuf; n: var Cursor; suffix: string; st: var Stats;
                 summaries: ptr FunctionSummaryTable) =
  ## Copy the single tree/token at `n` into `dest`, replacing a proc body
  ## with its optimized version. Children are iterated with the scope-bounded
  ## `hasMore` loop inside `into`; the caller hands us one root tree at a time.
  if n.kind == ParLe:
    let tag = n.tagId
    let info = n.info
    if n.stmtKind == ProcS:
      inc st.procs
      let d = takeProcDecl(n)                # advances n past the whole proc
      dest.addParLe(tag, info)
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      dest.addSubtree d.pragmas
      if d.body.kind == ParLe:
        inc st.bodies
        var body = createTokenBuf(64)
        var bc = d.body
        body.addSubtree bc
        optimizeBody(body, suffix, st, summaries)
        var rb = beginRead(body)
        dest.addSubtree rb
      else:
        dest.addSubtree d.body              # forward decl / extern: empty body
      dest.addParRi()
    else:
      dest.addParLe(tag, info)
      n.into:
        while n.hasMore:
          rebuildTree(dest, n, suffix, st, summaries)
      dest.addParRi()
  else:
    dest.add n
    inc n

proc optimizeModule(input: var TokenBuf; suffix, xnifDir: string;
                    st: var Stats): TokenBuf =
  if not optAllowed(suffix):
    # Pass module through untouched (debug module-filter).
    result = createTokenBuf(input.len)
    var n0 = beginRead(input)
    result.addSubtree n0
    return
  if not off("imi") and runInterModuleInliner(input, suffix, xnifDir):
    inc st.intermodChanged
  var summaries = collectFunctionSummaries(input)
  result = createTokenBuf(input.len + input.len div 8)
  var n = beginRead(input)
  # Special-case the outermost block: the buffer is one module-level
  # `(stmts …)`, so rebuild that single root tree.
  rebuildTree(result, n, suffix, st, addr summaries)

# ---- well-formedness check -------------------------------------------------

proc checkWellFormed(buf: var TokenBuf) =
  ## Walk every top-level tree; `skip` asserts (and aborts) on a malformed
  ## buffer. A pass that produces garbage is a bug, so we let it crash loudly
  ## rather than swallowing the failure and silently skipping the file.
  var n = beginRead(buf)
  # Drain to exhaustion: a single-root `(stmts …)` buffer leaves the cursor at
  # `rem == 0` after the last `skip`, so use the exhaustion-safe primitive
  # rather than `hasMore` (which only stops at a closing `ParRi`).
  while n.hasCurrentToken:
    skip n
  endRead(buf)

# ---- per-file driver -------------------------------------------------------

proc processFile(input, output: string; verify, stats: bool): bool =
  let suffix = extractModuleSuffix(input)
  var src = parseFromFile(input, 4000)
  var st = Stats()
  # `.x.nif` files of *other* modules sit alongside (and one level up from)
  # the `.c.nif` we're reading; the inliner's `findForeignFile` searches
  # both. For non-main modules `<nimcache>/M.x.nif` is one dir up from the
  # `<nimcache>/<backend>/M.c.nif` input.
  let xnifDir = splitFile(input).dir
  var optimized = optimizeModule(src, suffix, xnifDir, st)

  checkWellFormed(optimized)

  writeFile(optimized, output)

  if stats:
    var parts: seq[string] = @[]
    if st.intermodChanged > 0: parts.add "intermodinliner=" & $st.intermodChanged
    for i in 0 ..< PassNames.len:
      if st.changed[i] > 0: parts.add PassNames[i] & "=" & $st.changed[i]
    echo "  ", extractFilename(input), ": ", st.procs, " procs, ",
         st.bodies, " bodies",
         (if parts.len > 0: "  [" & parts.join(" ") & "]" else: "  [no changes]")

  result = true
  if verify:
    var back = parseFromFile(output, 4000)
    checkWellFormed(back)

# ---- CLI -------------------------------------------------------------------

proc main =
  var positional: seq[string] = @[]
  var outdir = ""
  var verify = false
  var stats = false
  for i in 1 .. paramCount():
    let a = paramStr(i)
    if a.startsWith("--outdir:"): outdir = a["--outdir:".len .. ^1]
    elif a == "--verify": verify = true
    elif a == "--stats": stats = true
    elif a.startsWith("-"): quit "unknown option: " & a
    else: positional.add a

  if positional.len == 0:
    quit "usage: optimizer [--outdir:DIR] [--verify] [--stats] <input.nif> [<output.nif>]"

  var ok = true
  if outdir.len > 0:
    createDir outdir
    for inp in positional:
      let outp = outdir / extractFilename(inp)
      if not processFile(inp, outp, verify, stats): ok = false
  elif positional.len == 2:
    if not processFile(positional[0], positional[1], verify, stats): ok = false
  else:
    # in-place rewrite of each input
    for inp in positional:
      if not processFile(inp, inp, verify, stats): ok = false

  if not ok: quit 1

main()
