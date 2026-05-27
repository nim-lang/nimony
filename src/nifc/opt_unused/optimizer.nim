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
## The flow-sensitive passes (`arcopt`, `copy_propagation`, `cse`) analyse a
## single procedure body, so the driver walks the module and applies the
## pipeline to every `(proc … body)` body in turn, leaving declarations,
## types and headers untouched. The whole-program passes (`constant_folding`,
## `induction_variables`) are run on the same body so the whole pipeline is a
## single per-body sequence:
##
##   arcopt → copy_propagation → constant_folding → cse → induction_variables
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
import arcopt, copy_propagation, constant_folding, cse, induction_variables

type
  Stats = object
    procs, bodies: int
    changed: array[5, int]   # per-pass: how many bodies each pass altered

const PassNames = ["arcopt", "copy_propagation", "constant_folding",
                   "cse", "induction_variables"]

# ---- one body through the pipeline ----------------------------------------

proc optimizeBody(buf: var TokenBuf; suffix: string; st: var Stats) =
  template stage(i: int; call: untyped) =
    let before = render(buf)
    call
    if render(buf) != before: inc st.changed[i]
  stage 0: runArcopt buf
  stage 1: runCopyPropagation buf
  stage 2: runConstantFolding buf
  stage 3: runCSE(buf, suffix)
  stage 4: runInductionVariables(buf, suffix)

# ---- module rebuild --------------------------------------------------------

proc rebuildTree(dest: var TokenBuf; n: var Cursor; suffix: string; st: var Stats) =
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
        optimizeBody(body, suffix, st)
        var rb = beginRead(body)
        dest.addSubtree rb
      else:
        dest.addSubtree d.body              # forward decl / extern: empty body
      dest.addParRi()
    else:
      dest.addParLe(tag, info)
      n.into:
        while n.hasMore:
          rebuildTree(dest, n, suffix, st)
      dest.addParRi()
  else:
    dest.add n
    inc n

proc optimizeModule(input: var TokenBuf; suffix: string; st: var Stats): TokenBuf =
  result = createTokenBuf(input.len + input.len div 8)
  var n = beginRead(input)
  # Special-case the outermost block: the buffer is one module-level
  # `(stmts …)`, so rebuild that single root tree.
  rebuildTree(result, n, suffix, st)

# ---- well-formedness check -------------------------------------------------

proc wellFormed(buf: var TokenBuf): bool =
  result = true
  try:
    var n = beginRead(buf)
    skip n                  # skip the single outermost block; asserts on overrun
  except CatchableError, Defect:
    result = false

# ---- per-file driver -------------------------------------------------------

proc processFile(input, output: string; verify, stats: bool): bool =
  let suffix = extractModuleSuffix(input)
  var src = parseFromFile(input, 4000)
  var st = Stats()
  var optimized = optimizeModule(src, suffix, st)

  if not wellFormed(optimized):
    echo "  ", extractFilename(input), ": ** MALFORMED after optimization **"
    return false

  writeFile(optimized, output)

  if stats:
    var parts: seq[string] = @[]
    for i in 0 ..< PassNames.len:
      if st.changed[i] > 0: parts.add PassNames[i] & "=" & $st.changed[i]
    echo "  ", extractFilename(input), ": ", st.procs, " procs, ",
         st.bodies, " bodies",
         (if parts.len > 0: "  [" & parts.join(" ") & "]" else: "  [no changes]")

  result = true
  if verify:
    var back = parseFromFile(output, 4000)
    if not wellFormed(back):
      echo "  ", extractFilename(output), ": ** reload FAILED **"
      result = false

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
