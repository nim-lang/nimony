## Robustness driver for the opt_unused passes: load a NIFC `.x.nif`
## module, run every optimizer (whole-module for the recursive passes,
## per-proc-body for the flow-sensitive ones), and check that each pass
## (a) does not crash and (b) leaves a structurally well-formed buffer.
##
##   nim c -d:rewriterDebug -o:/tmp/optfuzz optfuzz.nim
##   /tmp/optfuzz <file.x.nif> [<file2.x.nif> ...]

import std / [os, assertions, times, strutils]
include "../../lib" / nifprelude
import nifstreams, nifcursors
import ".." / nifc_model
import ".." / ".." / models / tags   # `*TagId` enumerators for genRewriter
import nifrender
import arcopt, copy_propagation, constant_folding, cse, induction_variables

# ---- rewriter instantiation ----------------------------------------------
import rewriter
proc isPow2(c: Cursor): bool =
  if c.kind != IntLit: return false
  let v = pool.integers[c.intId]
  v > 0 and (v and (v - 1)) == 0
genRewriter staticRead("rules/arith.rewrite.nif")

# ---- helpers --------------------------------------------------------------

proc copyBuf(src: var TokenBuf): TokenBuf =
  ## Independent copy of `src` (a fresh subtree clone).
  result = createTokenBuf(src.len + 4)
  var c = beginRead(src)
  while c.hasMore:
    result.addSubtree c
    skip c

proc wellFormed(buf: var TokenBuf): bool =
  ## A pass over every top-level tree via `skip`; trips an assertion on a
  ## malformed buffer (unbalanced / overrun), which we catch.
  result = true
  try:
    var n = beginRead(buf)
    while n.hasMore:
      skip n
  except CatchableError, Defect:
    result = false

proc rec(n: var Cursor; acc: var seq[TokenBuf]) =
  ## Recursive descent collecting every ProcS body (incl. nested procs).
  while n.hasMore:
    if n.kind == ParLe:
      if n.stmtKind == ProcS:
        var probe = n
        let d = takeProcDecl(probe)
        if d.body.kind == ParLe:
          var b = createTokenBuf(64)
          var bc = d.body
          b.addSubtree bc
          acc.add b
        skip n
      else:
        n.into:
          rec(n, acc)
    else:
      inc n

proc collectProcBodies(buf: var TokenBuf): seq[TokenBuf] =
  ## Every `(proc … body)`'s body subtree, extracted into its own buffer.
  result = @[]
  var n = beginRead(buf)
  rec(n, result)

var totalPasses = 0
var totalCrashes = 0
var totalMalformed = 0

template guard(label: string; body: untyped) =
  inc totalPasses
  try:
    body
  except CatchableError, Defect:
    inc totalCrashes
    echo "    !! CRASH ", label, ": ", getCurrentExceptionMsg()

proc reportPass(label: string; before, after: var TokenBuf) =
  let changed = before.len != after.len
  let ok = wellFormed(after)
  if not ok: inc totalMalformed
  echo "    ", label,
       "  tokens ", before.len, " -> ", after.len,
       (if changed: "  (changed)" else: "  (no-op)"),
       (if ok: "  [well-formed]" else: "  [** MALFORMED **]")

# ---- per-file driver ------------------------------------------------------

proc fuzzFile(fn: string) =
  echo "######## ", extractFilename(fn)
  var base = parseFromFile(fn, 4000)
  echo "  module tokens: ", base.len

  # Whole-module recursive passes.
  block:
    var b = copyBuf(base)
    var orig = copyBuf(base)
    guard "constant_folding":
      runConstantFolding b
      reportPass "constant_folding", orig, b
  block:
    var b = copyBuf(base)
    var orig = copyBuf(base)
    guard "induction_variables":
      runInductionVariables(b, "M")
      reportPass "induction_variables", orig, b
  block:
    var b = copyBuf(base)
    var orig = copyBuf(base)
    guard "rewriter":
      let fired = runRewritesFix(b, 8)
      echo "    rewriter  passes=", fired
      reportPass "rewriter", orig, b

  # Flow-sensitive passes, per proc body.
  var bodies = collectProcBodies(base)
  echo "  proc bodies: ", bodies.len
  var arcChanged, cpChanged, cseChanged = 0
  var arcBad, cpBad, cseBad = 0
  for i in 0 ..< bodies.len:
    # Compare by rendered content, not token count: copy-prop replaces one
    # symbol token with one literal/symbol token, so a length delta misses it.
    let before = render(bodies[i])
    block:
      var b = copyBuf(bodies[i])
      guard "arcopt#" & $i:
        runArcopt b
        if render(b) != before: inc arcChanged
        if not wellFormed(b): inc arcBad
    block:
      var b = copyBuf(bodies[i])
      guard "copy_propagation#" & $i:
        runCopyPropagation b
        if render(b) != before: inc cpChanged
        if not wellFormed(b): inc cpBad
    block:
      var b = copyBuf(bodies[i])
      guard "cse#" & $i:
        runCSE(b, "M")
        if render(b) != before: inc cseChanged
        if not wellFormed(b): inc cseBad
  echo "    arcopt:           changed ", arcChanged, "/", bodies.len,
       (if arcBad > 0: "  ** " & $arcBad & " MALFORMED **" else: "")
  echo "    copy_propagation: changed ", cpChanged, "/", bodies.len,
       (if cpBad > 0: "  ** " & $cpBad & " MALFORMED **" else: "")
  echo "    cse:              changed ", cseChanged, "/", bodies.len,
       (if cseBad > 0: "  ** " & $cseBad & " MALFORMED **" else: "")

proc main =
  if paramCount() == 0:
    quit "usage: optfuzz <file.x.nif> ..."
  let t0 = epochTime()
  for i in 1 .. paramCount():
    fuzzFile paramStr(i)
  echo "================================================"
  echo "passes run: ", totalPasses, "   crashes: ", totalCrashes,
       "   malformed: ", totalMalformed,
       "   time: ", formatFloat(epochTime() - t0, ffDecimal, 2), "s"

main()
