# Temporary stage-by-stage tracer for the shoggoth pipeline (Claude session).
# Mirrors optdriver.processFile/optimizeBody but dumps the body of procs whose
# name contains a substring after every pass.
#
#   tracer_tmp <input.c.nif> <procNameSubstr>

import std / [os, assertions, strutils, syncio]
import ".." / ".." / "lib" / nifcoreparse
import ".." / ".." / "lib" / nifcdecl
import induction_variables
import cse
import scalarizer
import copyprop
import imi_bridge
import vmrewriter
import ".." / nifmodules
import ".." / typenav

const ArithRules = staticRead("rules/arith.rewrite.nif")

proc extractModuleSuffix(filename: string): string =
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

var wantedSubstr = ""

proc dump(stage: string; buf: var TokenBuf) =
  echo "───────────────── after ", stage, " (", buf.len, " tokens) ─────────────────"
  echo toString(buf)

proc optimizeBodyTraced(buf: var TokenBuf; suffix: string; bodies: int;
                        summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                        params: Cursor; eng: Engine; traced: bool) =
  let bodySuffix = suffix & "." & $bodies
  if traced: dump("(input body)", buf)
  if eng != nil:
    runRewritesFix(eng, buf)
    if traced: dump("rewriter#1", buf)
  runConstructorProjection(buf)
  if traced: dump("constructorProjection", buf)
  runScalarize(buf, bodySuffix, m)
  if traced: dump("scalarize", buf)
  runCopyProp(buf, params, summaries, m)
  if traced: dump("copyProp", buf)
  if eng != nil:
    runRewritesFix(eng, buf)
    if traced: dump("rewriter#2", buf)
  runInductionVariables(buf, bodySuffix, m)
  if traced: dump("inductionVars", buf)
  discard runCSE(buf, bodySuffix, summaries, m)
  if traced: dump("cse", buf)

proc rebuildTree(dest: var TokenBuf; n: var Cursor; suffix: string;
                 procs, bodiesCount: var int;
                 summaries: ptr FunctionSummaryTable; m: ptr MainModule;
                 eng: Engine) =
  if n.kind == TagLit:
    if n.stmtKind == ProcS:
      inc procs
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      let d = takeProcDecl(n)
      var procName = ""
      if d.name.kind == SymbolDef: procName = symName(d.name)
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      dest.addSubtree d.name
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      dest.addSubtree d.pragmas
      if d.body.kind == TagLit:
        inc bodiesCount
        let traced = wantedSubstr.len > 0 and procName.find(wantedSubstr) >= 0
        if traced:
          echo "═════════════════ PROC ", procName, " ═════════════════"
        if m != nil:
          m[].openScope()
          m[].registerParams(d.params)
        var body = createTokenBuf(64, dest.pool, dest.tags)
        body.addSubtree d.body
        optimizeBodyTraced(body, suffix, bodiesCount, summaries, m, d.params,
                           eng, traced)
        if m != nil: m[].closeScope()
        var rb = body.beginRead()
        dest.addSubtree rb
      else:
        dest.addSubtree d.body
      dest.closeTag()
    else:
      let tag = n.cursorTagId
      let li = rawLineInfo(n)
      dest.openTag tag
      if li.isValid: dest.appendLineInfo li
      n.into:
        while n.hasMore:
          rebuildTree(dest, n, suffix, procs, bodiesCount, summaries, m, eng)
      dest.closeTag()
  else:
    dest.addSubtree n
    inc n

proc main =
  let input = paramStr(1)
  wantedSubstr = paramStr(2)
  let suffix = extractModuleSuffix(input)
  var imiChanged = false
  let imiNif = runImi(input, suffix, splitFile(input).dir, imiChanged)
  echo "imi changed: ", imiChanged
  var typeCtx = load(input)
  var src = parseFromBuffer(imiNif, suffix, 4000,
                            sharedPool = typeCtx.pool, sharedTags = typeCtx.tags)
  var eng = newEngine(ArithRules, typeCtx.pool, typeCtx.tags)
  var summaries = collectFunctionSummaries(src)
  var dest = createTokenBuf(src.len + src.len div 8, src.pool, src.tags)
  var n = src.beginRead()
  var procs = 0
  var bodiesCount = 0
  rebuildTree(dest, n, suffix, procs, bodiesCount, addr summaries,
              addr typeCtx, eng)
  echo "procs: ", procs, " bodies: ", bodiesCount

main()
