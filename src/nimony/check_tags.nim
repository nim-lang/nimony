#
#
#           Nimony Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## `check_tags` statically analyses compiler pass source code (e.g. lambdalifting.nim)
## to verify that every `copyIntoKind`/`buildTree` call site constructs NIF nodes
## conforming to the grammar defined in `doc/tags.md`.
##
## It checks:
## - Child count matches the spec
## - Child kinds match (SymDef where D is expected, type where T is expected, etc.)
## - DotToken only appears where tags.md uses the `.` prefix (e.g. `.D`, `.X`)
##
## Usage: check_tags <passfile.nim> [tags.md]

import std / [strutils, os, tables, sets, osproc, assertions, syncio, sequtils]
include ".." / lib / nifprelude
import ".." / lib / [tooldirs]
import ".." / models / [tags, nimony_tags]
import nimony_model
import effect_graph
import tags_grammar

# ---------------------------------------------------------------------------
# Step 1: tags.md is parsed by `tags_grammar` - we just reuse its types.
# ---------------------------------------------------------------------------

proc toSpecKind(k: effect_graph.ChildKind): tags_grammar.ChildKind =
  case k
  of effect_graph.ckDot: tags_grammar.ckDot
  of effect_graph.ckD: tags_grammar.ckD
  of effect_graph.ckT: tags_grammar.ckT
  of effect_graph.ckX: tags_grammar.ckX
  of effect_graph.ckS: tags_grammar.ckS
  of effect_graph.ckY: tags_grammar.ckY
  of effect_graph.ckLit: tags_grammar.ckLit
  of effect_graph.ckAny: tags_grammar.ckAny
  of effect_graph.ckNested: tags_grammar.ckNested

type
  Violation = object
    line: int
    col: int
    file: string
    tag: string
    msg: string

  CheckContext = object
    grammar: TagGrammar
    effectGraph: EffectGraph
    violations: seq[Violation]
    filename: string
    checked: int
    skipped: int

proc lineInfoStr(info: PackedLineInfo): (int, int) =
  let u = unpack(pool.man, info)
  (u.line.int, u.col.int)


proc enumNameToTag(name: string): string =
  result = ""
  for e in TagEnum:
    if e == InvalidTagId: continue
    let (tagStr, _) = TagData[e]
    let nimName = tagStr[0].toUpperAscii & tagStr[1..^1]
    for suffix in ["X", "S", "T", "U", "P", "Y", "H", "F", "V", "Idx", "L", "C", "Q"]:
      if nimName & suffix == name:
        return tagStr

proc addViolation(ctx: var CheckContext; info: PackedLineInfo; tag, msg: string) =
  let (line, col) = lineInfoStr(info)
  ctx.violations.add Violation(line: line, col: col, file: ctx.filename,
                               tag: tag, msg: msg)

proc checkCopyIntoKind(ctx: var CheckContext; n: Cursor; info: PackedLineInfo) =
  var c = n
  if c.kind != ParLe: return
  inc c

  var tagName = ""
  var bodyPos = c
  var destName = "" # track which variable this writes to

  if c.kind == ParLe:
    let name = extractDotCallName(c)
    if name notin ["copyIntoKind", "buildTree"]: return
    destName = extractDotReceiver(c)
    skip c
    if c.kind == Ident:
      tagName = pool.strings[c.litId]
      skip c
    else:
      return
    skip c  # skip info
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        bodyPos = c
        break
      skip c
  elif c.kind == Ident:
    let name = pool.strings[c.litId]
    if name notin ["copyIntoKind", "buildTree"]: return
    inc c
    # Extract dest variable before skipping it
    if c.kind == Ident:
      destName = pool.strings[c.litId]
    elif c.kind == ParLe:
      destName = extractLastDotField(c)
    skip c  # skip dest
    if c.kind == Ident:
      tagName = pool.strings[c.litId]
      skip c
    else:
      return
    skip c  # skip info
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        bodyPos = c
        break
      skip c
  else:
    return

  if tagName.len == 0: return

  let mdTag = enumNameToTag(tagName)
  if mdTag.len == 0: return
  if mdTag notin ctx.grammar: return

  let specs = ctx.grammar[mdTag]

  # Use the effect graph to analyze the body, tracking the dest variable
  let effect = analyzeStmtsBody(ctx.effectGraph, bodyPos, destName)
  let flat = flatten(effect)

  if not flat.ok:
    ctx.skipped += 1
    return

  let children = flat.children.mapIt(toSpecKind(it))
  ctx.checked += 1

  # Try each allowed spec form — if any matches fully, we're good
  var bestErrors: seq[string] = @[]
  var matched = false

  for spec in specs:
    let errors = tryMatchSpec(children, spec)
    if errors.len == 0:
      matched = true
      break
    if bestErrors.len == 0 or errors.len < bestErrors.len:
      bestErrors = errors

  if not matched:
    let tagLabel = tagName & " (" & mdTag & ")"
    for err in bestErrors:
      addViolation(ctx, info, tagLabel, err)

proc scanForCopyIntoKind(ctx: var CheckContext; buf: var TokenBuf) =
  var n = beginRead(buf)
  var nested = 0
  assert n.kind == ParLe
  inc nested
  inc n
  while nested > 0:
    case n.kind
    of ParLe:
      let tag = pool.tags[n.tag]
      if tag in ["cmd", "call"]:
        var peek = n
        inc peek
        var found = false
        if peek.kind == ParLe:
          found = extractDotCallName(peek) in ["copyIntoKind", "buildTree"]
        elif peek.kind == Ident:
          found = pool.strings[peek.litId] in ["copyIntoKind", "buildTree"]
        if found:
          checkCopyIntoKind(ctx, n, n.info)
      inc nested
      inc n
    of ParRi:
      dec nested
      inc n
    else:
      inc n

# ---------------------------------------------------------------------------
# Step 2b: Check that case n.stmtKind / n.exprKind / etc. have no `else`
# ---------------------------------------------------------------------------

const ExhaustiveDiscriminators = [
  "stmtKind", "exprKind", "typeKind", "substructureKind", "symKind"]

proc scanForNonExhaustiveCases(ctx: var CheckContext; buf: var TokenBuf) =
  ## Find `case n.stmtKind` / `case n.exprKind` / etc. that have an `else`
  ## branch. These must enumerate all values explicitly so that the Nim
  ## compiler enforces exhaustive coverage when new tags are added.
  var n = beginRead(buf)
  var nested = 0
  assert n.kind == ParLe
  inc nested
  inc n
  while nested > 0:
    case n.kind
    of ParLe:
      let tag = pool.tags[n.tag]
      if tag == "case":
        # Extract discriminator: (case (dot n stmtKind) ...)
        var peek = n
        inc peek  # skip (case
        var discr = ""
        if peek.kind == ParLe:
          discr = extractLastDotField(peek)
        if discr in ExhaustiveDiscriminators:
          # Scan children for an `else` branch
          skip peek  # skip discriminator
          while peek.kind != ParRi:
            if peek.kind == ParLe and pool.tags[peek.tag] == "else":
              addViolation(ctx, n.info, "case " & discr,
                "`else` branch not allowed; enumerate all values for exhaustive checking")
              break
            skip peek
      inc nested
      inc n
    of ParRi:
      dec nested
      inc n
    else:
      inc n

# ---------------------------------------------------------------------------
# Step 3: Main
# ---------------------------------------------------------------------------

proc parseFileViaNifler(nimFile: string): TokenBuf =
  let nifler = findTool("nifler")
  let outFile = getTempDir() / "check_tags_" & extractFilename(nimFile).changeFileExt("nif")
  let cmd = quoteShell(nifler) & " parse " & quoteShell(nimFile) & " " & quoteShell(outFile)
  let (output, exitCode) = execCmdEx(cmd)
  if exitCode != 0:
    quit "nifler failed on " & nimFile & ": " & output

  var stream = nifstreams.open(outFile)
  try:
    discard processDirectives(stream.r)
    result = fromStream(stream)
  finally:
    nifstreams.close(stream)

proc main() =
  if paramCount() < 1:
    quit "Usage: check_tags <passfile.nim> [tags.md]"

  let passFile = paramStr(1)
  let tagsFile = if paramCount() >= 2: paramStr(2)
                 else:
                   let appDir = getAppDir()
                   var candidate = appDir / ".." / ".." / "doc" / "tags.md"
                   if not fileExists(candidate):
                     candidate = "doc/tags.md"
                   candidate

  if not fileExists(tagsFile):
    quit "Cannot find tags.md at: " & tagsFile
  if not fileExists(passFile):
    quit "Cannot find pass file: " & passFile

  let grammar = parseTagsMd(tagsFile)
  echo "Loaded ", grammar.len, " tags from ", tagsFile

  var buf = parseFileViaNifler(passFile)
  echo "Parsed ", passFile, " (", buf.len, " tokens)"

  # Build the effect graph — derive each proc's effect from its body
  var eg = buildEffectGraph(buf)
  echo "Built effect graph: ", eg.procs.len, " procs analyzed"

  # Report annotation mismatches (ensuresNif doesn't match derived body effect)
  for m in eg.mismatches:
    let af = flatten(m.annotation)
    let df = flatten(m.derived)
    var aDesc = "annotation: "
    if af.ok:
      for k in af.children: aDesc.add kindName(toSpecKind(k)) & " "
    else:
      aDesc.add "?"
    var dDesc = "body: "
    if df.ok:
      for k in df.children: dDesc.add kindName(toSpecKind(k)) & " "
    else:
      dDesc.add "?"
    echo "  WARNING: ", m.procName, " — ensuresNif mismatch: ", aDesc, "vs ", dDesc

  var ctx = CheckContext(grammar: grammar, effectGraph: eg, filename: passFile)
  scanForCopyIntoKind(ctx, buf)
  scanForNonExhaustiveCases(ctx, buf)

  echo "Checked ", ctx.checked, " call sites, skipped ", ctx.skipped, " (too complex)"

  # Check preservation property: procs with wrapsInput or requiresNif
  # annotations must advance the cursor on every code path.
  var preservationWarnings: seq[string] = @[]
  let procList = findProcBodies(buf)
  for (name, procCursor) in procList:
    if name notin eg.procs: continue
    let pe = eg.procs[name]
    # Only check procs explicitly marked via wrapsInput or requiresNif annotation
    let cv = if pe.cursorVar.len > 0: pe.cursorVar
             elif pe.wrapsInput: "n"
             else: ""
    if cv.len == 0: continue
    # Find the body
    var c = procCursor
    inc c
    var lastStmts = c
    var foundStmts = false
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        lastStmts = c
        foundStmts = true
      skip c
    if not foundStmts: continue
    let state = analyzeCursorPath(eg, lastStmts, cv)
    if state == csNotAdvanced:
      preservationWarnings.add name & " — cursor `" & cv &
        "` not advanced on every code path"

  if preservationWarnings.len > 0:
    echo preservationWarnings.len, " preservation warning(s):"
    for w in preservationWarnings:
      echo "  ", passFile, ": ", w

  var hasErrors = false
  if eg.mismatches.len > 0:
    hasErrors = true
  if ctx.violations.len == 0 and not hasErrors:
    echo "OK: no violations found."
  else:
    if ctx.violations.len > 0:
      echo ctx.violations.len, " violation(s) found:"
      for v in ctx.violations:
        echo "  ", v.file, "(", v.line, ",", v.col, "): ", v.tag,
             " [", v.msg, "]"
    quit 1

main()
