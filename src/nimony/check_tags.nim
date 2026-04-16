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

# ---------------------------------------------------------------------------
# Step 1: Parse tags.md to extract child specs
# ---------------------------------------------------------------------------

type
  ChildKind = enum
    ckDot     ## DotToken (empty placeholder)
    ckD       ## SymbolDef
    ckE       ## export marker
    ckP       ## pragmas
    ckT       ## type expression
    ckX       ## value expression
    ckS       ## statement / statement list
    ckXS      ## expression or statement (ambiguous context)
    ckY       ## symbol use
    ckLit     ## literal (INTLIT, STR, LIT)
    ckAny     ## unknown / don't care — matches any slot
    ckNested  ## nested tag like (kv ...), (elif ...), etc.

  ChildSpec = object
    kind: ChildKind
    dotAllowed: bool ## `.D` in tags.md: DotToken is allowed in this slot
    optional: bool   ## trailing `?`
    repeated: bool   ## trailing `*` or `+`

  TagSpec = object
    tag: string
    children: seq[ChildSpec]
    isVarargs: bool  ## ends with `...` (underspecified)

  TagGrammar = Table[string, seq[TagSpec]]  ## tag name -> allowed forms

proc classifyChild(s: string): ChildSpec =
  var name = s
  result = default(ChildSpec)
  if name.endsWith("?"):
    result.optional = true
    name = name[0..^2]
  elif name.endsWith("*"):
    result.repeated = true
    name = name[0..^2]
  elif name.endsWith("+"):
    result.repeated = true
    name = name[0..^2]

  # `.D` in tags.md means "DotToken or D" — the slot always exists,
  # but DotToken is allowed as a placeholder.
  if name.len > 1 and name[0] == '.':
    result.dotAllowed = true
    name = name[1..^1]

  result.kind = case name
    of "D": ckD
    of "E": ckE
    of "P": ckP
    of "T": ckT
    of "X": ckX
    of "S": ckS
    of "XS": ckXS
    of "Y": ckY
    of "LIT", "STR", "INTLIT", "INT_LIT", "STR_LIT": ckLit
    else:
      if name.startsWith("("):
        ckNested
      else:
        ckAny

proc parseTagSpec(spec: string): TagSpec =
  ## Parse a spec like `(add T X X)` or `(if (elif X X)+ (else X)?)`
  var s = spec.strip()
  if s.startsWith("("):
    s = s[1..^1]
  if s.endsWith(")"):
    s = s[0..^2]

  result = default(TagSpec)
  var i = 0
  while i < s.len and s[i] in IdentChars: inc i
  result.tag = s[0..<i]

  s = s[i..^1].strip()
  if s == "...":
    result.isVarargs = true
    return

  var pos = 0
  while pos < s.len:
    if s[pos] == ' ':
      inc pos
      continue
    if s[pos] == '(':
      # nested tag spec like (elif X X) or (kv Y X)*
      var depth = 1
      inc pos
      while pos < s.len and depth > 0:
        if s[pos] == '(': inc depth
        elif s[pos] == ')': dec depth
        inc pos
      var suffix = ""
      if pos < s.len and s[pos] in {'?', '*', '+'}:
        suffix = $s[pos]
        inc pos
      var child = ChildSpec(kind: ckNested)
      if suffix == "?": child.optional = true
      elif suffix in ["*", "+"]: child.repeated = true
      result.children.add child
    elif s[pos] == '.' and pos + 2 < s.len and s[pos+1] == '.' and s[pos+2] == '.':
      result.isVarargs = true  # underspecified in tags.md, skip checking
      pos += 3
    elif s[pos] == '.':
      # .D, .T, .X, .Y — slot where DotToken is allowed
      if pos + 1 < s.len and s[pos+1] in IdentChars:
        var j = pos
        while j < s.len and s[j] notin {' ', ')', '?', '*', '+'}: inc j
        var suffix = ""
        if j < s.len and s[j] in {'?', '*', '+'}:
          suffix = $s[j]
          inc j
        var child = classifyChild(s[pos..<j - suffix.len])
        child.optional = child.optional or suffix == "?"
        child.repeated = child.repeated or suffix in ["*", "+"]
        result.children.add child
        pos = j
      else:
        inc pos
    else:
      var j = pos
      while j < s.len and s[j] notin {' ', ')', '('}:
        inc j
      let token = s[pos..<j]
      if token.len > 0 and token != "...":
        result.children.add classifyChild(token)
      elif token == "...":
        result.isVarargs = true
      pos = j

proc parseTagsMd(filename: string): TagGrammar =
  result = initTable[string, seq[TagSpec]]()
  var i = -2
  var inTable = false
  for line in lines(filename):
    inc i
    if i <= 0: continue
    if not line.startsWith('|'):
      if inTable: break
      continue
    inTable = true
    var parts = line.split("|")
    if parts.len < 4: continue
    if parts[1].strip().startsWith('-'): continue

    let specField = parts[1].strip()
    let forms = specField.split(";")
    for form in forms:
      let f = form.strip().strip(chars = {'`'})
      if f.len == 0 or not f.startsWith("("): continue
      let spec = parseTagSpec(f)
      if spec.tag.len > 0:
        if spec.tag notin result:
          result[spec.tag] = @[]
        result[spec.tag].add spec

# ---------------------------------------------------------------------------
# Step 2: Analyse nifled pass source to find copyIntoKind call sites
# ---------------------------------------------------------------------------

proc toSpecKind(k: effect_graph.ChildKind): ChildKind =
  case k
  of effect_graph.ckDot: ckDot
  of effect_graph.ckD: ckD
  of effect_graph.ckT: ckT
  of effect_graph.ckX: ckX
  of effect_graph.ckS: ckS
  of effect_graph.ckY: ckY
  of effect_graph.ckLit: ckLit
  of effect_graph.ckAny: ckAny
  of effect_graph.ckNested: ckNested

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


proc kindName(k: ChildKind): string =
  case k
  of ckDot: "DotToken"
  of ckD: "SymDef"
  of ckE: "export"
  of ckP: "pragmas"
  of ckT: "type"
  of ckX: "expr"
  of ckS: "stmt"
  of ckXS: "expr|stmt"
  of ckY: "sym"
  of ckLit: "literal"
  of ckAny: "any"
  of ckNested: "nested"

proc specSlotName(s: ChildSpec): string =
  result = ""
  if s.dotAllowed: result.add "."
  result.add kindName(s.kind)
  if s.optional: result.add "?"
  if s.repeated: result.add "*"

proc childMatchesSlot(child: ChildKind; spec: ChildSpec;
                      exprStmtAmbiguous: bool): bool =
  ## Check if a produced child kind is compatible with a spec slot.
  if child == ckDot:
    # DotToken is allowed where tags.md uses the `.` prefix,
    # and E (export) and P (pragmas) slots can always be empty.
    return spec.dotAllowed or spec.kind in {ckE, ckP}
  if child == ckAny:
    return true
  if spec.kind == ckAny:
    return true
  if spec.kind == ckNested:
    return true
  if spec.kind == ckE:
    return child in {ckY, ckD, ckDot, ckAny}
  if spec.kind == ckP:
    return child in {ckNested, ckDot, ckAny, ckP}
  if spec.kind == ckX and child == ckLit:
    return true
  # XS matches both expressions and statements
  if spec.kind == ckXS and child in {ckX, ckS, ckLit}:
    return true
  # Tags like elif, else, of, ret, yld have expr/stmt ambiguity:
  # their X slots can contain statements in statement context
  if exprStmtAmbiguous and spec.kind == ckX and child == ckS:
    return true
  return child == spec.kind

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

const
  ExprStmtAmbiguousTags = ["elif", "else", "of", "ret", "yld",
    "var", "let", "const", "cursor", "gvar", "tvar", "glet", "tlet",
    "result", "patternvar", "expr"]

proc tryMatchSpec(children: seq[ChildKind]; spec: TagSpec): seq[string] =
  ## Try to match produced children against one spec form.
  ## Returns empty seq on success, or list of mismatch descriptions on failure.
  result = @[]
  if spec.isVarargs:
    return @[]  # underspecified, can't check

  # Build the expected slot sequence (expand non-optional, skip optional at end)
  var specSlots = spec.children
  let childCount = children.len

  # Count required and max slots
  var minSlots = 0
  var maxSlots = 0
  var hasRepeated = false
  for s in specSlots:
    if s.repeated:
      hasRepeated = true
    elif s.optional:
      maxSlots += 1
    else:
      minSlots += 1
      maxSlots += 1

  if hasRepeated:
    if childCount < minSlots:
      result.add "expected at least " & $minSlots & " children, got " & $childCount
      return
  else:
    if childCount < minSlots or childCount > maxSlots:
      result.add "expected " & $minSlots & ".." & $maxSlots & " children, got " & $childCount
      return

  # Match children positionally against spec slots
  let ambiguous = spec.tag in ExprStmtAmbiguousTags

  var ci = 0  # child index
  var si = 0  # spec index
  while ci < childCount and si < specSlots.len:
    let slot = specSlots[si]
    if slot.repeated:
      while ci < childCount:
        if not childMatchesSlot(children[ci], slot, ambiguous):
          result.add "child " & $ci & ": got " & kindName(children[ci]) &
                     ", expected " & specSlotName(slot)
        inc ci
      inc si
      break
    if slot.optional and ci < childCount:
      if childMatchesSlot(children[ci], slot, ambiguous):
        inc ci
        inc si
      else:
        inc si
    else:
      if not childMatchesSlot(children[ci], slot, ambiguous):
        result.add "child " & $ci & ": got " & kindName(children[ci]) &
                   ", expected " & specSlotName(slot)
      inc ci
      inc si

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
