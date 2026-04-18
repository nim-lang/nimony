#
#
#           Nimony Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Parser for `doc/tags.md`.
##
## `doc/tags.md` is the single source of truth for the Nimony / NIFC grammar.
## This module exposes the parsed form both for the source-level linter
## (`check_tags.nim`) and the in-compiler phase validator (`phase_validator.nim`).
##
## The grammar representation is intentionally minimal: a table keyed by tag
## name, with a list of alternative forms. Each form is a sequence of child
## slots (`ChildSpec`) plus an `isVarargs` flag for underspecified tags.

import std / [strutils, tables, syncio]

type
  ChildKind* = enum
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
    ckAny     ## unknown / don't care - matches any slot
    ckNested  ## nested tag like (kv ...), (elif ...), etc.

  ChildSpec* = object
    kind*: ChildKind
    dotAllowed*: bool ## `.D` in tags.md: DotToken is allowed in this slot
    optional*: bool   ## trailing `?`
    repeated*: bool   ## trailing `*` or `+`

  TagSpec* = object
    tag*: string
    children*: seq[ChildSpec]
    isVarargs*: bool  ## ends with `...` (underspecified)

  TagGrammar* = Table[string, seq[TagSpec]]  ## tag name -> allowed forms

const
  ExprStmtAmbiguousTags* = ["elif", "else", "of", "ret", "yld",
    "var", "let", "const", "cursor", "gvar", "tvar", "glet", "tlet",
    "result", "patternvar", "expr"]
    ## Tags whose `X` slots may legally contain a statement as well,
    ## because the surrounding context is statement-typed.

proc classifyChild*(s: string): ChildSpec =
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

proc parseTagSpec*(spec: string): TagSpec =
  ## Parse a spec like `(add T X X)` or `(if (elif X X)+ (else X)?)`.
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
      result.isVarargs = true
      pos += 3
    elif s[pos] == '.':
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

proc parseTagsMdLines*(mdLines: openArray[string]): TagGrammar =
  ## Parse the Markdown table in `doc/tags.md` split into lines.
  result = initTable[string, seq[TagSpec]]()
  var i = -2
  var inTable = false
  for line in mdLines:
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

proc parseTagsMdFromString*(content: string): TagGrammar =
  parseTagsMdLines(content.splitLines())

proc parseTagsMd*(filename: string): TagGrammar =
  ## Read and parse `doc/tags.md` at runtime.
  parseTagsMdFromString(readFile(filename))

proc kindName*(k: ChildKind): string =
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

proc specSlotName*(s: ChildSpec): string =
  result = ""
  if s.dotAllowed: result.add "."
  result.add kindName(s.kind)
  if s.optional: result.add "?"
  if s.repeated: result.add "*"

proc childMatchesSlot*(child: ChildKind; spec: ChildSpec;
                       exprStmtAmbiguous: bool): bool =
  ## Check if a produced child kind is compatible with a spec slot.
  if child == ckDot:
    return spec.dotAllowed or spec.kind in {ckE, ckP}
  if child == ckAny:
    return true
  if spec.kind == ckAny:
    return true
  if spec.kind == ckNested:
    return true
  if spec.kind == ckE:
    # Export slot accepts `x`/`.` *and* nullary kind markers like
    # `(typedesc)`, `(pointer)`, `(add -3)`, `(ismainmodule)`, `(concept ...)`,
    # `(inf)`, `(nan)` that stand in for the export marker on magic/builtin
    # declarations. Any tag kind is permitted in this slot.
    return child in {ckY, ckD, ckDot, ckAny, ckNested, ckT, ckX, ckS, ckXS, ckLit}
  if spec.kind == ckP:
    # `(pragmas …)` is classified as NimonyStmt (ckS) by the tag tables, so
    # we accept ckS here in addition to the nested form.
    return child in {ckNested, ckDot, ckAny, ckP, ckS, ckXS}
  if spec.kind == ckX and child in {ckLit, ckY, ckT, ckNested, ckS, ckXS}:
    # Type expressions *are* expressions in Nimony; several builtins
    # (`low`, `high`, `sizeof`, …) take a type where an expression is
    # expected by the grammar. Statement forms (`if`, `case`, `try`, …) can
    # also stand in for expressions when they are the tail of a block.
    return true
  if spec.kind == ckT and child in {ckY, ckNested}:
    return true
  if spec.kind == ckY and child in {ckNested, ckX, ckLit, ckS, ckXS}:
    # Overload-choice nodes (`(ochoice …)`, `(cchoice …)`) are NimonyExpr
    # but act as symbol references in e.g. `(dot expr symbol)`. Literal
    # keys are used for pragma key-value pairs such as `(kv "name" value)`.
    # `(except (let …) …)` and similar forms put a declaration node where
    # `Y` is expected.
    return true
  if spec.kind == ckD and child in {ckY, ckNested, ckX}:
    # Post-sem, `(unpackflat (let sym . . .))` binds an already-resolved
    # symbol where a SymDef would normally appear; accept it.
    return true
  if spec.kind == ckXS and child in {ckX, ckS, ckLit, ckY, ckT, ckNested}:
    return true
  if child == ckXS and spec.kind in {ckX, ckS, ckXS}:
    return true
  if spec.kind == ckS and child in {ckX, ckXS, ckLit, ckY, ckNested}:
    # Any expression is a valid statement in Nim/Nimony; `(stmts …)` may
    # contain expression-statements (template bodies, call statements).
    return true
  if exprStmtAmbiguous and spec.kind == ckX and child == ckS:
    return true
  return child == spec.kind

proc tryMatchSpec*(children: openArray[ChildKind]; spec: TagSpec): seq[string] =
  ## Try to match produced children against one spec form.
  ## Returns empty seq on success, or list of mismatch descriptions on failure.
  result = @[]
  if spec.isVarargs:
    return @[]

  var specSlots = spec.children
  let childCount = children.len

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

  let ambiguous = spec.tag in ExprStmtAmbiguousTags

  var ci = 0
  var si = 0
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
