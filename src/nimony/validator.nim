#
#
#           Nimony Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Validates compiler pass source code for structural correctness.
##
## Includes all checks from `check_tags` (tag grammar conformance, exhaustive cases,
## preservation) plus obligation tracking for cursor/buffer variables:
##
## - Every `n: var Cursor` parameter creates a must-skip obligation
## - Every `skip n` without corresponding `takeTree`/`takeToken` is flagged
## - Field access like `c.dest` is resolved via type declarations in the same module
## - Hardcoded known types: Cursor, TokenBuf (no need for full type resolution)
##
## Usage: validator <passfile.nim> [tags.md]

import std / [strutils, os, tables, sets, osproc, assertions, syncio, sequtils]
include ".." / lib / nifprelude
import ".." / lib / [tooldirs]
import ".." / models / [tags, nimony_tags]
import nimony_model
import effect_graph
import tags_grammar

template validate(cond: bool; msg: string = "") =
  ## For the Nim compiler, `validate` is an alias for `assert`.
  ## For the validator tool, each `validate` is a proof obligation.
  assert cond, msg

# ---------------------------------------------------------------------------
# Symbol table: tracks variable types within each proc for obligation checking
# ---------------------------------------------------------------------------

type
  TrackedKind = enum
    tkUnknown     ## type not recognized
    tkCursor      ## var Cursor — has must-skip obligation
    tkTokenBuf    ## var TokenBuf — has must-fill obligation
    tkOther       ## known but untracked type

  FieldInfo = object
    name: string
    trackedKind: TrackedKind

  TypeInfo = object
    name: string
    fields: seq[FieldInfo]

  VarInfo = object
    name: string
    typeName: string    ## declared type name (for looking up fields)
    trackedKind: TrackedKind
    isMut: bool         ## declared as `var` parameter or `var` local

  SymbolTable = object
    ## Scoped symbol table for a single proc body.
    ## Tracks local variables and parameters with their types.
    vars: Table[string, VarInfo]

  TypeRegistry = object
    ## Module-level registry of type declarations.
    ## Maps type names to their fields so we can resolve `c.dest` → TokenBuf.
    types: Table[string, TypeInfo]

const
  CursorTypeNames = ["Cursor", "NifCursor"]
  TokenBufTypeNames = ["TokenBuf", "NifBuilder"]

proc classifyTypeName(name: string): TrackedKind =
  if name in CursorTypeNames: tkCursor
  elif name in TokenBufTypeNames: tkTokenBuf
  else: tkOther

proc initSymbolTable(): SymbolTable =
  SymbolTable(vars: initTable[string, VarInfo]())

proc addVar(st: var SymbolTable; name, typeName: string; isMut: bool) =
  st.vars[name] = VarInfo(name: name, typeName: typeName,
                          trackedKind: classifyTypeName(typeName), isMut: isMut)

proc getVar(st: SymbolTable; name: string): VarInfo =
  st.vars.getOrDefault(name, VarInfo(trackedKind: tkUnknown))

proc initTypeRegistry(): TypeRegistry =
  TypeRegistry(types: initTable[string, TypeInfo]())

proc addType(reg: var TypeRegistry; name: string; fields: seq[FieldInfo]) =
  reg.types[name] = TypeInfo(name: name, fields: fields)

proc resolveField(reg: TypeRegistry; typeName, fieldName: string): TrackedKind =
  ## Resolve `obj.field` to the field's type classification.
  if typeName in reg.types:
    for f in reg.types[typeName].fields:
      if f.name == fieldName:
        return f.trackedKind
  tkUnknown

proc resolveDotExpr(reg: TypeRegistry; st: SymbolTable; receiver, field: string): TrackedKind =
  ## Resolve `c.dest` — look up `c`'s type, then find `dest` in that type's fields.
  let v = st.getVar(receiver)
  # If the receiver has a known type name, look up the field in that type
  if v.typeName.len > 0 and v.typeName in reg.types:
    for f in reg.types[v.typeName].fields:
      if f.name == field:
        return f.trackedKind
  # Fallback: search all types for the field name
  if v.trackedKind in {tkUnknown, tkOther}:
    for tname, tinfo in reg.types:
      for f in tinfo.fields:
        if f.name == field:
          return f.trackedKind
  tkUnknown

# ---------------------------------------------------------------------------
# Extract type declarations from parsed NIF
# ---------------------------------------------------------------------------

proc extractTypeName(n: Cursor): string =
  ## Extract a simple type name from a type position in parsed NIF.
  ## Handles: `Cursor`, `(mut Cursor)`, `(ref Cursor)`, `(ptr Cursor)` etc.
  var c = n
  if c.kind == Ident:
    return pool.strings[c.litId]
  if c.kind == ParLe:
    let tag = pool.tags[c.tag]
    if tag in ["mut", "ref", "ptr", "lent", "sink"]:
      inc c  # skip tag
      if c.kind == Ident:
        return pool.strings[c.litId]
  ""

proc scanField(n: var Cursor): FieldInfo =
  ## Analyze one (fld name export pragmas type value) node.
  validate n.kind == ParLe and pool.tags[n.tag] == "fld"
  inc n, SkipTag # skip (fld
  result = FieldInfo()
  if n.kind == Ident:
    result.name = pool.strings[n.litId]
  skip n, SkipName
  skip n, SkipExport
  skip n, SkipPragmas
  let fldType = extractTypeName(n)
  if fldType.len > 0:
    result.trackedKind = classifyTypeName(fldType)
  skip n, SkipType
  skip n, SkipValue
  validate n.kind == ParRi, "expected closing ParRi for fld"
  inc n, SkipParRi

proc scanObjectFields(n: var Cursor): seq[FieldInfo] =
  ## Pure analyzer: extract tracked fields from an (object ...) node.
  ## Advances n past the entire object node.
  result = @[]
  validate n.kind == ParLe, "expected (object"
  inc n, SkipTag # skip (object
  skip n, SkipType
  while n.kind != ParRi:
    if n.kind == ParLe and pool.tags[n.tag] == "fld":
      let field = scanField(n)
      if field.name.len > 0 and field.trackedKind != tkOther:
        result.add field
    else:
      skip n
  inc n, SkipParRi

proc scanTypeDecl(n: var Cursor; reg: var TypeRegistry) =
  ## Scan one (type name export pragmas genparams body) node.
  ## If it's an object type with tracked fields, register it.
  validate n.kind == ParLe and pool.tags[n.tag] == "type"
  inc n, SkipTag # skip (type
  if n.kind == Ident:
    let typeName = pool.strings[n.litId]
    skip n, SkipName
    skip n, SkipExport
    skip n, SkipPragmas
    skip n, SkipGenParams
    if n.kind == ParLe and pool.tags[n.tag] == "object":
      let fields = scanObjectFields(n)
      if fields.len > 0:
        reg.addType(typeName, fields)
    # skip remaining children
    while n.kind != ParRi:
      skip n
  else:
    while n.kind != ParRi:
      skip n
  inc n, SkipParRi

proc buildTypeRegistry(buf: var TokenBuf): TypeRegistry =
  ## Scan the module for type declarations and record their fields.
  result = initTypeRegistry()
  var n = beginRead(buf)
  validate n.kind == ParLe, "module must start with ParLe (stmts)"
  inc n, SkipTag # skip (stmts
  while n.kind != ParRi:
    if n.kind == ParLe and pool.tags[n.tag] == "type":
      scanTypeDecl(n, result)
    else:
      skip n
  inc n, SkipParRi
  endRead buf

# ---------------------------------------------------------------------------
# Extract proc parameter types for the symbol table
# ---------------------------------------------------------------------------

proc scanParam(n: var Cursor; st: var SymbolTable) =
  ## Scan one (param name export pragmas type default) node.
  validate n.kind == ParLe and pool.tags[n.tag] == "param"
  inc n, SkipTag # skip (param
  if n.kind == Ident:
    let paramName = pool.strings[n.litId]
    skip n, SkipName
    skip n, SkipExport
    skip n, SkipPragmas
    let typeName = extractTypeName(n)
    let isMut = n.kind == ParLe and pool.tags[n.tag] == "mut"
    if typeName.len > 0:
      st.addVar(paramName, typeName, isMut)
  # skip remaining children
  while n.kind != ParRi:
    skip n
  inc n, SkipParRi

proc buildProcSymbolTable(paramsNode: Cursor; reg: TypeRegistry): SymbolTable =
  ## Build a symbol table for a proc by scanning its parameter list.
  ## `paramsNode` should be at the proc's (params ...) node.
  result = initSymbolTable()
  var n = paramsNode
  if n.kind != ParLe: return
  if pool.tags[n.tag] != "params": return
  inc n, SkipTag # skip (params
  while n.kind != ParRi:
    if n.kind == ParLe and pool.tags[n.tag] == "param":
      scanParam(n, result)
    else:
      skip n

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
    isWarning: bool  ## warnings don't affect exit code

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
                               tag: tag, msg: msg, isWarning: false)

proc addWarning(ctx: var CheckContext; info: PackedLineInfo; tag, msg: string) =
  let (line, col) = lineInfoStr(info)
  ctx.violations.add Violation(line: line, col: col, file: ctx.filename,
                               tag: tag, msg: msg, isWarning: true)

proc checkCopyIntoKind(ctx: var CheckContext; n: Cursor; info: PackedLineInfo) =
  var c = n
  if c.kind != ParLe: return
  inc c

  var tagName = ""
  var bodyPos = c
  var destName = "" # track which variable this writes to

  if c.kind == ParLe:
    let name = extractDotCallName(c)
    if name notin ["copyIntoKind", "buildTree", "withTree"]: return
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
    if name notin ["copyIntoKind", "buildTree", "withTree"]: return
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
          found = extractDotCallName(peek) in ["copyIntoKind", "buildTree", "withTree"]
        elif peek.kind == Ident:
          found = pool.strings[peek.litId] in ["copyIntoKind", "buildTree", "withTree"]
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
# Step 3: Obligation tracking — scan procs for unmatched skip/emit
# ---------------------------------------------------------------------------

const
  ## Procs that consume a cursor (must-skip obligation discharged)
  CursorConsumeProcs = ["skip", "takeTree", "takeToken", "takeParRi", "skipParRi", "inc"]
  ## Procs that emit to a buffer without consuming a cursor (must-fill)
  BufferEmitProcs = ["addParLe", "addParRi", "addDotToken", "addSymUse", "addSymDef",
                     "addIntLit", "addStrLit", "addEmpty", "add",
                     # plugin API (nimonyplugins):
                     "addIdent", "addUIntLit", "addCharLit", "addFloatLit",
                     "addEmptyNode", "addEmptyNode2", "addEmptyNode3", "addEmptyNode4",
                     "addSubtree"]
  ## Procs that skip the ParRi after a while-kind-ParRi loop
  ParRiSkipProcs = ["inc", "skip", "skipParRi", "takeParRi", "takeToken"]

proc scanCallsForCursorArg(bc: Cursor; endNested: int; cursorParams: seq[string];
                           consumeCounts: var Table[string, int]) =
  ## Scan a subtree for any call/cmd that passes one of the cursor params
  ## as an argument. This catches both direct consume procs (skip, takeTree)
  ## and pass-internal procs that take `var Cursor` (trExpr, trStmt, etc.).
  var bc = bc
  var nested = endNested
  while nested > 0:
    case bc.kind
    of ParLe:
      let tag = pool.tags[bc.tag]
      if tag in ["cmd", "call"]:
        var peek = bc
        inc peek # skip (cmd/(call
        # Skip the callee name/dot-expr
        var callName = ""
        if peek.kind == Ident:
          callName = pool.strings[peek.litId]
          skip peek
        elif peek.kind == ParLe:
          callName = extractDotCallName(peek)
          skip peek
        # Scan all arguments for cursor param names
        while peek.kind != ParRi:
          if peek.kind == Ident:
            let argName = pool.strings[peek.litId]
            if argName in consumeCounts:
              consumeCounts[argName] += 1
          skip peek
      inc nested
      inc bc
    of ParRi:
      dec nested
      inc bc
    else:
      inc bc

proc scanObligations(ctx: var CheckContext; reg: TypeRegistry; buf: var TokenBuf) =
  ## For each proc in the module, build a symbol table and report:
  ## - Parameters of type `var Cursor` that are never passed to any call
  ##
  ## This is a first approximation — it checks that the cursor param is used
  ## as an argument to at least one call. Not flow-sensitive, but catches
  ## completely unused cursor parameters.
  let procList = findProcBodies(buf)
  for (name, procCursor) in procList:
    var c = procCursor
    inc c
    var paramsPos = c
    var foundParams = false
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "params":
        paramsPos = c
        foundParams = true
        break
      skip c
    if not foundParams: continue

    let st = buildProcSymbolTable(paramsPos, reg)

    var cursorParams: seq[string] = @[]
    for varName, info in st.vars:
      if info.trackedKind == tkCursor and info.isMut:
        cursorParams.add varName

    if cursorParams.len == 0: continue

    # Find the stmts body
    c = procCursor
    inc c
    var bodyPos = c
    var foundBody = false
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        bodyPos = c
        foundBody = true
      skip c
    if not foundBody: continue

    var consumeCounts = initTable[string, int]()
    for cp in cursorParams:
      consumeCounts[cp] = 0

    var bc = bodyPos
    inc bc # skip (stmts
    scanCallsForCursorArg(bc, 1, cursorParams, consumeCounts)

    for cp in cursorParams:
      if consumeCounts[cp] == 0:
        addWarning(ctx, procCursor.info, name,
          "parameter `" & cp & ": var Cursor` is never passed to any call")

# ---------------------------------------------------------------------------
# Step 3b: Cursor-buffer balance — tie traversal and emission together
# ---------------------------------------------------------------------------

const
  ## Procs that advance cursor WITHOUT emitting (creates debt)
  ## Procs that advance cursor — debt if bare, balanced if called with reason string
  CursorSkipProcs = ["skip", "inc"]
  ## Procs that advance cursor AND emit to buffer (balanced)
  PairedProcs = ["takeTree", "takeToken", "takeParRi", "skipParRi"]
  ## Procs that consume cursor structurally and return parsed fields for later emission
  StructuredReadProcs = ["takeLocal", "takeRoutine", "asLocal", "asRoutine",
                         "asForStmt", "takeLocalHeader", "takeRoutineHeader"]
  ## Procs that wrap: advance cursor tag, run body, close (balanced at tag level)
  WrapProcs = ["copyInto", "copyIntoKind", "copyIntoKinds", "copyIntoUnchecked"]
  ## Procs that emit to buffer WITHOUT consuming cursor (creates credit)
  EmitOnlyProcs = ["add", "addParLe", "addParRi", "addDotToken", "addSymUse", "addSymDef",
                   "addIntLit", "addStrLit", "addEmpty", "addSubtree",
                   "copyIntoSymUse", "copyTree",
                   "addIdent", "addUIntLit", "addCharLit", "addFloatLit",
                   "addEmptyNode", "addEmptyNode2", "addEmptyNode3", "addEmptyNode4"]

proc typeHasBufferField(reg: TypeRegistry; typeName: string): bool =
  ## Check if a type has any field of type TokenBuf.
  if typeName in reg.types:
    for f in reg.types[typeName].fields:
      if f.trackedKind == tkTokenBuf:
        return true
  false

proc classifyExpr(st: SymbolTable; reg: TypeRegistry; n: Cursor): TrackedKind =
  ## Classify an expression as cursor, buffer, or unknown.
  ## A variable whose type has buffer fields is treated as buffer access
  ## (e.g. passing `c: var Context` where Context has `dest: TokenBuf`).
  if n.kind == Ident:
    let name = pool.strings[n.litId]
    let v = st.getVar(name)
    if v.trackedKind in {tkCursor, tkTokenBuf}:
      return v.trackedKind
    # Check if the variable's declared type has buffer fields
    if v.typeName.len > 0 and typeHasBufferField(reg, v.typeName):
      return tkTokenBuf
    return v.trackedKind
  if n.kind == ParLe and pool.tags[n.tag] == "dot":
    var c = n
    inc c # skip (dot
    if c.kind == Ident:
      let receiver = pool.strings[c.litId]
      skip c
      if c.kind == Ident:
        let field = pool.strings[c.litId]
        return resolveDotExpr(reg, st, receiver, field)
  tkUnknown

proc hasCursorArg(st: SymbolTable; reg: TypeRegistry; callNode: Cursor): bool =
  ## Check if any argument of the call is a cursor variable.
  var c = callNode
  inc c # skip (cmd/(call
  skip c # skip callee
  while c.kind != ParRi:
    if classifyExpr(st, reg, c) == tkCursor:
      return true
    skip c
  false

proc hasBufferArg(st: SymbolTable; reg: TypeRegistry; callNode: Cursor): bool =
  ## Check if any argument of the call is a buffer variable.
  ## Also checks the receiver of dot-calls (e.g. `c.dest.add(n)` — `c.dest` is the buffer).
  var c = callNode
  inc c # skip (cmd/(call
  # Check if the callee is a dot-call whose receiver is a buffer
  if c.kind == ParLe and pool.tags[c.tag] == "dot":
    var dotExpr = c
    inc dotExpr # skip (dot
    # The receiver is the first child of the dot
    if classifyExpr(st, reg, dotExpr) == tkTokenBuf:
      return true
  skip c # skip callee
  while c.kind != ParRi:
    if classifyExpr(st, reg, c) == tkTokenBuf:
      return true
    skip c
  false

const
  SkipIntentNames = ["SkipTag", "SkipParRi", "SkipName", "SkipExport",
                     "SkipPragmas", "SkipType", "SkipValue", "SkipGenParams",
                     "SkipCond", "SkipBody", "SkipCallee", "SkipResult", "SkipFull"]

proc hasIntentArg(n: Cursor): bool =
  ## Check if the call/cmd has a SkipIntent enum argument or a string literal.
  ## Accepts both: enum values (Ident matching SkipIntentNames) and legacy strings.
  var c = n
  inc c # skip (cmd/(call
  skip c # skip callee
  while c.kind != ParRi:
    if c.kind == StringLit:
      return true
    if c.kind == Ident and pool.strings[c.litId] in SkipIntentNames:
      return true
    skip c
  false

proc classifyCall(st: SymbolTable; reg: TypeRegistry; n: Cursor): int =
  ## Classify a call/cmd node and return its balance contribution.
  ## Positive = cursor advanced without emit (debt).
  ## Negative = emit without cursor advance (credit).
  ## Zero = balanced (paired, wrapped, or delegated).
  var peek = n
  inc peek
  var callName = ""
  if peek.kind == Ident:
    callName = pool.strings[peek.litId]
  elif peek.kind == ParLe:
    callName = extractDotCallName(peek)

  if callName in CursorSkipProcs:
    # skip/inc with a string reason argument = justified (balanced)
    # skip/inc without = unjustified (debt)
    if hasCursorArg(st, reg, n):
      if hasIntentArg(n): return 0  # justified
      return 1  # unjustified debt
  elif callName in StructuredReadProcs:
    return 0
  elif callName in PairedProcs:
    return 0
  elif callName in WrapProcs:
    return 0
  elif callName in EmitOnlyProcs:
    if hasBufferArg(st, reg, n): return -1
  else:
    let hasCur = hasCursorArg(st, reg, n)
    let hasBuf = hasBufferArg(st, reg, n)
    if hasCur and hasBuf: return 0 # delegated
    elif hasCur: return 1
    elif hasBuf: return -1
  return 0

proc blockBalance(ctx: var CheckContext; st: SymbolTable; reg: TypeRegistry;
                  n: Cursor; procName: string): int =
  ## Recursively compute the cursor-buffer balance of a block.
  ## Returns the net balance (positive = more skips than emits).
  ## Reports warnings for branches with mismatched balance.
  result = 0
  var n = n
  if n.kind != ParLe: return
  let tag = pool.tags[n.tag]

  if tag == "stmts":
    # Sequential: balances add up
    inc n
    while n.kind != ParRi:
      result += blockBalance(ctx, st, reg, n, procName)
      skip n

  elif tag in ["if", "case"]:
    inc n
    if tag == "case":
      skip n, SkipValue
    while n.kind != ParRi:
      if n.kind == ParLe:
        let branchTag = pool.tags[n.tag]
        if branchTag in ["elif", "else", "of"]:
          var inner = n
          inc inner
          if branchTag == "elif":
            skip inner # skip condition
          elif branchTag == "of":
            skip inner # skip ranges
          let b = blockBalance(ctx, st, reg, inner, procName)
          if b > 0:
            addWarning(ctx, n.info, procName,
              "branch advances cursor " & $b &
              " more time(s) than it emits (possible dropped input)")
      skip n
    # if/case as a whole contributes 0 — each branch is self-contained
    result = 0

  elif tag in ["cmd", "call"]:
    result = classifyCall(st, reg, n)

  elif tag in ["while", "for", "block", "try"]:
    # Recurse into bodies
    inc n
    while n.kind != ParRi:
      if n.kind == ParLe:
        discard blockBalance(ctx, st, reg, n, procName)
      skip n

proc scanCursorBufferBalance(ctx: var CheckContext; reg: TypeRegistry; buf: var TokenBuf) =
  ## For each proc with both cursor and buffer access, check per-block balance.
  ## Reports warnings for if/case branches with mismatched cursor-buffer balance.
  let procList = findProcBodies(buf)
  for (name, procCursor) in procList:
    var c = procCursor
    inc c
    var paramsPos = c
    var foundParams = false
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "params":
        paramsPos = c
        foundParams = true
        break
      skip c
    if not foundParams: continue

    let st = buildProcSymbolTable(paramsPos, reg)

    var hasCursor = false
    var hasBuffer = false
    for varName, info in st.vars:
      if info.trackedKind == tkCursor and info.isMut: hasCursor = true
      if info.trackedKind == tkTokenBuf: hasBuffer = true
    if not hasBuffer:
      var pc = paramsPos
      inc pc
      while pc.kind != ParRi:
        if pc.kind == ParLe and pool.tags[pc.tag] == "param":
          var p = pc
          inc p; skip p; skip p; skip p
          let typeName = extractTypeName(p)
          if typeName in reg.types:
            for f in reg.types[typeName].fields:
              if f.trackedKind == tkTokenBuf:
                hasBuffer = true
        skip pc
    if not hasCursor or not hasBuffer: continue

    c = procCursor
    inc c
    var bodyPos = c
    var foundBody = false
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        bodyPos = c
        foundBody = true
      skip c
    if not foundBody: continue

    discard blockBalance(ctx, st, reg, bodyPos, name)

# ---------------------------------------------------------------------------
# Step 4: Unsafe cursor ops — flag bare skip/inc on cursors in emitter procs
# ---------------------------------------------------------------------------

const
  ## Cursor procs that need a string reason in emitter procs.
  ## With a string argument = justified. Without = flagged.
  ReasonRequiredProcs = ["skip", "inc"]

proc scanUnsafeCursorOps(ctx: var CheckContext; reg: TypeRegistry; buf: var TokenBuf) =
  ## In emitter procs (cursor + buffer access), flag every bare `skip n` and
  ## `inc n` on a cursor variable. These should be replaced by:
  ##   - `takeToken dest, n` (for inc that copies)
  ##   - `skipUnsafe n, "reason"` (for skip that drops)
  ##   - `skipParRi n` (for structural close)
  let procList = findProcBodies(buf)
  for (name, procCursor) in procList:
    var c = procCursor
    inc c
    var paramsPos = c
    var foundParams = false
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "params":
        paramsPos = c
        foundParams = true
        break
      skip c
    if not foundParams: continue

    let st = buildProcSymbolTable(paramsPos, reg)

    # Only flag in emitter procs (have both cursor and buffer access)
    var hasCursor = false
    var hasBuffer = false
    for varName, info in st.vars:
      if info.trackedKind == tkCursor and info.isMut: hasCursor = true
      if info.trackedKind == tkTokenBuf: hasBuffer = true
    if not hasBuffer:
      var pc = paramsPos
      inc pc
      while pc.kind != ParRi:
        if pc.kind == ParLe and pool.tags[pc.tag] == "param":
          var p = pc
          inc p; skip p; skip p; skip p
          let typeName = extractTypeName(p)
          if typeName.len > 0 and typeHasBufferField(reg, typeName):
            hasBuffer = true
        skip pc
    if not hasCursor or not hasBuffer: continue

    # Collect cursor param names
    var cursorNames: seq[string] = @[]
    for varName, info in st.vars:
      if info.trackedKind == tkCursor and info.isMut:
        cursorNames.add varName

    # Scan body for unsafe ops on cursor variables
    c = procCursor
    inc c
    var bodyPos = c
    var foundBody = false
    while c.kind != ParRi:
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        bodyPos = c
        foundBody = true
      skip c
    if not foundBody: continue

    var bc = bodyPos
    var nested = 0
    if bc.kind != ParLe: continue
    inc nested; inc bc
    while nested > 0:
      case bc.kind
      of ParLe:
        let tag = pool.tags[bc.tag]
        if tag in ["cmd", "call"]:
          var peek = bc
          inc peek
          var callName = ""
          if peek.kind == Ident:
            callName = pool.strings[peek.litId]
          elif peek.kind == ParLe:
            callName = extractDotCallName(peek)
          if callName in ReasonRequiredProcs and not hasIntentArg(bc):
            # skip/inc without reason string in an emitter proc — flag it
            if peek.kind == Ident:
              skip peek
            else:
              skip peek
            while peek.kind != ParRi:
              if peek.kind == Ident:
                let argName = pool.strings[peek.litId]
                if argName in cursorNames:
                  addWarning(ctx, bc.info, name,
                    "`" & callName & " " & argName &
                    "` needs a reason string in emitter proc")
                  break
              skip peek
        inc nested; inc bc
      of ParRi:
        dec nested; inc bc
      else:
        inc bc

# ---------------------------------------------------------------------------
# Step 5: while-ParRi completion — ensure ParRi is consumed after the loop
# ---------------------------------------------------------------------------

proc extractWhileParRiVar(n: Cursor): string =
  ## If `n` is at a `(while COND BODY)` where COND is `(infix != (dot VAR kind) ParRi)`,
  ## return VAR. Otherwise return "".
  var c = n
  if c.kind != ParLe or pool.tags[c.tag] != "while": return ""
  inc c # skip (while
  # Condition should be (infix != (dot VAR kind) ParRi)
  # or could be wrapped: (infix and (infix != ...) ...)
  # For now handle the simple case
  if c.kind != ParLe: return ""
  let condTag = pool.tags[c.tag]
  var infixCur = c
  if condTag == "infix":
    inc infixCur # skip (infix
    if infixCur.kind != Ident: return ""
    let op = pool.strings[infixCur.litId]
    if op != "!=": return ""
    skip infixCur # skip op name
    # LHS should be (dot VAR kind)
    if infixCur.kind != ParLe: return ""
    if pool.tags[infixCur.tag] != "dot": return ""
    var dotCur = infixCur
    inc dotCur # skip (dot
    if dotCur.kind != Ident: return ""
    let varName = pool.strings[dotCur.litId]
    skip dotCur # skip var name
    if dotCur.kind != Ident: return ""
    let fieldName = pool.strings[dotCur.litId]
    if fieldName != "kind": return ""
    skip dotCur # skip field name
    # RHS should be ParRi
    skip infixCur # skip dot expr
    if infixCur.kind != Ident: return ""
    let rhsName = pool.strings[infixCur.litId]
    if rhsName != "ParRi": return ""
    return varName
  ""

proc isParRiSkipCall(n: Cursor; varName: string): bool =
  ## Check if `n` is at a call/cmd that skips the ParRi for `varName`.
  ## Matches: (cmd inc N varName), (cmd skipParRi N varName),
  ##          (cmd takeParRi N ... varName), (call ~N takeParRi N ... varName)
  if n.kind != ParLe: return false
  let tag = pool.tags[n.tag]
  if tag notin ["cmd", "call"]: return false
  var c = n
  inc c # skip (cmd/(call
  var callName = ""
  if c.kind == Ident:
    callName = pool.strings[c.litId]
    skip c
  elif c.kind == ParLe:
    callName = extractDotCallName(c)
    skip c
  if callName notin ParRiSkipProcs: return false
  # Check if varName appears as any argument
  while c.kind != ParRi:
    if c.kind == Ident and pool.strings[c.litId] == varName:
      return true
    skip c
  false

const
  ## Templates/calls that wrap a cursor and handle its ParRi internally.
  ## If a while-ParRi loop is the last statement inside one of these calls'
  ## body, the ParRi is consumed by the template — no explicit skip needed.
  CopyIntoProcs = ["copyInto", "copyIntoKind", "copyIntoKinds", "copyIntoUnchecked",
    # plugin API (nimonyplugins):
    "withTree"]

const
  ## Procs that contribute to a dest buffer (write output).
  ## If a while-ParRi loop body calls any of these, early exit would drop output.
  DestContributingProcs = ["takeTree", "takeToken", "takeParRi", "copyTree",
    "copyIntoKind", "copyIntoKinds", "copyInto", "copyIntoUnchecked",
    "addParLe", "addParRi", "addDotToken", "addSymUse", "addSymDef",
    "addIntLit", "addStrLit", "addEmpty", "addSubtree",
    "trExpr", "trStmt", "trLocal", "trProcDecl", "tr",
    # plugin API (nimonyplugins):
    "addIdent", "addUIntLit", "addCharLit", "addFloatLit",
    "addEmptyNode", "addEmptyNode2", "addEmptyNode3", "addEmptyNode4",
    "withTree"]

proc whileBodyContributesDest(whileNode: Cursor): bool =
  ## Check if the while loop body contains any call that writes to a dest buffer.
  ## Only loops that contribute to dest need the ParRi consumed — scan-only loops
  ## can break early without issue.
  var c = whileNode
  inc c  # skip (while
  skip c # skip condition
  # c is at the body (stmts ...)
  if c.kind != ParLe: return false
  var nested = 0
  inc nested; inc c
  while nested > 0:
    case c.kind
    of ParLe:
      let tag = pool.tags[c.tag]
      if tag in ["cmd", "call"]:
        var peek = c
        inc peek
        var callName = ""
        if peek.kind == Ident:
          callName = pool.strings[peek.litId]
        elif peek.kind == ParLe:
          callName = extractDotCallName(peek)
        if callName in DestContributingProcs:
          return true
      inc nested; inc c
    of ParRi:
      dec nested; inc c
    else:
      inc c
  false

proc scanWhileInStmts(ctx: var CheckContext; stmtsNode: Cursor; insideCopyInto: bool)

proc scanWhileRecurse(ctx: var CheckContext; n: Cursor; insideCopyInto: bool) =
  ## Recurse into a subtree, delegating to `scanWhileInStmts` for every stmts node.
  ## Does NOT re-enter stmts nodes on its own — only `scanWhileInStmts` processes
  ## stmts children, ensuring the copyInto context is tracked correctly.
  var n = n
  if n.kind != ParLe: return
  inc n # skip the opening tag
  while n.kind != ParRi:
    if n.kind == ParLe and pool.tags[n.tag] == "stmts":
      scanWhileInStmts(ctx, n, insideCopyInto)
    elif n.kind == ParLe:
      scanWhileRecurse(ctx, n, insideCopyInto)
    skip n

proc scanWhileInStmts(ctx: var CheckContext; stmtsNode: Cursor; insideCopyInto: bool) =
  ## Scan children of a stmts node. For each child:
  ## - If it's a while-ParRi loop, check that ParRi is consumed after it
  ## - If it's a copyInto call, recurse into its body with insideCopyInto=true
  ## - Otherwise recurse normally
  var child = stmtsNode
  inc child # skip (stmts
  while child.kind != ParRi:
    if child.kind == ParLe:
      let childTag = pool.tags[child.tag]
      if childTag == "while":
        let varName = extractWhileParRiVar(child)
        if varName.len > 0 and not insideCopyInto and whileBodyContributesDest(child):
          let whileInfo = child.info
          var afterWhile = child
          skip afterWhile
          var found = false
          var lookAhead = 0
          var peek = afterWhile
          while peek.kind != ParRi and lookAhead < 3:
            if isParRiSkipCall(peek, varName):
              found = true
              break
            skip peek
            inc lookAhead
          if not found:
            addWarning(ctx, whileInfo, "while " & varName & ".kind != ParRi",
              "ParRi not consumed after loop for `" & varName & "`")
        # Recurse into the while body to find nested patterns
        scanWhileRecurse(ctx, child, insideCopyInto)
      elif childTag in ["cmd", "call"]:
        var peek = child
        inc peek
        var callName = ""
        if peek.kind == Ident:
          callName = pool.strings[peek.litId]
        elif peek.kind == ParLe:
          callName = extractDotCallName(peek)
        if callName in CopyIntoProcs:
          # copyInto handles the ParRi — recurse with insideCopyInto=true
          skip peek # skip callee
          while peek.kind != ParRi:
            if peek.kind == ParLe and pool.tags[peek.tag] == "stmts":
              scanWhileInStmts(ctx, peek, true)
            skip peek
        else:
          scanWhileRecurse(ctx, child, insideCopyInto)
      else:
        scanWhileRecurse(ctx, child, insideCopyInto)
    skip child

proc scanWhileParRiCompletion(ctx: var CheckContext; buf: var TokenBuf) =
  ## Scan for `while n.kind != ParRi` loops and check that the ParRi is consumed.
  ## Loops inside `copyInto` bodies are exempted (the template handles the ParRi).
  var n = beginRead(buf)
  scanWhileRecurse(ctx, n, false)
  endRead buf

# ---------------------------------------------------------------------------
# Step 5: Main
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
    quit "Usage: validator [--strict] <passfile.nim> [tags.md]"

  # --strict enables checks that only make sense for compiler-pass source
  # (e.g. exhaustive-case on stmtKind). Plugins pass files without --strict
  # because `else: takeTree n` pass-through is idiomatic for them.
  var strict = false
  var positional: seq[string] = @[]
  for i in 1..paramCount():
    let a = paramStr(i)
    if a == "--strict": strict = true
    else: positional.add a
  if positional.len < 1:
    quit "Usage: validator [--strict] <passfile.nim> [tags.md]"

  let passFile = positional[0]
  let tagsFile = if positional.len >= 2: positional[1]
                 else:
                   # Candidates, in order: appDir/../doc/tags.md (bin in project root),
                   # appDir/../../doc/tags.md (bin nested one deeper), cwd/doc/tags.md.
                   let appDir = getAppDir()
                   var candidate = appDir / ".." / "doc" / "tags.md"
                   if not fileExists(candidate):
                     candidate = appDir / ".." / ".." / "doc" / "tags.md"
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

  # Build module-level type registry
  let reg = buildTypeRegistry(buf)
  echo "Type registry: ", reg.types.len, " types with tracked fields"
  for tname, tinfo in reg.types:
    var tracked: seq[string] = @[]
    for f in tinfo.fields:
      if f.trackedKind != tkOther:
        tracked.add f.name & ": " & $f.trackedKind
    if tracked.len > 0:
      echo "  ", tname, ": ", tracked.join(", ")

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
  # Exhaustive-case on tag kinds is valuable for compiler passes (forces every
  # pass to be reviewed when a new tag is added), but plugins legitimately use
  # `else: takeTree n` as a pass-through for kinds they don't transform. Run
  # the check only in --strict mode.
  if strict:
    scanForNonExhaustiveCases(ctx, buf)

  echo "Checked ", ctx.checked, " call sites, skipped ", ctx.skipped, " (too complex)"

  # Check preservation property: procs with wrapsInput or requiresNif
  # annotations must advance the cursor on every code path.
  var preservationWarnings: seq[string] = @[]
  let procList = findProcBodies(buf)
  for (name, procCursor) in procList:
    if name notin eg.procs: continue
    let pe = eg.procs[name]
    let cv = if pe.cursorVar.len > 0: pe.cursorVar
             elif pe.wrapsInput: "n"
             else: ""
    if cv.len == 0: continue
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

  # Obligation tracking: check cursor params are consumed
  scanObligations(ctx, reg, buf)

  # Cursor-buffer balance: tie traversal and emission together
  scanCursorBufferBalance(ctx, reg, buf)

  # Unsafe cursor ops: flag bare skip/inc in emitter procs
  scanUnsafeCursorOps(ctx, reg, buf)

  # While-ParRi completion: check ParRi is consumed after while-kind-ParRi loops
  scanWhileParRiCompletion(ctx, buf)

  var errors = 0
  var warnings = 0
  for v in ctx.violations:
    if v.isWarning: inc warnings
    else: inc errors

  var hasErrors = errors > 0 or eg.mismatches.len > 0

  if warnings > 0:
    echo warnings, " warning(s):"
    for v in ctx.violations:
      if v.isWarning:
        echo "  ", v.file, "(", v.line, ",", v.col, "): ", v.tag,
             " [", v.msg, "]"

  if errors > 0:
    echo errors, " violation(s) found:"
    for v in ctx.violations:
      if not v.isWarning:
        echo "  ", v.file, "(", v.line, ",", v.col, "): ", v.tag,
             " [", v.msg, "]"

  if not hasErrors:
    echo "OK: no violations found."
  else:
    quit 1

main()
