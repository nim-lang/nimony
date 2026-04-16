#
#
#           Nimony Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Builds an "effect graph" for each proc in a compiler pass source file.
## An effect describes what a proc adds to a `dest: var TokenBuf` argument
## when called. Effects are composed: if proc A calls proc B, A's effect
## includes B's effect.
##
## Inspired by CompCert's per-pass simulation approach: instead of trusting
## annotations, we derive each proc's contract from its body.

import std / [tables, sets, assertions, strutils]
include ".." / lib / nifprelude
import ".." / models / [tags, nimony_tags]
import nimony_model

type
  ChildKind* = enum
    ckDot     ## DotToken (empty placeholder)
    ckD       ## SymbolDef
    ckT       ## type expression
    ckX       ## value expression
    ckS       ## statement / statement list
    ckY       ## symbol use
    ckLit     ## literal
    ckAny     ## unknown / matches any slot
    ckNested  ## nested substructure (params, elif, etc.)

  EffectKind* = enum
    ekFixed       ## adds exactly one child of known kind
    ekRepeat      ## adds 0+ children (while loop)
    ekCounted     ## adds exactly N children (for 0..<N loop)
    ekBranch      ## if/else: one of two effects
    ekSeq         ## sequential composition
    ekEmpty       ## adds nothing
    ekUnknown     ## cannot determine

  Effect* = ref object
    case kind*: EffectKind
    of ekFixed:
      childKind*: ChildKind
    of ekRepeat:
      repeatKind*: ChildKind
    of ekCounted:
      count*: int
      countedKind*: ChildKind
    of ekBranch:
      thenEffect*: Effect
      elseEffect*: Effect
    of ekSeq:
      children*: seq[Effect]
    of ekEmpty:
      discard
    of ekUnknown:
      discard

  ProcEffect* = object
    name*: string
    effect*: Effect  ## what it adds to dest
    annotated*: bool ## true if effect comes from ensuresNif annotation
    wrapsInput*: bool ## true if it does copyInto dest, n: (preserves tag)

  SymContext* = Table[string, ChildKind]
    ## Maps identifiers (field names, variable names) to their NIF kind
    ## based on their declaration context or type refinement.

  AnnotationMismatch* = object
    procName*: string
    annotation*: Effect
    derived*: Effect

  EffectGraph* = object
    symContext*: SymContext
    mismatches*: seq[AnnotationMismatch]
    procs*: Table[string, ProcEffect]

# Well-known constants from decls.nim
const
  BodyPos* = 8
  ParamsPos* = 4
  TypevarsPos* = 3
  ReturnTypePos* = 5
  ProcPragmasPos* = 6
  LocalPragmasPos* = 2
  LocalTypePos* = 3
  LocalValuePos* = 4

proc seqEffect*(effects: varargs[Effect]): Effect =
  var es: seq[Effect] = @[]
  for e in effects:
    if e.kind == ekSeq:
      es.add e.children
    elif e.kind != ekEmpty:
      es.add e
  if es.len == 0: Effect(kind: ekEmpty)
  elif es.len == 1: es[0]
  else: Effect(kind: ekSeq, children: es)

proc fixedEffect*(kind: ChildKind): Effect =
  Effect(kind: ekFixed, childKind: kind)

proc repeatEffect*(kind: ChildKind): Effect =
  Effect(kind: ekRepeat, repeatKind: kind)

proc countedEffect*(n: int; kind: ChildKind): Effect =
  if n == 0: Effect(kind: ekEmpty)
  elif n == 1: fixedEffect(kind)
  else: Effect(kind: ekCounted, count: n, countedKind: kind)

proc branchEffect*(a, b: Effect): Effect =
  Effect(kind: ekBranch, thenEffect: a, elseEffect: b)

proc unknownEffect*(): Effect =
  Effect(kind: ekUnknown)

proc emptyEffect*(): Effect =
  Effect(kind: ekEmpty)

# ---- Flattening: convert an Effect into a concrete seq[ChildKind] ----
# Returns empty seq if the effect can't be flattened (has unknowns or branches
# with differing counts).

type FlatResult* = object
  children*: seq[ChildKind]
  hasRepeat*: bool
  ok*: bool

proc flatten*(e: Effect): FlatResult =
  result = FlatResult(ok: true)
  case e.kind
  of ekFixed:
    result.children.add e.childKind
  of ekRepeat:
    result.hasRepeat = true
    # Can't flatten to a fixed sequence
  of ekCounted:
    for i in 0..<e.count:
      result.children.add e.countedKind
  of ekBranch:
    let a = flatten(e.thenEffect)
    let b = flatten(e.elseEffect)
    if not a.ok or not b.ok:
      result.ok = false
      return
    if a.children.len != b.children.len:
      # Different counts — check if both are valid
      # We can't flatten to a single sequence; return the longer one as approximate
      result.ok = false
      return
    # Same count: merge kinds (if they differ, use ckAny)
    for i in 0..<a.children.len:
      if a.children[i] == b.children[i]:
        result.children.add a.children[i]
      else:
        result.children.add ckAny
    result.hasRepeat = a.hasRepeat or b.hasRepeat
  of ekSeq:
    for child in e.children:
      let f = flatten(child)
      if not f.ok:
        result.ok = false
        return
      result.children.add f.children
      result.hasRepeat = result.hasRepeat or f.hasRepeat
  of ekEmpty:
    discard
  of ekUnknown:
    result.ok = false

# ---- Building the graph from nifled source ----

proc extractDotCallName*(c: Cursor): string =
  result = ""
  if c.kind != ParLe: return
  var n = c
  if pool.tags[n.tag] != "dot": return
  inc n
  skip n
  if n.kind == Ident:
    result = pool.strings[n.litId]

proc extractLastDotField(c: Cursor): string =
  ## From `(dot obj fieldName)`, extract the innermost field name.
  result = ""
  if c.kind != ParLe: return
  var n = c
  if pool.tags[n.tag] != "dot": return
  inc n
  skip n # skip receiver
  if n.kind == Ident:
    result = pool.strings[n.litId]

proc extractCallInfo(n: Cursor): (string, string) =
  ## From a (cmd ...) or (call ...) node, extract callee name and first arg.
  ## For first arg, handles both bare Idents and (dot obj field) → returns field name.
  var c = n
  if c.kind != ParLe: return ("", "")
  inc c
  var callee = ""
  if c.kind == ParLe:
    callee = extractDotCallName(c)
    skip c
  elif c.kind == Ident:
    callee = pool.strings[c.litId]
    inc c
    skip c  # skip dest for non-method call
  var firstArg = ""
  if c.kind == Ident:
    firstArg = pool.strings[c.litId]
  elif c.kind == ParLe:
    # Could be (dot obj field) — extract the field name
    firstArg = extractLastDotField(c)
  (callee, firstArg)

proc enumSuffixToKind*(name: string): ChildKind =
  # Special cases where the enum suffix doesn't match the semantic role
  if name == "PragmasS" or name == "PragmasU": return ckNested
  if name.endsWith("Idx"): return ckAny
  if name.len < 2: return ckAny
  case name[^1]
  of 'X': ckX
  of 'S': ckS
  of 'T': ckT
  of 'U': ckNested
  of 'P': ckNested
  of 'Y': ckD
  of 'H': ckX
  of 'F': ckS
  of 'V': ckS
  else: ckAny

proc resolveConstRange(n: Cursor): int =
  ## Try to resolve a `(infix ..< +0 BodyPos)` pattern to a constant.
  ## Returns -1 if can't resolve.
  var c = n
  if c.kind != ParLe: return -1
  let tag = pool.tags[c.tag]
  if tag != "infix": return -1
  inc c
  # operator: ..<
  if c.kind == Ident:
    let op = pool.strings[c.litId]
    if op notin ["..<", "\\2E\\2E<"]: return -1
  else: return -1
  skip c
  # start: should be +0 (IntLit 0)
  if c.kind != IntLit: return -1
  let startVal = pool.integers[c.intId]
  skip c
  # end: should be a known constant like BodyPos
  if c.kind == Ident:
    let name = pool.strings[c.litId]
    case name
    of "BodyPos": return BodyPos - startVal.int
    of "ParamsPos": return ParamsPos - startVal.int
    of "TypevarsPos": return TypevarsPos - startVal.int
    of "ReturnTypePos": return ReturnTypePos - startVal.int
    of "ProcPragmasPos": return ProcPragmasPos - startVal.int
    of "LocalPragmasPos": return LocalPragmasPos - startVal.int
    of "LocalTypePos": return LocalTypePos - startVal.int
    of "LocalValuePos": return LocalValuePos - startVal.int
    else: return -1
  elif c.kind == IntLit:
    return pool.integers[c.intId].int - startVal.int
  else:
    return -1

proc analyzeIfBranches*(graph: EffectGraph; n: Cursor): Effect
proc analyzeWhileLoop*(graph: EffectGraph; n: Cursor): Effect
proc analyzeForLoop*(graph: EffectGraph; n: Cursor): Effect

proc analyzeStmtsBody*(graph: EffectGraph; body: Cursor): Effect =
  ## Analyze a (stmts ...) block that produces children in `dest`.
  ## This is the core analysis routine that replaces `classifyChildren`.
  var n = body
  if n.kind != ParLe: return unknownEffect()
  if pool.tags[n.tag] != "stmts": return unknownEffect()
  inc n

  var effects: seq[Effect] = @[]

  while n.kind != ParRi:
    if n.kind != ParLe:
      if n.kind == Ident:
        # Bare identifier — could be template invocation
        return unknownEffect()
      skip n
      continue

    let stmtTag = pool.tags[n.tag]
    case stmtTag
    of "call", "cmd":
      let (callName, firstArg) = extractCallInfo(n)

      case callName
      of "addDotToken":
        effects.add fixedEffect(ckDot)
      of "addSymDef":
        effects.add fixedEffect(ckD)
      of "addSymUse", "copyIntoSymUse":
        # Try to classify via sym context (field type refinement or decl context)
        if firstArg.len > 0 and firstArg in graph.symContext:
          effects.add fixedEffect(graph.symContext[firstArg])
        else:
          effects.add fixedEffect(ckAny)
      of "addIntLit", "addUIntLit", "addIntVal", "addStrLit",
         "addCharLit", "addFloatLit":
        effects.add fixedEffect(ckLit)
      of "addParPair":
        effects.add fixedEffect(enumSuffixToKind(firstArg))
      of "copyIntoKind", "copyIntoKinds", "buildTree":
        effects.add fixedEffect(enumSuffixToKind(firstArg))
      of "copyInto":
        effects.add fixedEffect(ckAny) # copies one node from input
      of "takeTree", "takeToken", "copyTree", "addEmpty", "addToken",
         "addSubtree", "addTarget":
        effects.add fixedEffect(ckAny)
      of "addParLe":
        effects.add fixedEffect(ckAny) # manual node, paired with addParRi
      of "addParRi":
        discard # closing paren, not a child
      of "addEmpty2":
        # addEmpty2 adds two DotTokens
        effects.add fixedEffect(ckDot)
        effects.add fixedEffect(ckDot)
      of "addRootRef":
        effects.add fixedEffect(ckT) # adds a type reference
      of "skip", "skipToEnd", "skipParRi", "inc", "swap",
         "endRead", "assert", "registerLocal", "registerLocalPtrOf",
         "openScope", "closeScope", "openProcScope", "registerParams",
         "publish", "mgetOrPut":
        discard # non-output operations
      of "genObjectTypes":
        return unknownEffect()
      else:
        # Check if it's a known proc in the graph
        if callName in graph.procs:
          let pe = graph.procs[callName]
          effects.add pe.effect
        else:
          # Unknown call
          return unknownEffect()
      skip n

    of "if", "when":
      # Analyze all branches — they must produce the same effect
      let ifEffect = analyzeIfBranches(graph, n)
      if ifEffect.kind == ekUnknown:
        return unknownEffect()
      effects.add ifEffect
      skip n

    of "case":
      # Case statements in bodies — treat like if
      return unknownEffect()

    of "while":
      # while n.kind != ParRi: f(dest, n) → repeat
      let loopEffect = analyzeWhileLoop(graph, n)
      if loopEffect.kind == ekUnknown:
        return unknownEffect()
      effects.add loopEffect
      skip n

    of "for":
      # for i in 0..<N: f(dest, n) → counted repeat
      let forEffect = analyzeForLoop(graph, n)
      if forEffect.kind == ekUnknown:
        return unknownEffect()
      effects.add forEffect
      skip n

    of "discard", "var", "let", "const", "asgn", "comment":
      skip n # non-output statements
    else:
      return unknownEffect()

  return seqEffect(effects)

proc analyzeIfBranches*(graph: EffectGraph; n: Cursor): Effect =
  ## Analyze an if/when statement. All branches must produce compatible effects.
  var c = n
  if c.kind != ParLe: return unknownEffect()
  inc c # skip (if

  var allEffects: seq[Effect] = @[]
  var hasElse = false

  while c.kind != ParRi:
    if c.kind != ParLe:
      skip c
      continue
    let branchTag = pool.tags[c.tag]
    case branchTag
    of "elif":
      inc c # skip (elif
      skip c # skip condition
      # The body is the second child — should be (stmts ...)
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        allEffects.add analyzeStmtsBody(graph, c)
      else:
        return unknownEffect()
      skip c # skip body
      if c.kind == ParRi: inc c # close elif
    of "else":
      hasElse = true
      inc c # skip (else
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        allEffects.add analyzeStmtsBody(graph, c)
      else:
        return unknownEffect()
      skip c # skip body
      if c.kind == ParRi: inc c # close else
    else:
      skip c

  if allEffects.len == 0:
    return emptyEffect()

  # If no else branch, the if might produce nothing on the else path
  if not hasElse:
    allEffects.add emptyEffect()

  # Check all branches produce the same flattened count
  var combined = allEffects[0]
  for i in 1..<allEffects.len:
    combined = branchEffect(combined, allEffects[i])
  return combined

proc analyzeWhileLoop*(graph: EffectGraph; n: Cursor): Effect =
  ## Analyze `while n.kind != ParRi: body` — produces 0+ children.
  var c = n
  if c.kind != ParLe: return unknownEffect()
  inc c # skip (while
  skip c # skip condition
  # body
  if c.kind == ParLe and pool.tags[c.tag] == "stmts":
    let bodyEffect = analyzeStmtsBody(graph, c)
    let flat = flatten(bodyEffect)
    if flat.ok and flat.children.len > 0:
      # The loop body produces these children per iteration
      # Overall: repeated
      if flat.children.len == 1:
        return repeatEffect(flat.children[0])
      else:
        # Multiple children per iteration — still repeated
        return repeatEffect(ckAny)
    elif bodyEffect.kind == ekEmpty:
      return emptyEffect()
    else:
      return unknownEffect()
  else:
    return unknownEffect()

proc analyzeForLoop*(graph: EffectGraph; n: Cursor): Effect =
  ## Analyze `for i in 0..<N: body` — produces N*bodyEffect children.
  var c = n
  if c.kind != ParLe: return unknownEffect()
  inc c # skip (for

  # for loop structure in NIF: (for VARS RANGE BODY)
  # Skip the loop variable(s)
  skip c # loop var or unpackflat

  # The range expression: try to resolve as constant range
  let rangeCount = resolveConstRange(c)
  skip c # skip range

  # Body
  if c.kind == ParLe and pool.tags[c.tag] == "stmts":
    let bodyEffect = analyzeStmtsBody(graph, c)
    if rangeCount >= 0:
      let flat = flatten(bodyEffect)
      if flat.ok:
        let perIter = flat.children.len
        if perIter == 0:
          return emptyEffect()
        elif perIter == 1:
          return countedEffect(rangeCount, flat.children[0])
        else:
          # Multiple children per iteration
          var effects: seq[Effect] = @[]
          for iter in 0..<rangeCount:
            for k in flat.children:
              effects.add fixedEffect(k)
          return seqEffect(effects)
      else:
        return unknownEffect()
    else:
      # Unknown range — treat as repeat
      let flat = flatten(bodyEffect)
      if flat.ok and flat.children.len > 0:
        return repeatEffect(flat.children[0])
      else:
        return unknownEffect()
  else:
    return unknownEffect()

# ---- Building the SymContext from declarations ----

proc typeNameToKind(typeName: string): ChildKind =
  ## Map known type names to NIF child kinds.
  ## Distinct type names like ExprSymId/TypeSymId provide refinements.
  case typeName
  of "ExprSymId": ckX
  of "TypeSymId": ckT
  of "StmtSymId": ckS
  of "FieldSymId": ckY
  else: ckAny

proc declKindToChildKind(tag: string): ChildKind =
  ## Map a declaration construct to the kind of value its symbol represents.
  ## Only returns a specific kind for declarations whose category is unambiguous.
  ## For SymId-typed variables/fields, returns ckAny unless a distinct type provides refinement.
  case tag
  of "type": ckT
  else: ckAny  # without distinct type refinement, we can't know the kind

proc extractFieldType(n: Cursor): string =
  ## From a field declaration `(fld NAME . . TYPE .)`, extract the TYPE name.
  var c = n
  if c.kind != ParLe: return ""
  inc c # skip (fld
  skip c # skip name
  skip c # skip export
  skip c # skip pragmas
  # now at type
  if c.kind == Ident:
    return pool.strings[c.litId]
  return ""

proc tagToSymKind(tag: string): ChildKind =
  ## When a Nim variable is used in `addSymDef` inside a `copyIntoKind TAG`,
  ## the TAG tells us what kind of NIF symbol that variable holds.
  case tag
  of "VarS", "LetS", "CursorS", "ResultS", "GvarS", "TvarS",
     "GletS", "TletS", "ConstS", "PatternvarS": ckX  # value bindings → expr
  of "TypeS": ckT  # type declaration → type
  of "ParamU": ckX  # parameter → expr
  of "FldY": ckY  # field → sym reference
  of "EfldY": ckY  # enum field → sym reference
  else: ckAny

proc findAddSymDefInBody(body: Cursor; tag: string; result: var SymContext) =
  ## Scan a (stmts ...) body for addSymDef calls and record the variable name
  ## with the kind determined by the enclosing tag.
  let kind = tagToSymKind(tag)
  if kind == ckAny: return
  var n = body
  if n.kind != ParLe: return
  if pool.tags[n.tag] != "stmts": return
  inc n
  while n.kind != ParRi:
    if n.kind == ParLe:
      let stmtTag = pool.tags[n.tag]
      if stmtTag in ["cmd", "call"]:
        # Check if this is an addSymDef call
        var peek = n
        inc peek
        var callee = ""
        if peek.kind == ParLe:
          callee = extractDotCallName(peek)
          skip peek
        elif peek.kind == Ident:
          callee = pool.strings[peek.litId]
          inc peek
          skip peek  # skip dest
        if callee == "addSymDef" and peek.kind == Ident:
          let varName = pool.strings[peek.litId]
          result[varName] = kind
          # Only the first addSymDef in a declaration body is the defining name
          return
    skip n

proc buildSymContext*(buf: var TokenBuf): SymContext =
  ## Scan the nifled source to understand what kind of NIF symbol each
  ## Nim variable represents, based on:
  ##
  ## 1. Field type refinements: `(fld name . . ExprSymId .)` → distinct type
  ## 2. addSymDef context: `addSymDef VAR` inside `copyIntoKind TypeS` → VAR is a type sym
  ## 3. Type declarations: `(type NAME ...)` → NAME is a type
  result = initTable[string, ChildKind]()
  var n = beginRead(buf)
  var nested = 0
  if n.kind != ParLe: return
  inc nested
  inc n
  while nested > 0:
    case n.kind
    of ParLe:
      let tag = pool.tags[n.tag]

      # Field type refinements via distinct types
      if tag == "fld":
        let typeName = extractFieldType(n)
        let refined = typeNameToKind(typeName)
        if refined != ckAny:
          var p = n
          inc p
          if p.kind == Ident:
            result[pool.strings[p.litId]] = refined

      # Type declarations
      elif tag == "type":
        var p = n
        inc p
        if p.kind == Ident:
          result[pool.strings[p.litId]] = ckT

      # copyIntoKind/buildTree calls — scan their body for addSymDef
      elif tag in ["cmd", "call"]:
        var peek = n
        inc peek
        var copyTag = ""
        if peek.kind == ParLe:
          let callee = extractDotCallName(peek)
          if callee in ["copyIntoKind", "buildTree"]:
            skip peek  # skip (dot ...)
            if peek.kind == Ident:
              copyTag = pool.strings[peek.litId]
              skip peek  # skip tag
              skip peek  # skip info
              # Find the stmts body
              while peek.kind != ParRi:
                if peek.kind == ParLe and pool.tags[peek.tag] == "stmts":
                  findAddSymDefInBody(peek, copyTag, result)
                  break
                skip peek
        elif peek.kind == Ident:
          let callee = pool.strings[peek.litId]
          if callee in ["copyIntoKind", "buildTree"]:
            inc peek
            skip peek  # skip dest
            if peek.kind == Ident:
              copyTag = pool.strings[peek.litId]
              skip peek  # skip tag
              skip peek  # skip info
              while peek.kind != ParRi:
                if peek.kind == ParLe and pool.tags[peek.tag] == "stmts":
                  findAddSymDefInBody(peek, copyTag, result)
                  break
                skip peek

      inc nested
      inc n
    of ParRi:
      dec nested
      inc n
    else:
      inc n

# ---- Parsing ensuresNif annotations ----

proc predicateToKind(predName: string): ChildKind =
  case predName
  of "addedExpr": ckX
  of "addedType": ckT
  of "addedStmt": ckS
  of "addedDef": ckD
  of "addedSym": ckY
  of "addedLit": ckLit
  of "addedAny": ckAny
  of "addedDot": ckDot
  of "addedNested": ckNested
  else: ckAny

proc extractEnsuresNif(procCursor: Cursor): Effect =
  ## Scan a proc declaration for an `ensuresNif` pragma annotation.
  ## Returns nil if no annotation found.
  ## The proc structure is: (proc NAME ... (pragmas ... (kv ensuresNif (call PRED ARG))) ... (stmts ...))
  var c = procCursor
  if c.kind != ParLe: return nil
  inc c # skip (proc
  # Walk children looking for (pragmas ...)
  while c.kind != ParRi:
    if c.kind == ParLe and pool.tags[c.tag] == "pragmas":
      # Found pragmas — scan for (kv ensuresNif ...)
      var p = c
      inc p # skip (pragmas
      while p.kind != ParRi:
        if p.kind == ParLe and pool.tags[p.tag] == "kv":
          var kv = p
          inc kv # skip (kv
          if kv.kind == Ident and pool.strings[kv.litId] == "ensuresNif":
            skip kv # skip "ensuresNif"
            # Next should be (call PREDICATE ARG)
            if kv.kind == ParLe and pool.tags[kv.tag] == "call":
              var call = kv
              inc call # skip (call
              if call.kind == Ident:
                let predName = pool.strings[call.litId]
                if predName == "addedNothing":
                  return emptyEffect()
                else:
                  return fixedEffect(predicateToKind(predName))
        skip p
      # No ensuresNif found in this pragmas block
      skip c
    else:
      skip c
  return nil

# ---- Building the full graph for a file ----

proc findProcBodies*(buf: var TokenBuf): seq[(string, Cursor)] =
  ## Find all proc declarations in the nifled source and return
  ## (procName, bodyStmtsCursor) pairs.
  result = @[]
  var n = beginRead(buf)
  var nested = 0
  assert n.kind == ParLe
  inc nested
  inc n
  while nested > 0:
    case n.kind
    of ParLe:
      let tag = pool.tags[n.tag]
      if tag == "proc" or tag == "func" or tag == "method" or
         tag == "converter" or tag == "iterator":
        # Found a proc declaration. Extract name and find the body stmts.
        var p = n
        inc p # skip (proc
        var name = ""
        if p.kind == Ident:
          name = pool.strings[p.litId]
        if name.len > 0:
          result.add (name, n)
      inc nested
      inc n
    of ParRi:
      dec nested
      inc n
    else:
      inc n

proc buildEffectGraph*(buf: var TokenBuf): EffectGraph =
  ## Build the effect graph for all procs in a nifled source file.
  ## Also builds a SymContext from declarations for refined addSymUse classification.
  ## Each proc's effect describes what it adds to its `dest` parameter.
  result = EffectGraph(procs: initTable[string, ProcEffect](),
                       symContext: buildSymContext(buf))

  # First pass: find all proc declarations
  # We process them in order so that forward-declared procs get
  # filled in when their body is encountered.
  let procList = findProcBodies(buf)

  # First, extract ensuresNif annotations.
  var annotations = initTable[string, Effect]()
  for (name, procCursor) in procList:
    let annotEffect = extractEnsuresNif(procCursor)
    if annotEffect != nil:
      annotations[name] = annotEffect
      # Register the annotation as the proc's effect for call-site resolution
      result.procs[name] = ProcEffect(name: name, effect: annotEffect,
                                       annotated: true)

  # Then derive effects from bodies, iterating to resolve dependencies.
  const MaxIterations = 5
  for iteration in 0..<MaxIterations:
    var changed = false
    for (name, procCursor) in procList:
      # Find the body of this proc — it's the last (stmts ...) child
      var c = procCursor
      inc c # skip (proc
      var lastStmts = c
      var foundStmts = false
      while c.kind != ParRi:
        if c.kind == ParLe and pool.tags[c.tag] == "stmts":
          lastStmts = c
          foundStmts = true
        skip c

      if not foundStmts:
        continue # forward declaration or external proc

      let effect = analyzeStmtsBody(result, lastStmts)

      if name in annotations:
        discard # annotation takes priority; cross-check done after all iterations
      else:
        # Non-annotated proc: use derived effect
        let prev = result.procs.getOrDefault(name)
        if prev.effect == nil or prev.effect.kind == ekUnknown:
          if effect.kind != ekUnknown:
            result.procs[name] = ProcEffect(name: name, effect: effect)
            changed = true
        elif prev.effect != effect:
          if effect.kind != ekUnknown:
            result.procs[name] = ProcEffect(name: name, effect: effect)
            changed = true

    if not changed:
      break

  # Cross-check: verify annotated procs' body effects match their annotations.
  for (name, procCursor) in procList:
    if name notin annotations: continue
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

    let effect = analyzeStmtsBody(result, lastStmts)
    if effect.kind == ekUnknown: continue

    let annot = annotations[name]
    let annotFlat = flatten(annot)
    let bodyFlat = flatten(effect)
    if annotFlat.ok and bodyFlat.ok:
      if annotFlat.children.len != bodyFlat.children.len:
        result.mismatches.add AnnotationMismatch(
          procName: name, annotation: annot, derived: effect)
      else:
        for i in 0..<annotFlat.children.len:
          let a = annotFlat.children[i]
          let b = bodyFlat.children[i]
          # ckAny in annotation means "don't care". ckAny in body means
          # "unknown" — if the annotation claims a specific kind, the body
          # must provably produce that kind, not just "something".
          if a == ckAny:
            discard # annotation doesn't care
          elif b == ckAny or a != b:
            result.mismatches.add AnnotationMismatch(
              procName: name, annotation: annot, derived: effect)
            break
