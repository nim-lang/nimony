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
import ".." / nimony / nimony_model

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

  ProcEffect* = ref object
    name*: string
    effect*: Effect  ## what it adds to dest
    annotated*: bool ## true if effect comes from ensuresNif annotation
    wrapsInput*: bool ## true if it does copyInto dest, n: (preserves tag)
    destLv*: Cursor   ## lvalue of the output variable (e.g. bare `dest` or `c.dest`)
    cursorLvs*: seq[Cursor]  ## lvalues of input cursor variables (e.g. `n`, `f`, `a`)
    destIsParam*: bool ## true if dest is a direct parameter (not accessed via context field)

  SymContext* = Table[string, ChildKind]
    ## Maps identifiers (field names, variable names) to their NIF kind
    ## based on their declaration context or type refinement.

  AnnotationMismatch* = object
    procName*: string
    annotation*: Effect
    derived*: Effect

  ProcInfo* = object
    name*: string
    procCursor*: Cursor   ## points to the (proc ...) node
    paramsPos*: Cursor    ## points to (params ...), invalid if hasBody is false
    bodyPos*: Cursor      ## points to the last (stmts ...) child
    hasBody*: bool

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

proc sameEffect*(a, b: Effect): bool =
  ## Structural equality — the default ref `==` only compares pointer identity,
  ## which makes the fixed-point iteration loop unable to detect convergence.
  if a.isNil and b.isNil: return true
  if a.isNil or b.isNil: return false
  if a.kind != b.kind: return false
  case a.kind
  of ekFixed: a.childKind == b.childKind
  of ekRepeat: a.repeatKind == b.repeatKind
  of ekCounted: a.count == b.count and a.countedKind == b.countedKind
  of ekBranch: sameEffect(a.thenEffect, b.thenEffect) and
               sameEffect(a.elseEffect, b.elseEffect)
  of ekSeq:
    if a.children.len != b.children.len: return false
    for i in 0..<a.children.len:
      if not sameEffect(a.children[i], b.children[i]): return false
    true
  of ekEmpty: true
  of ekUnknown: true

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

proc extractLastDotField*(c: Cursor): string =
  ## From `(dot obj fieldName)`, extract the innermost field name.
  result = ""
  if c.kind != ParLe: return
  var n = c
  if pool.tags[n.tag] != "dot": return
  inc n
  skip n # skip receiver
  if n.kind == Ident:
    result = pool.strings[n.litId]

proc extractDotReceiver*(c: Cursor): Cursor =
  ## From `(dot receiver field)`, return a Cursor to the receiver lvalue.
  ## Works for nested dots: `(dot (dot c dest) field)` returns cursor to `(dot c dest)`.
  if c.kind != ParLe: return default(Cursor)
  var n = c
  if pool.tags[n.tag] != "dot": return default(Cursor)
  inc n
  return n  # cursor at the receiver (Ident or nested dot)

proc equalLvalues*(a, b: Cursor): bool =
  ## Structural comparison of two lvalue expressions.
  ## Lvalues are either bare Ident nodes or (dot recv field) expressions (potentially nested).
  if a.kind != b.kind: return false
  case a.kind
  of Ident:
    return a.litId == b.litId
  of ParLe:
    if pool.tags[a.tag] != "dot" or pool.tags[b.tag] != "dot":
      return false
    var ca = a
    var cb = b
    inc ca # skip (dot
    inc cb
    # Compare receivers recursively
    if not equalLvalues(ca, cb): return false
    skip ca
    skip cb
    # Compare field names
    if ca.kind != Ident or cb.kind != Ident: return false
    return ca.litId == cb.litId
  else:
    return false

proc lvalueToStr*(c: Cursor): string =
  ## Convert an lvalue Cursor to a human-readable string for error messages.
  case c.kind
  of Ident:
    return pool.strings[c.litId]
  of ParLe:
    if pool.tags[c.tag] == "dot":
      var n = c
      inc n
      result = lvalueToStr(n)
      skip n
      if n.kind == Ident:
        result.add "."
        result.add pool.strings[n.litId]
    else:
      return "?"
  else:
    return "?"

proc isLvalue*(c: Cursor): bool =
  ## Check if cursor points at an analyzable lvalue (Ident or dot expression).
  c.kind == Ident or (c.kind == ParLe and pool.tags[c.tag] == "dot")

proc callMentionsDest*(n: Cursor; destLv: Cursor): bool =
  ## Check if a call/cmd node writes to the tracked dest lvalue.
  ## For method calls, checks the receiver. For all calls, scans arguments.
  ## Returns true if destLv is nil (no tracking).
  if cursorIsNil(destLv): return true
  var c = n
  if c.kind != ParLe: return false
  inc c
  if c.kind == ParLe:
    # Method call: (call (dot RECV callee) args...)
    if pool.tags[c.tag] == "dot":
      var dot = c
      inc dot # skip (dot — now at receiver
      if equalLvalues(dot, destLv): return true
    skip c
  elif c.kind == Ident:
    # Function call: (call callee DEST args...)
    inc c  # skip callee
  # Scan remaining arguments
  while c.kind != ParRi:
    if equalLvalues(c, destLv): return true
    skip c
  return false

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

proc extractCallMeta(n: Cursor; targetLv: Cursor): (string, string, bool) =
  ## Single-pass extraction for call/cmd nodes:
  ## - callee name
  ## - first arg name (string, for symContext lookup)
  ## - whether targetLv lvalue is mentioned as receiver/argument
  var c = n
  if c.kind != ParLe: return ("", "", false)
  inc c
  var callee = ""
  var mentionsTarget = cursorIsNil(targetLv)
  if c.kind == ParLe:
    # Method call: (call (dot RECV callee) args...)
    callee = extractDotCallName(c)
    if not mentionsTarget and pool.tags[c.tag] == "dot":
      var dot = c
      inc dot # skip (dot — now at receiver
      if equalLvalues(dot, targetLv):
        mentionsTarget = true
    skip c
  elif c.kind == Ident:
    # Function call: (call callee DEST args...)
    callee = pool.strings[c.litId]
    inc c

  var firstArg = ""
  if c.kind == Ident:
    firstArg = pool.strings[c.litId]
  elif c.kind == ParLe:
    firstArg = extractLastDotField(c)

  while c.kind != ParRi:
    if not mentionsTarget:
      if equalLvalues(c, targetLv):
        mentionsTarget = true
    skip c
  (callee, firstArg, mentionsTarget)

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

proc analyzeIfBranches*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect
proc analyzeCaseBranches*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect
proc analyzeWhileLoop*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect
proc analyzeForLoop*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect

proc analyzeStmtsBody*(graph: EffectGraph; body: Cursor; destLv: Cursor): Effect =
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
      let (callName, firstArg, writesToDest) = extractCallMeta(n, destLv)

      case callName
      of "addDotToken":
        if writesToDest: effects.add fixedEffect(ckDot)
      of "addSymDef":
        if writesToDest: effects.add fixedEffect(ckD)
      of "addSymUse", "copyIntoSymUse":
        if writesToDest:
          # Try to classify via sym context (field type refinement or decl context)
          if firstArg.len > 0 and firstArg in graph.symContext:
            effects.add fixedEffect(graph.symContext[firstArg])
          else:
            effects.add fixedEffect(ckAny)
      of "addIntLit", "addUIntLit", "addIntVal", "addStrLit",
         "addCharLit", "addFloatLit":
        if writesToDest: effects.add fixedEffect(ckLit)
      of "addParPair":
        if writesToDest: effects.add fixedEffect(enumSuffixToKind(firstArg))
      of "copyIntoKind", "copyIntoKinds", "buildTree":
        if writesToDest: effects.add fixedEffect(enumSuffixToKind(firstArg))
      of "copyInto":
        if writesToDest: effects.add fixedEffect(ckAny) # copies one node from input
      of "takeTree", "takeToken", "copyTree", "addEmpty", "addToken",
         "addSubtree", "addTarget":
        if writesToDest: effects.add fixedEffect(ckAny)
      of "addParLe":
        if writesToDest: effects.add fixedEffect(ckAny) # manual node, paired with addParRi
      of "addParRi", "takeParRi":
        discard # closing paren, not a child
      of "addEmpty2":
        if writesToDest:
          # addEmpty2 adds two DotTokens
          effects.add fixedEffect(ckDot)
          effects.add fixedEffect(ckDot)
      of "addRootRef":
        if writesToDest: effects.add fixedEffect(ckT) # adds a type reference
      of "addIdent":
        if writesToDest: effects.add fixedEffect(ckAny)
      of "skip", "skipToEnd", "skipParRi", "inc", "swap",
         "endRead", "assert", "registerLocal", "registerLocalPtrOf",
         "openScope", "closeScope", "openProcScope", "registerParams",
         "publish", "mgetOrPut":
        discard # non-output operations
      of "genObjectTypes":
        if writesToDest: return unknownEffect()
      else:
        # Check if it's a known proc in the graph
        if callName in graph.procs:
          let pe = graph.procs[callName]
          if writesToDest:
            effects.add pe.effect
          elif not pe.destIsParam:
            # Proc accesses dest through context field (c.dest).
            # The shared context carries dest, so always include.
            effects.add pe.effect
          # else: proc takes dest as parameter but call doesn't pass dest → skip
        elif writesToDest:
          # Unknown call that mentions dest
          return unknownEffect()
        # else: call doesn't mention dest, skip it
      skip n

    of "if", "when":
      # Analyze all branches — they must produce the same effect
      let ifEffect = analyzeIfBranches(graph, n, destLv)
      if ifEffect.kind == ekUnknown:
        return unknownEffect()
      effects.add ifEffect
      skip n

    of "case":
      let caseEffect = analyzeCaseBranches(graph, n, destLv)
      if caseEffect.kind == ekUnknown:
        return unknownEffect()
      effects.add caseEffect
      skip n

    of "while":
      # while n.kind != ParRi: f(dest, n) → repeat
      let loopEffect = analyzeWhileLoop(graph, n, destLv)
      if loopEffect.kind == ekUnknown:
        return unknownEffect()
      effects.add loopEffect
      skip n

    of "for":
      # for i in 0..<N: f(dest, n) → counted repeat
      let forEffect = analyzeForLoop(graph, n, destLv)
      if forEffect.kind == ekUnknown:
        return unknownEffect()
      effects.add forEffect
      skip n

    of "discard", "var", "let", "const", "asgn", "comment":
      skip n # non-output statements
    else:
      return unknownEffect()

  return seqEffect(effects)

proc analyzeIfBranches*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect =
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
      skip c, SkipCond # skip condition
      # The body is the second child — should be (stmts ...)
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        allEffects.add analyzeStmtsBody(graph, c, destLv)
      else:
        return unknownEffect()
      skip c, SkipBody # skip body
      if c.kind == ParRi: inc c # close elif
    of "else":
      hasElse = true
      inc c # skip (else
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        allEffects.add analyzeStmtsBody(graph, c, destLv)
      else:
        return unknownEffect()
      skip c, SkipBody # skip body
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

proc analyzeCaseBranches*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect =
  ## Analyze a case statement. All branches must produce compatible effects.
  var c = n
  if c.kind != ParLe: return unknownEffect()
  inc c # skip (case
  skip c, SkipValue # skip discriminator

  var allEffects: seq[Effect] = @[]
  var hasElse = false

  while c.kind != ParRi:
    if c.kind != ParLe:
      skip c
      continue
    let branchTag = pool.tags[c.tag]
    case branchTag
    of "of":
      inc c # skip (of
      skip c # skip ranges
      # The body is the second child — should be (stmts ...)
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        allEffects.add analyzeStmtsBody(graph, c, destLv)
      else:
        return unknownEffect()
      skip c, SkipBody # skip body
      if c.kind == ParRi: inc c # close of
    of "else":
      hasElse = true
      inc c # skip (else
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        allEffects.add analyzeStmtsBody(graph, c, destLv)
      else:
        return unknownEffect()
      skip c, SkipBody # skip body
      if c.kind == ParRi: inc c # close else
    else:
      skip c

  if allEffects.len == 0:
    return emptyEffect()

  # If no else branch, the case might produce nothing on the unmatched path
  if not hasElse:
    allEffects.add emptyEffect()

  result = allEffects[0]
  for i in 1..<allEffects.len:
    result = branchEffect(result, allEffects[i])

proc analyzeWhileLoop*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect =
  ## Analyze `while n.kind != ParRi: body` — produces 0+ children.
  var c = n
  if c.kind != ParLe: return unknownEffect()
  inc c # skip (while
  skip c, SkipCond # skip condition
  # body
  if c.kind == ParLe and pool.tags[c.tag] == "stmts":
    let bodyEffect = analyzeStmtsBody(graph, c, destLv)
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

proc analyzeForLoop*(graph: EffectGraph; n: Cursor; destLv: Cursor): Effect =
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
    let bodyEffect = analyzeStmtsBody(graph, c, destLv)
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
  skip c, SkipName # skip name
  skip c, SkipExport # skip export
  skip c, SkipPragmas # skip pragmas
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

proc extractEnsuresNif(procCursor: Cursor): (Effect, string) =
  ## Scan a proc declaration for an `ensuresNif` pragma annotation.
  ## Returns (nil, "") if no annotation found.
  ## Returns (effect, destVarName) where destVarName is the argument to the predicate.
  ## The proc structure is: (proc NAME ... (pragmas ... (kv ensuresNif (call PRED ARG))) ... (stmts ...))
  var c = procCursor
  if c.kind != ParLe: return (nil, "")
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
                  return (emptyEffect(), "")
                else:
                  inc call # skip predicate name, now at the argument
                  var destName = ""
                  if call.kind == Ident:
                    destName = pool.strings[call.litId]
                  elif call.kind == ParLe:
                    destName = extractLastDotField(call)
                  return (fixedEffect(predicateToKind(predName)), destName)
        skip p
      # No ensuresNif found in this pragmas block
      skip c
    else:
      skip c
  return (nil, "")

proc extractRequiresNif(procCursor: Cursor): (ChildKind, string) =
  ## Scan a proc declaration for a `requiresNif` pragma annotation.
  ## Returns (cursorKind, cursorVarName) or (ckAny, "") if no annotation.
  var c = procCursor
  if c.kind != ParLe: return (ckAny, "")
  inc c # skip (proc
  while c.kind != ParRi:
    if c.kind == ParLe and pool.tags[c.tag] == "pragmas":
      var p = c
      inc p
      while p.kind != ParRi:
        if p.kind == ParLe and pool.tags[p.tag] == "kv":
          var kv = p
          inc kv
          if kv.kind == Ident and pool.strings[kv.litId] == "requiresNif":
            skip kv
            if kv.kind == ParLe and pool.tags[kv.tag] == "call":
              var call = kv
              inc call
              if call.kind == Ident:
                let predName = pool.strings[call.litId]
                let kind = case predName
                  of "isExpr": ckX
                  of "isType": ckT
                  of "isStmt": ckS
                  else: ckAny
                inc call # skip predicate name
                var cursorName = ""
                if call.kind == Ident:
                  cursorName = pool.strings[call.litId]
                elif call.kind == ParLe:
                  cursorName = extractLastDotField(call)
                return (kind, cursorName)
        skip p
      skip c
    else:
      skip c
  return (ckAny, "")

proc detectDestIsParam*(destLv: Cursor): bool =
  ## Check whether dest lvalue is a direct parameter (bare ident like `dest`)
  ## or through a context field (dot expression like `c.dest`).
  ## Returns true for direct parameter, false for context access.
  if cursorIsNil(destLv): return true
  destLv.kind == Ident

proc detectDestLvalue*(body: Cursor; hintName: string): Cursor =
  ## Deep-scan a proc body for calls that reference `hintName` as a dest variable.
  ## Returns the full lvalue Cursor (bare `dest` or `(dot c dest)` etc.).
  ## Returns nil Cursor if not found.
  var c = body
  if c.kind != ParLe: return default(Cursor)
  var nested = 0
  inc nested
  inc c
  while nested > 0:
    case c.kind
    of ParLe:
      let tag = pool.tags[c.tag]
      if tag in ["call", "cmd"]:
        var peek = c
        inc peek
        if peek.kind == ParLe:
          # Method call: (call (dot RECV callee) args...)
          if pool.tags[peek.tag] == "dot":
            var dot = peek
            inc dot # skip (dot — now at receiver
            if dot.kind == Ident and pool.strings[dot.litId] == hintName:
              return dot  # bare `dest` as receiver
            elif dot.kind == ParLe and pool.tags[dot.tag] == "dot":
              if extractLastDotField(dot) == hintName:
                return dot  # `(dot c dest)` as receiver
        elif peek.kind == Ident:
          inc peek  # skip callee
          # Scan args for dest reference
          while peek.kind != ParRi:
            if peek.kind == Ident and pool.strings[peek.litId] == hintName:
              return peek  # bare `dest` as argument
            elif peek.kind == ParLe and pool.tags[peek.tag] == "dot":
              if extractLastDotField(peek) == hintName:
                return peek  # `(dot c dest)` as argument
            skip peek
      inc nested
      inc c
    of ParRi:
      dec nested
      inc c
    else:
      inc c
  return default(Cursor)

proc detectWrapsInput*(graph: EffectGraph; body: Cursor; destLv: Cursor): bool =
  ## Check if the proc body uses copyInto dest, n: at the top level,
  ## indicating it preserves the input tag (the "preservation property").
  var c = body
  if c.kind != ParLe or pool.tags[c.tag] != "stmts": return false
  inc c
  while c.kind != ParRi:
    if c.kind == ParLe:
      let tag = pool.tags[c.tag]
      if tag in ["call", "cmd"]:
        let (callName, _, mentionsDest) = extractCallMeta(c, destLv)
        if callName == "copyInto" and mentionsDest:
          return true
    skip c
  return false

proc detectCursorLvs*(body: Cursor): seq[Cursor] =
  ## Deep-scan a proc body for cursor-advancing operations (inc, skip,
  ## copyInto, takeTree, etc.) and return all distinct cursor lvalues found.
  const IgnoredNames = ["dest", "c", "result", "info", "nested"]
  result = @[]
  var c = body
  if c.kind != ParLe: return
  var nested = 0
  inc nested
  inc c
  template addIfNew(lv: Cursor) =
    var found = false
    for existing in result:
      if equalLvalues(existing, lv):
        found = true
        break
    if not found:
      result.add lv
  while nested > 0:
    case c.kind
    of ParLe:
      let tag = pool.tags[c.tag]
      if tag in ["call", "cmd"]:
        let (callName, _) = extractCallInfo(c)
        if callName in ["inc", "skip", "skipParRi", "skipToEnd",
                        "copyInto", "takeTree", "takeToken", "takeParRi"]:
          # Extract the cursor argument
          var peek = c
          inc peek
          if peek.kind == ParLe:
            # Method call: (call (dot RECV callee) args...)
            let recv = extractDotReceiver(peek)
            if not cursorIsNil(recv) and isLvalue(recv):
              if recv.kind != Ident or pool.strings[recv.litId] notin IgnoredNames:
                addIfNew recv
            skip peek
            # Also check first arg after dot
            if peek.kind == Ident:
              let arg = pool.strings[peek.litId]
              if arg notin IgnoredNames:
                addIfNew peek
          elif peek.kind == Ident:
            let callee = pool.strings[peek.litId]
            inc peek  # skip callee
            if callee in ["inc", "skip", "skipParRi", "skipToEnd"]:
              # First arg is the cursor
              if peek.kind == Ident:
                let arg = pool.strings[peek.litId]
                if arg notin IgnoredNames:
                  addIfNew peek
            elif callee in ["copyInto", "takeTree", "takeToken", "takeParRi"]:
              # Second arg (after dest) is the cursor
              skip peek  # skip dest
              if peek.kind == Ident:
                let arg = pool.strings[peek.litId]
                if arg notin IgnoredNames:
                  addIfNew peek
      inc nested
      inc c
    of ParRi:
      dec nested
      inc c
    else:
      inc c

# ---- Building the full graph for a file ----

proc locateProcChildren(info: var ProcInfo) =
  ## From a proc cursor, locate its params and body children.
  var c = info.procCursor
  inc c # skip (proc
  while c.kind != ParRi:
    if c.kind == ParLe:
      let tag = pool.tags[c.tag]
      if tag == "params":
        info.paramsPos = c
      elif tag == "stmts":
        info.bodyPos = c
        info.hasBody = true
    skip c

proc findProcs*(buf: var TokenBuf): seq[ProcInfo] =
  ## Find all proc declarations with their params and body positions.
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
        var p = n
        inc p # skip (proc
        var name = ""
        if p.kind == Ident:
          name = pool.strings[p.litId]
        if name.len > 0:
          result.add ProcInfo(name: name, procCursor: n)
      inc nested
      inc n
    of ParRi:
      dec nested
      inc n
    else:
      inc n
  # Locate params and body from cached cursors
  for info in result.mitems:
    locateProcChildren(info)

proc buildEffectGraph*(buf: var TokenBuf; procList: seq[ProcInfo]): EffectGraph =
  ## Build the effect graph for all procs in a nifled source file.
  ## `procList` comes from `findProcs` — body positions are already cached.
  ## Each proc's effect describes what it adds to its `dest` parameter.
  result = EffectGraph(procs: initTable[string, ProcEffect](),
                       symContext: buildSymContext(buf))

  # Extract ensuresNif and requiresNif annotations.
  var annotations = initTable[string, Effect]()
  var destHints = initTable[string, string]()   # procName -> dest hint name
  var cursorHints = initTable[string, string]()  # procName -> cursor hint name
  var destLvs = initTable[string, Cursor]()     # procName -> detected dest lvalue
  var allCursorLvs = initTable[string, seq[Cursor]]() # procName -> all cursor lvalues
  for p in procList:
    let (annotEffect, destName) = extractEnsuresNif(p.procCursor)
    if annotEffect != nil:
      annotations[p.name] = annotEffect
      let hint = if destName.len > 0: destName else: "dest"
      destHints[p.name] = hint
    let (_, cursorName) = extractRequiresNif(p.procCursor)
    if cursorName.len > 0:
      cursorHints[p.name] = cursorName

  # Detect actual lvalue cursors from bodies using hints.
  for p in procList:
    if not p.hasBody: continue
    let hint = destHints.getOrDefault(p.name, "dest")
    let lv = detectDestLvalue(p.bodyPos, hint)
    if not cursorIsNil(lv):
      destLvs[p.name] = lv
    # Collect cursor lvalues: from annotation hint + auto-detection
    var clvs: seq[Cursor] = @[]
    let cursorHint = cursorHints.getOrDefault(p.name, "")
    if cursorHint.len > 0:
      let clv = detectDestLvalue(p.bodyPos, cursorHint)
      if not cursorIsNil(clv):
        clvs.add clv
    else:
      clvs = detectCursorLvs(p.bodyPos)
    if clvs.len > 0:
      allCursorLvs[p.name] = clvs

  # Register annotated procs.
  for p in procList:
    if p.name notin annotations: continue
    let destLv = destLvs.getOrDefault(p.name)
    result.procs[p.name] = ProcEffect(name: p.name, effect: annotations[p.name],
                                       annotated: true, destLv: destLv)

  # Derive effects from bodies, iterating to resolve inter-proc dependencies.
  const MaxIterations = 5
  for iteration in 0..<MaxIterations:
    var changed = false
    for p in procList:
      if not p.hasBody: continue
      if p.name in annotations: continue

      let destLv = destLvs.getOrDefault(p.name)
      let effect = analyzeStmtsBody(result, p.bodyPos, destLv)
      if effect.kind == ekUnknown: continue

      let prev = result.procs.getOrDefault(p.name)
      if prev == nil or prev.effect == nil or
          prev.effect.kind == ekUnknown or not sameEffect(prev.effect, effect):
        let dip = detectDestIsParam(destLv)
        let clvs = allCursorLvs.getOrDefault(p.name)
        result.procs[p.name] = ProcEffect(name: p.name, effect: effect,
                                           destLv: destLv, cursorLvs: clvs,
                                           destIsParam: dip)
        changed = true

    if not changed:
      break

  # After convergence, compute wrapsInput once per proc.
  for p in procList:
    if not p.hasBody: continue
    if p.name notin result.procs: continue
    let pe = result.procs[p.name]
    if pe.annotated: continue
    pe.wrapsInput = detectWrapsInput(result, p.bodyPos, pe.destLv)

  # Cross-check: verify annotated procs' body effects match their annotations.
  for p in procList:
    if not p.hasBody: continue
    if p.name notin annotations: continue

    let destLv = destLvs.getOrDefault(p.name)
    let effect = analyzeStmtsBody(result, p.bodyPos, destLv)
    if effect.kind == ekUnknown: continue

    let annot = annotations[p.name]
    let annotFlat = flatten(annot)
    let bodyFlat = flatten(effect)
    if annotFlat.ok and bodyFlat.ok:
      if annotFlat.children.len != bodyFlat.children.len:
        result.mismatches.add AnnotationMismatch(
          procName: p.name, annotation: annot, derived: effect)
      else:
        for i in 0..<annotFlat.children.len:
          let a = annotFlat.children[i]
          let b = bodyFlat.children[i]
          if a == ckAny:
            discard
          elif b == ckAny or a != b:
            result.mismatches.add AnnotationMismatch(
              procName: p.name, annotation: annot, derived: effect)
            break

# ---- Cursor advancement analysis (preservation property) ----

type
  CursorState* = enum
    csNotAdvanced  ## cursor hasn't been moved on this path
    csAdvanced     ## cursor has been moved at least once
    csUnknown      ## can't determine

proc callIsNoReturn(callName: string): bool =
  ## Check if a call is known to never return (error/assertion handlers).
  callName in ["bug", "error", "quit", "raiseAssert", "doAssert", "assert"]

proc callAdvancesCursor(callName: string): bool =
  ## Check if a call is a known cursor-advancing operation.
  case callName
  of "inc", "skip", "skipParRi", "skipToEnd",
     "copyInto", "takeTree", "takeToken", "takeParRi", "copyTree":
    return true
  else:
    return false

proc analyzeIfCursorPaths*(graph: EffectGraph; n: Cursor; cursorLv: Cursor): CursorState
proc analyzeCaseCursorPaths*(graph: EffectGraph; n: Cursor; cursorLv: Cursor): CursorState

proc analyzeCursorPath*(graph: EffectGraph; body: Cursor; cursorLv: Cursor): CursorState =
  ## Analyze whether the cursor is advanced on all paths through a (stmts ...) body.
  ## Returns csAdvanced if every code path advances the cursor at least once,
  ## or if the path terminates via a noreturn call.
  if cursorIsNil(cursorLv): return csUnknown
  var n = body
  if n.kind != ParLe: return csUnknown
  if pool.tags[n.tag] != "stmts": return csUnknown
  inc n

  var advanced = false
  while n.kind != ParRi:
    if n.kind != ParLe:
      skip n
      continue
    let tag = pool.tags[n.tag]
    case tag
    of "call", "cmd":
      let (callName, _, mentionsCursor) = extractCallMeta(n, cursorLv)
      if mentionsCursor and callAdvancesCursor(callName):
        advanced = true
      elif callIsNoReturn(callName):
        advanced = true  # path terminates, no need to advance cursor
      elif callName in graph.procs:
        # Known proc that receives the cursor — assume it advances
        if mentionsCursor:
          advanced = true
      skip n
    of "if", "when":
      let state = analyzeIfCursorPaths(graph, n, cursorLv)
      if state == csAdvanced:
        advanced = true
      skip n
    of "case":
      let state = analyzeCaseCursorPaths(graph, n, cursorLv)
      if state == csAdvanced:
        advanced = true
      skip n
    of "while":
      # Analyze the while body — cursor might be advanced inside the loop
      var c = n
      inc c  # skip (while
      skip c, SkipCond  # skip condition
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        let state = analyzeCursorPath(graph, c, cursorLv)
        if state == csAdvanced: advanced = true
      skip n
    of "for":
      var c = n
      inc c  # skip (for
      skip c, SkipName  # skip loop var
      skip c, SkipValue  # skip range
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        let state = analyzeCursorPath(graph, c, cursorLv)
        if state == csAdvanced: advanced = true
      skip n
    else:
      skip n

  if advanced: csAdvanced else: csNotAdvanced

proc analyzeIfCursorPaths*(graph: EffectGraph; n: Cursor; cursorLv: Cursor): CursorState =
  ## Analyze an if/when — cursor must be advanced in ALL branches.
  ## Missing else means there's a path without advancement.
  var c = n
  if c.kind != ParLe: return csUnknown
  inc c  # skip (if

  var allAdvanced = true
  var hasElse = false
  while c.kind != ParRi:
    if c.kind != ParLe:
      skip c
      continue
    let branchTag = pool.tags[c.tag]
    case branchTag
    of "elif":
      inc c  # skip (elif
      skip c, SkipCond  # skip condition
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        if analyzeCursorPath(graph, c, cursorLv) != csAdvanced:
          allAdvanced = false
      skip c, SkipBody  # skip body
      if c.kind == ParRi: inc c  # close elif
    of "else":
      hasElse = true
      inc c  # skip (else
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        if analyzeCursorPath(graph, c, cursorLv) != csAdvanced:
          allAdvanced = false
      skip c, SkipBody  # skip body
      if c.kind == ParRi: inc c  # close else
    else:
      skip c

  if not hasElse:
    allAdvanced = false  # missing else = path without cursor advancement
  if allAdvanced: csAdvanced else: csNotAdvanced

proc analyzeCaseCursorPaths*(graph: EffectGraph; n: Cursor; cursorLv: Cursor): CursorState =
  ## Analyze a case — cursor must be advanced in ALL branches.
  var c = n
  if c.kind != ParLe: return csUnknown
  inc c  # skip (case
  skip c, SkipValue  # skip discriminator

  var allAdvanced = true
  var hasElse = false
  while c.kind != ParRi:
    if c.kind != ParLe:
      skip c
      continue
    let branchTag = pool.tags[c.tag]
    case branchTag
    of "of":
      inc c  # skip (of
      skip c  # skip match values/ranges
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        if analyzeCursorPath(graph, c, cursorLv) != csAdvanced:
          allAdvanced = false
      skip c, SkipBody  # skip body
      if c.kind == ParRi: inc c  # close of
    of "else":
      hasElse = true
      inc c  # skip (else
      if c.kind == ParLe and pool.tags[c.tag] == "stmts":
        if analyzeCursorPath(graph, c, cursorLv) != csAdvanced:
          allAdvanced = false
      skip c, SkipBody  # skip body
      if c.kind == ParRi: inc c  # close else
    else:
      skip c

  if not hasElse:
    allAdvanced = false
  if allAdvanced: csAdvanced else: csNotAdvanced
