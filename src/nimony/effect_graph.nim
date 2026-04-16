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
    wrapsInput*: bool ## true if it does copyInto dest, n: (preserves tag)

  EffectGraph* = object
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

proc extractCallInfo(n: Cursor): (string, string) =
  ## From a (cmd ...) or (call ...) node, extract callee name and first arg.
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
        effects.add fixedEffect(ckAny) # sym refs are context-dependent
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
  ## Each proc's effect describes what it adds to its `dest` parameter.
  result = EffectGraph(procs: initTable[string, ProcEffect]())

  # First pass: find all proc declarations
  # We process them in order so that forward-declared procs get
  # filled in when their body is encountered.
  let procList = findProcBodies(buf)

  # For each proc, analyze its body to derive the effect.
  # We iterate multiple times to resolve dependencies (proc A calls proc B).
  # In practice 2-3 iterations suffice since call graphs are shallow.
  const MaxIterations = 5
  for iteration in 0..<MaxIterations:
    var changed = false
    for (name, procCursor) in procList:
      # Find the body of this proc — it's the last child before ParRi
      # In nifled output: (proc NAME ... (stmts BODY))
      var c = procCursor
      inc c # skip (proc
      # Walk to find the last (stmts ...) child
      var lastStmts = c  # placeholder
      var foundStmts = false
      while c.kind != ParRi:
        if c.kind == ParLe and pool.tags[c.tag] == "stmts":
          lastStmts = c
          foundStmts = true
        skip c

      if not foundStmts:
        continue # forward declaration or external proc

      let effect = analyzeStmtsBody(result, lastStmts)

      let prev = result.procs.getOrDefault(name)
      if prev.effect == nil or prev.effect.kind == ekUnknown:
        if effect.kind != ekUnknown:
          result.procs[name] = ProcEffect(name: name, effect: effect)
          changed = true
      elif prev.effect != effect:
        # Effect changed (maybe refined)
        if effect.kind != ekUnknown:
          result.procs[name] = ProcEffect(name: name, effect: effect)
          changed = true

    if not changed:
      break
