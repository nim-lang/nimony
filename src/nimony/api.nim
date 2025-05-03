#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## API for plugins.

import std / [assertions, tables, sequtils]
include ".." / lib / nifprelude
import ".." / lib / stringviews

import ".." / models / [tags, nimony_tags, callconv_tags]
export nimony_tags, callconv_tags

import nimony_model, typenav, programs
export nimony_model, typenav

type
  VisitorMode* = enum
    AnalyzeOnly       ## Only analyze, do not copy tokens
    Passthrough       ## Copy all tokens unless intercepted

  VisitorState* = enum
    Normal
    InitValue
    RoutineBody

  Visitor* = object
    cache*: TypeCache
    mode*: VisitorMode
    dest*: TokenBuf          ## Destination buffer for Passthrough mode
    scopeDepth: int         ## Current parenthesis nesting level
    scopeCloseLevels: seq[int] ## Stack of depths where scopes should close
    root: Cursor            ## The starting cursor of the traversal
    state: VisitorState

proc traverseNode*(v: var Visitor; n: var Cursor)

# --- Helper Procs ---

proc checkCloseScope(v: var Visitor) =
  if v.scopeCloseLevels.len > 0 and v.scopeCloseLevels[^1] == v.scopeDepth:
    v.cache.closeScope()
    discard v.scopeCloseLevels.pop()

proc add*(v: var Visitor; n: var Cursor) =
  if v.mode == Passthrough:
    v.dest.add n
  inc n

proc addSubtree*(v: var Visitor; n: var Cursor) =
  if v.mode == Passthrough:
    v.dest.addSubtree n
  else:
    skip n

proc handleLocalDecl(v: var Visitor; n: var Cursor; kind: SymKind) =
  # Assumes n points to the start of the (let/var...)
  let declStart = n
  add v, n # head
  let name = n.symId
  add v, n # name
  addSubtree v, n # export marker
  addSubtree v, n # pragmas
  # 'n' now points to the type
  v.cache.registerLocal(name, kind, n)
  addSubtree v, n # type
  v.state = InitValue

proc handleRoutineDecl(v: var Visitor; n: var Cursor; kind: SymKind) =
  # Assumes n points to the start of the (proc/func...)
  let declStart = n
  let r = asRoutine(n) # Need nimony_model for this
  v.cache.openScope()
  # Register routine symbol itself (optional, but good practice)
  v.cache.registerLocal(r.name.symId, kind, r.params)
  v.cache.registerParams(r.name.symId, r.params)
  v.scopeCloseLevels.add(v.scopeDepth + 1) # Scope closes at the level of the routine's ')'
  v.state = RoutineBody

proc traverseNode*(v: var Visitor; n: var Cursor) =
  ## Recursively traverses the NIF structure starting from cursor `n`.
  ## Handles scope opening/closing and type registration.
  ## If `v.mode == Passthrough`, copies tokens to `v.dest`.
  if cursorIsNil(n) or n.kind == EofToken:
    return

  case n.kind
  of ParLe:
    let startNode = n
    let stmtk = n.stmtKind
    inc v.scopeDepth

    case stmtk
    of ScopeS:
      v.cache.openScope()
      v.scopeCloseLevels.add(v.scopeDepth)
      if v.mode == Passthrough: v.dest.add n
      inc n
      while n.kind != ParRi:
        traverseNode(v, n)
      # Closing ')' handled below

    of LetS, VarS, ConstS, TvarS, TletS, GvarS, GletS:
      # Need to handle this before recursing
      # Use a temporary cursor to avoid messing up 'n' for passthrough
      var localCursor = startNode
      handleLocalDecl(v, localCursor, cast[SymKind](stmtk))
      # 'n' remains at the start for passthrough or further processing
      # The handleLocalDecl already added tokens if needed and advanced its internal cursor
      # We need to advance 'n' past the handled declaration
      n = localCursor

    of ProcS, FuncS, MacroS, MethodS, ConverterS, IteratorS:
      # Similar to local decls, handle before standard recursion
      var routineCursor = startNode
      handleRoutineDecl(v, routineCursor, cast[SymKind](stmtk))
      n = routineCursor # Advance 'n' past the handled routine

    else:
      # Default handling for other ParLe kinds
      if v.mode == Passthrough: v.dest.add n
      inc n
      while n.kind != ParRi:
        traverseNode(v, n)
      # Closing ')' handled below

    # Handle the closing ParRi for the current ParLe
    if n.kind == ParRi:
      if v.mode == Passthrough: v.dest.add n
      checkCloseScope(v) # Check if a scope needs closing *before* decrementing depth
      dec v.scopeDepth
      inc n

  of ParRi:
    # This case should ideally only be reached if not consumed by ParLe handling
    # It indicates an unmatched ')' or structure error.
    when defined(debug):
      echo "Warning: Unexpected ParRi encountered outside ParLe handling at depth ", v.scopeDepth
    if v.mode == Passthrough: v.dest.add n
    # We might have scopes opened by non-ParLe constructs (less common, but possible?)
    checkCloseScope(v)
    if v.scopeDepth > 0: # Avoid going negative
      dec v.scopeDepth
    inc n


  else: # Handle non-parenthesis tokens (Symbol, IntLit, etc.)
    if v.mode == Passthrough: v.dest.add n
    inc n

# --- Entry Point ---

proc traverseModule*(root: Cursor; mode: VisitorMode = AnalyzeOnly; initialDestCap: int = 1024): Visitor =
  ## Creates a Visitor and traverses the NIF tree starting from `root`.
  result = Visitor(
    cache: createTypeCache(),
    mode: mode,
    dest: if mode == Passthrough: createTokenBuf(initialDestCap) else: default(TokenBuf),
    scopeDepth: 0,
    scopeCloseLevels: @[],
    root: root
  )
  result.cache.openScope() # Open global scope

  var n = root
  while n.kind != EofToken:
    let currentPos = n.pos # Store position before traversal step
    traverseNode(result, n)
  result.cache.closeScope() # Close global scope
