#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [sets, syncio, assertions, os]
include ".." / lib / nifprelude
import ".." / lib / [symparser, nifindexes, tooldirs]
import semos, programs, typenav, typeprops, decls, nifconfig, nimony_model
import ".."/gear2/modnames

proc lineInfoMatch*(info, toTrack: PackedLineInfo; tokenLen: int): bool =
  let i = unpack(lineMan, info)
  let t = unpack(lineMan, toTrack)
  if i.file.isValid and t.file.isValid:
    if i.file != t.file: return false
    if i.line != t.line: return false
    # Check if target column falls within the token's range
    if t.col < i.col: return false
    if t.col > i.col + tokenLen: return false
  return true

proc foundSymbol(tok: NifToken; mode: TrackMode) =
  discard "idetools --track over a raw NifToken needs the cursor-based rewrite"
type
  SearchKind = enum skOther, skField, skDot

  Usage = object
    n: Cursor
    containingType: Cursor
      ## For field definitions: type containing the field
      ## For dot expressions: type of lhs
      ## For bare identifiers: nil

  IdeContext = object
    toTrack: PackedLineInfo
    trackMode: TrackMode
    searchKind: SearchKind
    sym: SymId
    tokenLen: int # Length of identifier (excluding nif suffixes)
    typeCache: TypeCache
    currentDotLhs: Cursor # Points to the left hand side of dot expression while traversing dot expressions
    currentType: Cursor # Points to the type being traversed, used while processing fields contained in that type
    usages: seq[Usage]

proc findAndAddFieldDefinition(c: var IdeContext, n: var Cursor, name: string, nameSym: SymId, containingType: Cursor) =
  n.loopInto:
    if n.substructureKind == FldU:
      var sym = n
      inc sym
      if sym.isSymbolDef and sym.symId == nameSym:
        c.usages.add(Usage(n: sym, containingType: containingType))
    skip n

proc tr(c: var IdeContext, n: var Cursor) =
  ## Traverse the tree and collect definitions and usages matching the sym in the context
  case n.stmtKind
  of ScopeS:
    c.typeCache.openScope()
    n.loopInto:
      tr(c, n)
    c.typeCache.closeScope()

  of ProcS, FuncS, MethodS, IteratorS, TemplateS, MacroS, ConverterS:
    let decl = n
    c.typeCache.openScope(ProcScope)
    n.into:
      let symId = n.symId
      for i in 0..<BodyPos:
        if i == ParamsPos:
          c.typeCache.registerParams(symId, decl, n)
          tr(c, n)
        else:
          tr(c, n)
      tr(c, n) # body
    c.typeCache.closeScope()

  of TypeS:
    let t = c.currentType
    n.into:
      if n.symId == c.sym:
        c.usages.add(Usage(n: n, containingType: c.currentType))
      c.currentType = n
      while n.hasMore:
        tr(c, n)
    c.currentType = t

  of VarS, LetS, ConstS:
    let symKind = n.symKind
    n.into:
      let sym = n.symId
      if sym == c.sym and c.searchKind notin {skField, skDot}:
        c.usages.add(Usage(n: n, containingType: c.currentType))
      inc n # sym
      inc n # exported
      inc n # pragmas
      let typ = n
      tr(c, n) # type
      tr(c, n) # value
      c.typeCache.registerLocal(sym, symKind, typ)

  else:
    case n.exprKind
    of DotX, DdotX:
      n.into:
        let lhs = n
        tr(c, n) # lhs
        let typeContext = c.currentDotLhs
        c.currentDotLhs = lhs
        tr(c, n) # rhs
        c.currentDotLhs = typeContext
        # process any remaining children
        while n.hasMore:
          tr(c, n)

    of OconstrX:
      skip n

    else:
      case n.substructureKind
      of ParamU:
        n.into:
          let sym = n.symId
          if sym == c.sym and c.searchKind notin {skField, skDot}:
            c.usages.add(Usage(n: n, containingType: c.currentType))
          inc n # sym
          inc n # exported
          inc n # pragmas
          tr(c, n) # type
          tr(c, n) # value

      of FldU:
        n.into:
          if n.symId == c.sym and c.trackMode == TrackUsages and c.searchKind in {skField, skDot}:
            c.usages.add(Usage(n: n, containingType: c.currentType))
          # process any remaining children
          while n.hasMore:
            tr(c, n)

      else:
        case n.kind
        of Symbol:
          if n.symId == c.sym and c.searchKind in {skField, skDot} and not c.currentDotLhs.cursorIsNil:
            var containingType = c.typeCache.getType(c.currentDotLhs, {SkipAliases})
            if containingType.typeKind in {RefT, PtrT}:
              inc containingType

            if containingType.isSymbol and lineInfoMatch(n.info, c.toTrack, c.tokenLen):
              # Add definition of field, only happens once because of the matching line info check
              let typeDefinition = getTypeSection(containingType.symId)
              var typeBody = typeDefinition.body
              findAndAddFieldDefinition(c, typeBody, pool.syms[c.sym], c.sym, containingType)

            c.usages.add(Usage(n: n, containingType: containingType))

          elif n.symId == c.sym and c.searchKind notin {skField, skDot} and c.currentDotLhs.cursorIsNil:
            c.usages.add(Usage(n: n))

          inc n

        of TagLit:
          n.loopInto:
            tr(c, n)

        else:
          inc n

proc getParent(n: Cursor): Cursor =
  ## Walk cursor backwards until reaching the start of the parent of n
  result = n  # backward paren-counting has no nifcore analogue (no ParRi token)
proc locateSymImpl(n: var Cursor; buf: TokenBuf; sym: SymId; toTrack: PackedLineInfo;
                   tokenLen: int; parentPos: int; symOffset, parentOffset: var int): bool =
  ## Positions of the tracked symbol token and its innermost parent TagLit
  ## in the PARSED buffer. (The caller's stream scan counts the file's
  ## physical ParRi tokens, which are elided in the in-memory buffer under
  ## `-d:virtualParRi`, so its offsets cannot be used here.)
  result = false
  case n.kind
  of TagLit:
    let myPos = cursorToPosition(buf, n)
    n.into:
      while n.hasMore:
        if locateSymImpl(n, buf, sym, toTrack, tokenLen, myPos, symOffset, parentOffset):
          return true
  of Symbol, SymbolDef:
    if n.symId == sym and lineInfoMatch(n.info, toTrack, tokenLen):
      symOffset = cursorToPosition(buf, n)
      parentOffset = parentPos
      result = true
    else:
      inc n
  else:
    inc n

proc findLocal(file: string; sym: SymId; toTrack: PackedLineInfo; mode: TrackMode) =
  var buf = parseFromFile(file)

  var name = pool.syms[sym]
  extractBasename name

  var offset = -1
  var parentOffset = 0
  block locateBlock:
    var loc = beginRead(buf)
    discard locateSymImpl(loc, buf, sym, toTrack, name.len, 0, offset, parentOffset)
  if offset < 0:
    quit "symbol not found"

  var n = beginRead(buf)

  var symCursor = buf.cursorAt(offset)
  var symParent = buf.cursorAt(parentOffset)

  var c = IdeContext()
  c.toTrack = toTrack
  c.trackMode = mode
  c.sym = sym
  c.tokenLen = name.len
  c.typeCache = createTypeCache()
  c.typeCache.openScope()

  if symParent.substructureKind == FldU:
    c.searchKind = skField
  elif symParent.exprKind in {DotX, DdotX}:
    var firstChild = symParent
    inc firstChild
    if symCursor != firstChild:
      # When the searched for symbol is not the left hand side but the right hand side then we're searching for a field
      c.searchKind = skDot

  # Collect definitions and usages matching the symbol. In case of field access this might return
  # extra things that need to be filtered below
  tr(c, n)

  var expectedContainingType = Cursor()
  if c.searchKind in {skDot, skField}:
    for usage in c.usages:
      if lineInfoMatch(usage.n.info, c.toTrack, c.tokenLen):
        expectedContainingType = usage.containingType
        break

  for usage in c.usages:
    # Filter based on expectedContainingType if set
    if not expectedContainingType.cursorIsNil and
        (usage.containingType.cursorIsNil or usage.containingType.symId != expectedContainingType.symId):
      continue

    foundSymbol(usage.n.load, c.trackMode)

proc usages*(files: openArray[string]; config: NifConfig) =
  quit "idetools --track is not yet supported on nifcore (needs a cursor-based rewrite of the streaming/offset walk)"