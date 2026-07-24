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

proc foundSymbol(n: Cursor; mode: TrackMode) =
  # format that is compatible with nimsuggest's in the hope it helps:
  let info = unpack(lineMan, n.info)
  if info.file.isValid:
    if (n.isSymbol and mode == TrackUsages) or (n.isSymbolDef and mode == TrackDef):
      var r = (if n.isSymbol: "use\t" else: "def\t")
      r.add "\t" # unknown symbol kind
      r.add pool.syms[n.symId]
      r.add "\t"
      # unknown signature:
      r.add "\t"
      # filename:
      r.add "\t"
      r.add pool.filenames[info.file]
      r.add "\t"
      r.addInt info.line
      r.add "\t"
      r.addInt info.col
      stdout.writeLine(r)
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

    foundSymbol(usage.n, c.trackMode)

proc usages*(files: openArray[string]; config: NifConfig) =
  # This is comparable to a linking step: we scan the module's semmed NIF to
  # see what symbol is meant by the `file,line,col` tracking information.
  let requestedInfo = lineinfos.pack(lineMan, pool.filenames.getOrIncl(config.toTrack.filename),
                                     config.toTrack.line, config.toTrack.col)
  # first pass: search for the symbol at `file,line,col`:
  var isLocalSym = false
  var symId = SymId 0

  let moduleName = moduleSuffix(config.toTrack.filename, config.paths)
  let nifFile = config.nifcachePath / moduleName & ".s.nif"

  var buf = parseFromFile(nifFile)
  block search:
    var n = beginRead(buf)
    while n.hasMore:
      if n.isSymbol or n.isSymbolDef:
        # performance critical! May run over every symbol in the project!
        let name = pool.syms[n.symId]
        var tokenLen = 0
        var dots = 0
        for i in 0 ..< name.len:
          if name[i] == '.':
            inc dots
          if dots == 0: inc tokenLen
        if lineInfoMatch(n.info, requestedInfo, tokenLen):
          isLocalSym = dots < 2
          symId = n.symId
          break search
      inc n

  if symId == SymId 0:
    quit "symbol not found"
  elif isLocalSym:
    # Set path so files are found when resolving symbols
    prog.main.dir = nifFile.splitPath.head
    findLocal(nifFile, symId, requestedInfo, config.toTrack.mode)
  else:
    for file in files:
      var fbuf = parseFromFile(file)
      var n = beginRead(fbuf)
      while n.hasMore:
        if (n.isSymbol or n.isSymbolDef) and n.symId == symId:
          foundSymbol(n, config.toTrack.mode)
        inc n