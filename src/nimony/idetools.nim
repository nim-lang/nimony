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
  let i = unpack(pool.man, info)
  let t = unpack(pool.man, toTrack)
  if i.file.isValid and t.file.isValid:
    if i.file != t.file: return false
    if i.line != t.line: return false
    # Check if target column falls within the token's range
    if t.col < i.col: return false
    if t.col > i.col + tokenLen: return false
  return true

proc foundSymbol(tok: PackedToken; mode: TrackMode) =
  # format that is compatible with nimsuggest's in the hope it helps:
  let info = unpack(pool.man, tok.info)
  if info.file.isValid:
    if (tok.kind == Symbol and mode == TrackUsages) or (tok.kind == SymbolDef and mode == TrackDef):
      var r = (if tok.kind == Symbol: "use\t" else: "def\t")
      r.add "\t" # unknown symbol kind
      r.add pool.syms[tok.symId]
      r.add "\t"
      # unknown signature:
      r.add "\t"
      # filename:
      r.add "\t"
      r.add pool.files[info.file]
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
      if sym.kind == SymbolDef and sym.symId == nameSym:
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

            if containingType.kind == Symbol and lineInfoMatch(n.info, c.toTrack, c.tokenLen):
              # Add definition of field, only happens once because of the matching line info check
              let typeDefinition = getTypeSection(containingType.symId)
              var typeBody = typeDefinition.body
              findAndAddFieldDefinition(c, typeBody, pool.syms[c.sym], c.sym, containingType)

            c.usages.add(Usage(n: n, containingType: containingType))

          elif n.symId == c.sym and c.searchKind notin {skField, skDot} and c.currentDotLhs.cursorIsNil:
            c.usages.add(Usage(n: n))

          inc n

        of ParLe:
          n.loopInto:
            tr(c, n)

        else:
          inc n

proc getParent(n: Cursor): Cursor =
  ## Walk cursor backwards until reaching the start of the parent of n
  result = n
  unsafeDec result
  var depth = 1
  while depth > 0:
    case result.kind
    of ParLe:
      dec depth
      if depth == 0:
        break
    of ParRi:
      inc depth
    else:
      discard
    unsafeDec result

proc locateSymImpl(n: var Cursor; buf: TokenBuf; sym: SymId; toTrack: PackedLineInfo;
                   tokenLen: int; parentPos: int; symOffset, parentOffset: var int): bool =
  ## Positions of the tracked symbol token and its innermost parent ParLe
  ## in the PARSED buffer. (The caller's stream scan counts the file's
  ## physical ParRi tokens, which are elided in the in-memory buffer under
  ## `-d:virtualParRi`, so its offsets cannot be used here.)
  result = false
  case n.kind
  of ParLe:
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
    endRead(buf)
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
  # This is comparable to a linking step: We iterate over all `.idetools.nif` files to see
  # what symbol is meant by the `file,line,col` tracking information.
  let requestedInfo = lineinfos.pack(pool.man, pool.files.getOrIncl(config.toTrack.filename),
                                     config.toTrack.line, config.toTrack.col)
  # first pass: search for the symbol at `file,line,col`:
  var isLocalSym = false
  var symId = SymId 0
  var symFile = ""
  var symOffset = 0
  var symParentOffset = 0

  let moduleName = moduleSuffix(config.toTrack.filename, config.paths)
  let nifFile = config.nifcachePath / moduleName & ".s.nif"

  var s = nifstreams.open(nifFile)
  var parentStarts = newSeqOfCap[int](100)
  try:
    discard processDirectives(s.r)
    var i = 0
    while true:
      let tok = next(s)
      case tok.kind
      of Symbol, SymbolDef:
        # performance critical! May run over every symbol in the project!
        var name = addr pool.syms[tok.symId]
        var tokenLen = 0
        var dots = 0
        for i in 0 ..< name[].len:
          if name[][i] == '.':
            inc dots
          if dots == 0: inc tokenLen
        if lineInfoMatch(tok.info, requestedInfo, tokenLen):
          symOffset = i
          symParentOffset = parentStarts[^1]
          isLocalSym = dots < 2
          symId = tok.symId
          symFile = nifFile
          break
      of EofToken: break
      of ParLe:
        parentStarts.add(i)
      of ParRi:
        discard parentStarts.pop()
      of UnknownToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
        discard "proceed"

      inc i
  finally:
    close(s)

  if symId == SymId 0:
    quit "symbol not found"
  elif isLocalSym:
    # Set path so files are found when resolving symbols
    prog.main.dir = nifFile.splitPath.head
    findLocal(symFile, symId, requestedInfo, config.toTrack.mode)
  else:
    for file in files:
      var s = nifstreams.open(file)
      try:
        discard processDirectives(s.r)
        let indexStart = s.r.indexStartsAt()
        while true:
          if indexStart > 0 and s.r.offset() >= indexStart: break
          let tok = next(s)
          case tok.kind
          of Symbol:
            if tok.symId == symId: foundSymbol(tok, config.toTrack.mode)
          of SymbolDef:
            if tok.symId == symId: foundSymbol(tok, config.toTrack.mode)
          of EofToken: break
          of UnknownToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit, ParLe, ParRi:
            discard "proceed"
      finally:
        close(s)
