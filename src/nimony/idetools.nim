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

type IdeContext = object
  toTrack: PackedLineInfo
  trackMode: TrackMode
  sym: SymId
  tokenLen: int
  typeCache: TypeCache
  currentDotLhs: Cursor

proc findField(c: var IdeContext, n: var Cursor, name: string, nameSym: SymId) =
  inc n
  while n.kind != ParRi:
    if n.substructureKind == FldU:
      var sym = n
      inc sym
      if sym.kind == SymbolDef and sym.symId == nameSym:
        foundSymbol(sym.load, c.trackMode)
    skip n

proc tr(c: var IdeContext, n: var Cursor) =
  case n.stmtKind
  of ScopeS, StmtsS:
    c.typeCache.openScope()
    n.loopInto():
      tr(c, n)
    c.typeCache.closeScope()

  of ProcS, FuncS, MethodS, IteratorS, TemplateS, MacroS, ConverterS:
    let decl = n
    inc n # ParLe
    let symId = n.symId
    c.typeCache.openScope(ProcScope)
    for i in 0..<BodyPos:
      if i == ParamsPos:
        c.typeCache.registerParams(symId, decl, n)
        tr(c, n)
      else:
        tr(c, n)
        # n.skip()

    tr(c, n) # body

    assert n.kind == ParRi
    inc n

    c.typeCache.closeScope()

  of VarS, LetS, ConstS:
    let symKind = n.symKind
    inc n # ParLe
    let sym = n.symId
    if sym == c.sym:
      foundSymbol(n.load, c.trackMode)
    inc n # sym
    inc n # exported
    inc n # pragmas
    let typ = n
    tr(c, n) # type
    tr(c, n) # value
    assert n.kind == ParRi
    inc n

    c.typeCache.registerLocal(sym, symKind, typ)

  else:
    case n.exprKind
    of DotX, DdotX:
      let dot = n
      inc n # skip ParLe
      let lhs = n
      tr(c, n) # lhs
      let typeContext = c.currentDotLhs
      c.currentDotLhs = lhs
      tr(c, n) # rhs
      c.currentDotLhs = typeContext
      # skip remaining children
      while n.kind != ParRi:
        tr(c, n)
      assert n.kind == ParRi
      inc n
    else:
      case n.substructureKind
      of ParamU:
        let symKind = n.symKind
        inc n # ParLe
        let sym = n.symId
        if sym == c.sym:
          foundSymbol(n.load, c.trackMode)
        inc n # sym
        inc n # exported
        inc n # pragmas
        let typ = n
        tr(c, n) # type
        tr(c, n) # value
        assert n.kind == ParRi
        inc n

      else:
        case n.kind
        of Symbol, SymbolDef:
          if n.symId == c.sym and lineInfoMatch(n.info, c.toTrack, c.tokenLen):

            if not c.currentDotLhs.cursorIsNil:
              # Symbol is right hand side of DotX or DdotX, look up field in type of lhs
              var typ = c.typeCache.getType(c.currentDotLhs, {SkipAliases})
              if typ.typeKind in {RefT, PtrT}:
                inc typ

              if typ.kind == Symbol:
                let typeDefinition = getTypeSection(typ.symId)
                var typeBody = typeDefinition.body
                findField(c, typeBody, pool.syms[c.sym], c.sym)
            else:
              foundSymbol(n.load, c.trackMode)

          inc n

        of ParLe:
          n.loopInto():
            tr(c, n)

        else:
          inc n

proc findLocal(file: string; sym: SymId; toTrack: PackedLineInfo; mode: TrackMode) =
  var buf = parseFromFile(file)
  var n = beginRead(buf)

  var name = pool.syms[sym]
  extractBasename name

  var c = IdeContext()
  c.toTrack = toTrack
  c.trackMode = mode
  c.sym = sym
  c.tokenLen = name.len
  c.typeCache = createTypeCache()
  c.typeCache.openScope()

  tr(c, n)

proc usages*(files: openArray[string]; config: NifConfig) =
  # This is comparable to a linking step: We iterate over all `.idetools.nif` files to see
  # what symbol is meant by the `file,line,col` tracking information.
  let requestedInfo = lineinfos.pack(pool.man, pool.files.getOrIncl(config.toTrack.filename),
                                     config.toTrack.line, config.toTrack.col)
  # first pass: search for the symbol at `file,line,col`:
  var isLocalSym = false
  var symId = SymId 0
  var symFile = ""

  let moduleName = moduleSuffix(config.toTrack.filename, config.paths)
  let nifFile = config.nifcachePath / moduleName & ".s.nif"

  var s = nifstreams.open(nifFile)
  try:
    discard processDirectives(s.r)
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
        let i = unpack(pool.man, tok.info)
        let t = unpack(pool.man, requestedInfo)
        if lineInfoMatch(tok.info, requestedInfo, tokenLen):
          isLocalSym = dots < 2
          symId = tok.symId
          symFile = nifFile
          break
      of EofToken: break
      of UnknownToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit, ParLe, ParRi:
        discard "proceed"
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
