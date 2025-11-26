#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [sets, syncio]
include ".." / lib / nifprelude
import semos, symparser, nifindexes, tooldirs, nifconfig, nimony_model

proc lineInfoMatch*(info, toTrack: PackedLineInfo; tokenLen: int): bool =
  let i = unpack(pool.man, info)
  let t = unpack(pool.man, toTrack)
  if i.file.isValid and t.file.isValid:
    if i.file != t.file: return false
    if i.line != t.line: return false
    # Check if target column falls within the token's range
    if t.col < i.col: return false
    if t.col >= i.col + tokenLen: return false
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

proc findLocal(file: string; sym: SymId; toTrack: PackedLineInfo; mode: TrackMode) =
  var buf = parseFromFile(file)
  var n = beginRead(buf)
  var scopes: seq[(Cursor, int)] = @[(n, 0)]

  var name = pool.syms[sym]
  extractBasename name

  let tokenLen = name.len
  var foundScope = false
  var nested = 0
  while true:
    case n.stmtKind
    of ScopeS:
      inc nested
      scopes.add (n, nested)
    of ProcS, FuncS, MethodS, IteratorS, TemplateS, MacroS, ConverterS:
      inc nested
      scopes.add (n, nested)
    else:
      case n.kind
      of Symbol, SymbolDef:
        if n.symId == sym and lineInfoMatch(n.info, toTrack, tokenLen):
          foundScope = true
          break
      of ParLe: inc nested
      of ParRi:
        dec nested
        if nested == scopes[^1][1]:
          discard scopes.pop()

        if nested == 0: break
      else:
        discard
    inc n

  n = scopes[^1][0]
  inc n
  nested = 1 # in owning structure
  while true:
    case n.kind
    of ParLe: inc nested
    of ParRi:
      dec nested
      if nested == 0: break
    of Symbol, SymbolDef:
      if n.symId == sym:
        foundSymbol(n.load, mode)
    else:
      discard
    inc n

proc usages*(files: openArray[string]; config: NifConfig) =
  # This is comparable to a linking step: We iterate over all `.idetools.nif` files to see
  # what symbol is meant by the `file,line,col` tracking information.
  let requestedInfo = lineinfos.pack(pool.man, pool.files.getOrIncl(config.toTrack.filename),
                                     config.toTrack.line, config.toTrack.col)
  # first pass: search for the symbol at `file,line,col`:
  var isLocalSym = false
  var symId = SymId 0
  var symFile = ""
  for file in files:
    var s = nifstreams.open(file)
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
          if lineInfoMatch(tok.info, requestedInfo, tokenLen):
            isLocalSym = dots < 2
            symId = tok.symId
            symFile = file
            break
        of EofToken: break
        of UnknownToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit, ParLe, ParRi:
          discard "proceed"
    finally:
      close(s)

  if symId == SymId 0:
    quit "symbol not found"
  elif isLocalSym:
    findLocal(symFile, symId, requestedInfo, config.toTrack.mode)
  else:
    for file in files:
      var s = nifstreams.open(file)
      try:
        discard processDirectives(s.r)
        while true:
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
