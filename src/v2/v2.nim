#       V2 plugin
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

const
  Usage = """v2 - Nim 2.0 plugin for Nimony

Usage:
  v2 path/file.nim nimcache/modname.nif nimcache/modname.idx.nif
"""

import std/[assertions, os]
include ".." / lib / nifprelude
import ".." / lib / [nifindexes, symparser]
import ".." / nimony / [decls, nimony_model]

proc getAttachedOp(symId: SymId, attackedOp: var AttachedOp): bool =
  var name = pool.syms[symId]
  extractBasename(name)

  attackedOp = case name
    of "=destroy": attachedDestroy
    of "=wasMoved": attachedWasMoved
    of "=trace": attachedTrace
    of "=copy": attachedCopy
    of "=sink": attachedSink
    of "=dup": attachedDup
    else: return false

  return true

proc indexFromNif*(infile: string) =
  ## Extract index from `infile` Nif file and write it to `*.idx.nif` file.
  ##
  ## See https://github.com/nim-lang/nimony/issues/1162
  var stream = nifstreams.open(infile)
  discard processDirectives(stream.r)
  var buf = fromStream(stream)
  stream.close

  var n = beginRead buf
  let root = n.info
  var hookIndexLog = default array[AttachedOp, seq[HookIndexEntry]]
  var converterIndexMap = default seq[(SymId, SymId)]
  var classIndexMap = default seq[ClassIndexEntry]

  assert n.stmtKind == StmtsS
  inc n
  while n.kind != ParRi:
    if n.kind == ParLe:
      case n.stmtKind:
      of ProcS, FuncS, ConverterS, MethodS:
        let routine = takeRoutine(n, SkipFinalParRi)
        let symId = routine.name.symId
        var op = default AttachedOp
        if getAttachedOp(symId, op):
          var param = routine.params
          assert param.substructureKind == ParamsU
          inc param
          assert param.substructureKind == ParamU
          let typ = takeLocal(param, SkipExclBody).typ.skipModifier
          # this assertion fails when got generics proc as generics parameters are not supported yet.
          assert typ.kind == Symbol
          let obj = typ.symId
          let isGeneric = routine.typevars.substructureKind == TypevarsU
          hookIndexLog[op].add HookIndexEntry(typ: obj, hook: symId, isGeneric: isGeneric)
      else:
        skip n
    else:
      skip n

  endRead buf

  createIndex infile, root, true,
    IndexSections(hooks: move hookIndexLog,
      converters: move converterIndexMap,
      classes: move classIndexMap)

proc main(nimFile, nifFile, idxFile: string) =
  let nimcacheDir = splitFile(nifFile).dir
  let c = "nim nif --nimcache:" & quoteShell(nimcacheDir) & " " & quoteShell(nimFile)
  if os.execShellCmd(c) != 0:
    quit "v2: failed to compile " & nimFile
  indexFromNif nifFile

if paramCount() != 3:
  quit "v2: invalid number of arguments; expected 3, got " & $paramCount()
else:
  main(paramStr(1), paramStr(2), paramStr(3))
