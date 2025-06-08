#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nimony index generator.

import std / [os, assertions]
include ".." / lib / nifprelude
import ".." / lib / [nifindexes, symparser]
import decls, nimony_model, programs

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
          if typ.kind == Symbol:
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
