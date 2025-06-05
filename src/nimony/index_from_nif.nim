import std/[assertions]
include nifprelude
import nifindexes, symparser
import decls, nimony_model

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

  var buf = block:
    var stream = nifstreams.open(infile)
    discard processDirectives(stream.r)
    var result = fromStream(stream)
    stream.close
    move result
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

when isMainModule:
  import std/[os, osproc, syncio]
  import semos
  import ".." / gear2 / modnames

  const fname = "test.nim"
  writeFile(fname, """
type
  Foo* = object
  Bar = object

proc foo* = discard
proc bar = discard

var fooVar* = 0
var barVar = 0

proc `=destroy`*(x: var Foo) = discard
proc `=wasMoved`*(x: var Foo) = discard
proc `=trace`*(x: var Foo; env: pointer) = discard
proc `=copy`*(x: var Foo; y: Foo) = discard
proc `=sink`*(x: var Foo; y: Foo) = discard
proc `=dup`*(x: Foo): Foo = x

#[
type
  Goo[T] = object

proc `=destroy`*[T](x: var Goo[T]) = discard
proc `=wasMoved`*[T](x: var Goo[T]) = discard
proc `=trace`*[T](x: var Goo[T]; env: pointer) = discard
proc `=copy`*[T](x: var Goo[T]; y: Goo[T]) = discard
proc `=sink`*[T](x: var Goo[T]; y: Goo[T]) = discard
proc `=dup`*[T](x: Goo[T]): Goo[T] = x
]#
  """)

  const cacheDir = "nim2cache"
  createDir cacheDir
  assert 0 == execCmd("nim nif --nimcache:" & cacheDir & " " & fname)
  let nifname = cacheDir / (moduleSuffix(fname.toAbsolutePath, []) & ".nim2.nif")
  indexFromNif nifname
