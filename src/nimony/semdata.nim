#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Types required by semantic checking.

import std / [tables, sets, os, syncio, formatfloat, assertions]
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser,
  programs, magics, reporters, nifconfig, nifindexes

import ".." / gear2 / modnames

type
  TypeCursor* = Cursor
  SemRoutine* {.acyclic.} = ref object
    kind*: SymKind
    inGeneric*, inLoop*, inBlock*, inInst*: int
    returnType*: TypeCursor
    pragmas*: set[PragmaKind]
    resId*: SymId
    parent*: SemRoutine

proc createSemRoutine*(kind: SymKind; parent: SemRoutine): SemRoutine =
  result = SemRoutine(kind: kind, parent: parent, resId: SymId(0))

const
  MaxNestedTemplates* = 100

type
  ImportedModule* = object
    path*: string
    iface*: Iface
    exports*: Table[SymId, ImportFilter]

  InstRequest* = object
    origin*: SymId
    targetSym*: SymId
    #targetType*: TypeCursor
    #typeParams*: seq[TypeCursor]
    inferred*: Table[SymId, Cursor]
    requestFrom*: seq[PackedLineInfo]

  ProcInstance* = object
    targetSym*: SymId
    procType*: TypeCursor
    returnType*: TypeCursor

  ProgramContext* = ref object # shared for every `SemContext`
    config*: NifConfig

  ObjField* = object
    sym*: SymId
    level*: int # inheritance level
    typ*: TypeCursor
    exported*: bool
    rootOwner*: SymId # generic root of owner type

  SemPhase* = enum
    SemcheckTopLevelSyms,
    SemcheckSignatures,
    SemcheckBodies

  MetaInfo* = object
    includedFiles*: seq[string] # will become part of the index file
    importedFiles*: seq[string] # likewise

  ModuleFlag* = enum
    IsSystem, IsMain, SkipSystem

  SemContext* = object
    dest*: TokenBuf
    routine*: SemRoutine
    currentScope*: Scope
    g*: ProgramContext
    procRequests*: seq[InstRequest]
    typeInstDecls*: seq[SymId]
      ## syms of type instantiations to add their declarations to module
    includeStack*: seq[string]
    importedModules*: Table[SymId, ImportedModule]
    selfModuleSym*: SymId
    instantiatedFrom*: seq[PackedLineInfo]
    importTab*: OrderedTable[StrId, seq[SymId]] ## mapping of identifiers to modules containing the identifier
    globals*, locals*: Table[string, int]
    types*: BuiltinTypes
    typeMem*: Table[string, TokenBuf]
    instantiatedTypes*: Table[string, SymId]
    instantiatedProcs*: Table[(SymId, string), SymId]
    thisModuleSuffix*: string
    moduleFlags*: set[ModuleFlag]
    processedModules*: Table[string, SymId] # suffix to sym
    usedTypevars*: int
    phase*: SemPhase
    canSelfExec*: bool
    templateInstCounter*: int
    commandLineArgs*: string # for IC we make nimony `exec` itself. Thus it is important
                             # to forward command line args properly.
    #fieldsCache: Table[SymId, Table[StrId, ObjField]]
    meta*: MetaInfo
    hookIndexLog*: array[AttachedOp, seq[HookIndexEntry]] # only a log, used for index generation, but is not read from.
    converters*: Table[SymId, seq[SymId]]
    converterIndexMap*: seq[(SymId, SymId)]
    methods*: Table[SymId, seq[SymId]]
    classIndexMap*: seq[ClassIndexEntry]
    exports*: OrderedTable[SymId, ImportFilter] # module syms to export filter
    freshSyms*: HashSet[SymId] ## symdefs that should count as new for semchecking
    toBuild*: TokenBuf
    unoverloadableMagics*: HashSet[StrId]
    debugAllowErrors*: bool
    pending*: TokenBuf
    pendingTypePlugins*: Table[SymId, StrId]
    pendingModulePlugins*: seq[StrId]
    pluginBlacklist*: HashSet[StrId] # make 1984 fiction again
    systemSymId*: SymId
    ignoreErr*: bool
    outfile*: string

proc typeToCanon*(buf: TokenBuf; start: int): string =
  result = ""
  for i in start..<buf.len:
    case buf[i].kind
    of ParLe:
      result.add '('
      result.addInt buf[i].tagId.int
    of ParRi: result.add ')'
    of Ident, StringLit:
      result.add ' '
      result.addInt buf[i].litId.int
    of UnknownToken: result.add " unknown"
    of EofToken: result.add " eof"
    of DotToken: result.add '.'
    of Symbol, SymbolDef:
      result.add " s"
      result.addInt buf[i].symId.int
    of CharLit:
      result.add " c"
      result.addInt buf[i].uoperand.int
    of IntLit:
      result.add " i"
      result.addInt buf[i].intId.int
    of UIntLit:
      result.add " u"
      result.addInt buf[i].uintId.int
    of FloatLit:
      result.add " f"
      result.addInt buf[i].floatId.int

proc typeToCursor*(c: var SemContext; buf: TokenBuf; start: int): TypeCursor =
  let key = typeToCanon(buf, start)
  if c.typeMem.hasKey(key):
    result = cursorAt(c.typeMem[key], 0)
  else:
    var newBuf = createTokenBuf(buf.len - start)
    for i in start..<buf.len:
      newBuf.add buf[i]
    # make resilient against crashes:
    #if newBuf.len == 0: newBuf.add dotToken(NoLineInfo)
    result = cursorAt(newBuf, 0)
    c.typeMem[key] = newBuf

proc typeToCursor*(c: var SemContext; start: int): TypeCursor =
  typeToCursor(c, c.dest, start)

template emptyNode*(c: var SemContext): Cursor =
  # XXX find a better solution for this
  c.types.voidType
