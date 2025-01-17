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
    inGeneric*, inLoop*, inBlock*: int
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
    iface*: Iface

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
    typeRequests*, procRequests*: seq[InstRequest]
    includeStack*: seq[string]
    importedModules*: Table[SymId, ImportedModule]
    instantiatedFrom*: seq[PackedLineInfo]
    importTab*: OrderedTable[StrId, seq[SymId]] ## mapping of identifiers to modules containing the identifier
    globals*, locals*: Table[string, int]
    types*: BuiltinTypes
    typeMem*: Table[string, TokenBuf]
    instantiatedTypes*: OrderedTable[string, SymId]
    instantiatedProcs*: OrderedTable[(SymId, string), SymId]
    thisModuleSuffix*: string
    moduleFlags*: set[ModuleFlag]
    processedModules*: HashSet[string]
    usedTypevars*: int
    phase*: SemPhase
    canSelfExec*: bool
    templateInstCounter*: int
    commandLineArgs*: string # for IC we make nimony `exec` itself. Thus it is important
                             # to forward command line args properly.
    #fieldsCache: Table[SymId, Table[StrId, ObjField]]
    meta*: MetaInfo
    genericHooks*: Table[SymId, seq[SymId]]
    hookIndexMap*: Table[string, seq[(SymId, SymId)]]
    freshSyms*: HashSet[SymId] ## symdefs that should count as new for semchecking
