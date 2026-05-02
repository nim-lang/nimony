#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Types required by semantic checking.

import std / [tables, sets, hashes, os, syncio, formatfloat, assertions]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [symparser, nifindexes]
import nimony_model, symtabs, builtintypes, decls, programs, magics, reporters, nifconfig, xints,
  langmodes, features

import ".." / gear2 / modnames

type
  TypeCursor* = Cursor
  SemRoutine* {.acyclic.} = ref object
    kind*: SymKind
    hasDefer*: bool
    inGeneric*, inLoop*, inBlock*, inInst*, inExcept*: int
    returnType*: TypeCursor
    pragmas*: set[PragmaKind]
    raisesType*: TypeCursor  # Type from .raises pragma (e.g., ErrorCode, MyError)
    resId*: SymId
    parent*: SemRoutine

proc createSemRoutine*(kind: SymKind; parent: SemRoutine): SemRoutine =
  result = SemRoutine(kind: kind, parent: parent, resId: SymId(0))

const
  MaxNestedTemplates* = 100

type
  ImportedModule* = object
    path*: string
    fromPlugin*: string
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
    guarded*: bool # true for gfld fields (cannot be accessed via dot)
    rootOwner*: SymId # generic root of owner type

  # SemPhase and ToplevelEntry are now in programs.nim

  MetaInfo* = object
    includedFiles*: seq[string] # will become part of the index file
    importedFiles*: seq[string] # likewise

  SemExpressionExecutor* = proc (c: var SemContext; expr: Cursor; expectedType: TypeCursor; result: var TokenBuf; info: PackedLineInfo): string {.nimcall.}
  SemStmtCallback* = proc (c: var SemContext; dest: var TokenBuf; n: Cursor) {.nimcall.}
  SemGetSize* = proc(c: var SemContext; n: Cursor; strict=false): xint {.nimcall.}
  ForceInstantiate* = proc (c: var SemContext; dest: var TokenBuf) {.nimcall.}

  MethodIndexEntry* = object
    fn*: SymId
    signature*: StrId

  ClassEntry* = object
    methods*: seq[MethodIndexEntry]

  Classes* = Table[SymId, ClassEntry]

  SemContext* = object
    #dest*: TokenBuf
    routine*: SemRoutine
    currentScope*: Scope
    g*: ProgramContext
    procRequests*: seq[InstRequest]
    typeInstDecls*: seq[SymId]
      ## syms of type instantiations to add their declarations to module
    pendingSumtypes*: TokenBuf
      ## synthesized oneof type declarations to emit at module end
    includeStack*: seq[string]
    importedModules*: OrderedTable[SymId, ImportedModule]
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
    features*: set[Feature]
    processedModules*: Table[string, SymId] # suffix to sym
    usedTypevars*: int
    phase*: SemPhase
    canSelfExec*: bool
    checkedForWriteNifModule*: bool
    inWhen*: int
    inUncheckedAccess*: int
    templateInstCounter*: int
    commandLineArgs*: string # for IC we make nimony `exec` itself. Thus it is important
                             # to forward command line args properly.
    #fieldsCache: Table[SymId, Table[StrId, ObjField]]
    meta*: MetaInfo
    #hookIndexLog*: array[AttachedOp, seq[HookIndexEntry]] # only a log, used for index generation, but is not read from.
    typeHooks*: Table[SymId, HooksPerType] # hooks per type, for embedding in type declarations
    converters*: Table[SymId, seq[SymId]]
    converterIndexMap*: seq[(SymId, SymId)]
    classes*: Classes # class entries with methods for vtables
    exports*: OrderedTable[SymId, ImportFilter] # module syms to export filter
    freshSyms*: HashSet[SymId] ## symdefs that should count as new for semchecking
    toBuild*: TokenBuf
    unoverloadableMagics*: HashSet[StrId]
    debugAllowErrors*: bool
    pending*: TokenBuf
    pendingTypePlugins*: Table[SymId, (StrId, PackedLineInfo)]
    pendingModulePlugins*: seq[(StrId, PackedLineInfo)]
    pluginBlacklist*: HashSet[StrId] # make 1984 fiction again
    cachedTypeboundOps*: Table[(SymId, StrId), seq[SymId]]
    userPragmas*: Table[StrId, TokenBuf]
    customPragmaTemplates*: HashSet[StrId]
      ## Names of templates declared with `{.pragma.}`. Such templates can
      ## be used as custom pragmas that accept arguments, e.g.
      ## `template ensuresNif*(x: untyped) {.pragma.}` lets later code attach
      ## `{.ensuresNif: addedAny(dest).}`. The template body is not expanded
      ## here — the annotation is simply accepted and dropped, matching
      ## Nim's treatment for tooling-only pragmas.
    usingStmtMap*: Table[StrId, TypeCursor] # mapping of identifiers to types declared in using statements
    pragmaStack*: seq[TokenBuf] # used to implement {.push.} and {.pop.}
    executeExpr*: SemExpressionExecutor
    semStmtCallback*: SemStmtCallback
    semGetSize*: SemGetSize
    forceInstantiate*: ForceInstantiate
    passL*: seq[string]
    passC*: seq[string]
    importSnippets*: TokenBuf ## NIF snippets for import statements (with absolute paths), for use by exprexec
    genericInnerProcs*: HashSet[SymId] # these are special in that they must be instantiated in specific places
    expanded*: TokenBuf
    forwardDecls*: Table[StrId, seq[SymId]] # forward declaration candidates by name
    matchedForwardDecls*: HashSet[SymId] ## Forward decls whose matching
      ## implementation has been seen. The proc-decl tokens are still in
      ## `dest` (because we've already moved past them when the match
      ## happens), so `writeOutput` strips them before serialising the
      ## module so they do not leak into the export index. See
      ## tests/nimony/lookups/tforward_decl_export.nim.
    deferredCyclicImports*: seq[(string, SymId)] # (module suffix, module sym) for cyclic imports to resolve after phase1
    inTypeInst*: int # > 0 means we're inside a generic type instantiation

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
    # `hasKey` just returned true, so `getOrQuit` will not quit.
    result = cursorAt(c.typeMem.getOrQuit(key), 0)
  else:
    var newBuf = createTokenBuf(buf.len - start)
    for i in start..<buf.len:
      newBuf.add buf[i]
    # make resilient against crashes:
    #if newBuf.len == 0: newBuf.add dotToken(NoLineInfo)
    result = cursorAt(newBuf, 0)
    c.typeMem[key] = newBuf

template emptyNode*(c: var SemContext): Cursor =
  # XXX find a better solution for this
  c.types.voidType
