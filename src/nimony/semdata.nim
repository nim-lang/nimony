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
  Item* = object
    ## A semchecked expression together with its type. Lives here (rather than
    ## in sigmatch) so that the `SemContext` callback typedefs below can name it
    ## without creating an import cycle (sigmatch already imports semdata).
    n*, typ*: Cursor
    kind*: SymKind

  SemFlag* = enum
    KeepMagics
    AllowOverloads
    PreferIterators
    AllowUndeclared
    AllowModuleSym
    AllowEmpty
    InTypeContext
    BypassFieldVis  ## input (dot ...) already carried an access-token
                    ## certifying the field access was hygiene-checked in
                    ## the field's owner module; skip the visibility check
                    ## during re-semcheck
    BypassGuardedCheck  ## input (dot ...) had a resolved Symbol for the field,
                        ## meaning it was already validated in a prior semcheck pass

  TypeDeclContext* = enum
    InLocalDecl, InTypeSection, InReturnTypeDecl, AllowValues,
    InGenericConstraint, InInvokeHead

  CrucialPragma* = object
    ## Pragma summary collected while sem-checking a routine/type's pragmas.
    ## Fields are exported so the pragma module (sempragmas) and the sem core
    ## can share it across the module boundary.
    sym*: SymId
    magic*, externName*: string
    bits*: int
    size*: int  ## value of `{.size: X.}` pragma in bytes; 0 if not set
    hasVarargs*: PackedLineInfo
    flags*: set[PragmaKind]
    raisesType*: TypeCursor  # Type from .raises pragma
    headerFileTok*: PackedToken

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

  PluginObj* = object
    path*: StrId
    info*: PackedLineInfo

  SemExpressionExecutor* = proc (c: var SemContext; expr: Cursor; expectedType: TypeCursor; result: var TokenBuf; info: PackedLineInfo): string {.nimcall.}
  SemStmtCallback* = proc (c: var SemContext; dest: var TokenBuf; n: Cursor) {.nimcall.}
  SemGetSize* = proc(c: var SemContext; n: Cursor; strict=false): xint {.nimcall.}
  ForceInstantiate* = proc (c: var SemContext; dest: var TokenBuf) {.nimcall.}
  SemInstantiateType* = proc (c: var SemContext; typ: Cursor; bindings: Table[SymId, Cursor]): Cursor {.nimcall.}
  # Callbacks into the sem core, used by separately-compiled handler modules
  # (e.g. semmagics) to break the otherwise mutual recursion with sem.nim.
  SemExprCallbackT* = proc (c: var SemContext; dest: var TokenBuf; it: var Item; flags: set[SemFlag]) {.nimcall.}
  SemStmtCallbackT* = proc (c: var SemContext; dest: var TokenBuf; n: var Cursor; isNewScope: bool) {.nimcall.}
  CommonTypeCallbackT* = proc (c: var SemContext; dest: var TokenBuf; it: var Item; argBegin: int; expected: TypeCursor) {.nimcall.}
  SemLocalTypeImplCallbackT* = proc (c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext; exported: bool; ownerSym: SymId) {.nimcall.}
  # Additional core entry points needed by the pragma module (sempragmas).
  DeclareResultCallbackT* = proc (c: var SemContext; dest: var TokenBuf; info: PackedLineInfo): SymId {.nimcall.}
  SemEmitCallbackT* = proc (c: var SemContext; dest: var TokenBuf; it: var Item) {.nimcall.}

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
    fieldCounts*: Table[string, int]
      ## Per-name field counts for the object type currently being declared.
      ## Unlike `globals`/`locals` this is NOT a monotonic counter: a field is
      ## numbered `.0` within its owning type, bumped only when an ancestor type
      ## already declares the same name (so a field's SymId is independent of the
      ## global counter / semcheck order). `semObjectType` saves/restores it so
      ## nested anonymous object types each get their own numbering.
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
    toBundle*: TokenBuf ## `.bundle` custom-linker entries (link-step override)
    unoverloadableMagics*: HashSet[StrId]
    debugAllowErrors*: bool
    pending*: TokenBuf
    pendingTypePlugins*: Table[SymId, PluginObj]
    pendingModulePlugins*: seq[PluginObj]
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
    semInstantiateType*: SemInstantiateType
    semExprCB*: SemExprCallbackT
    semStmtCB*: SemStmtCallbackT
    commonTypeCB*: CommonTypeCallbackT
    semLocalTypeImplCB*: SemLocalTypeImplCallbackT
    declareResultCB*: DeclareResultCallbackT
    semEmitCB*: SemEmitCallbackT
    passL*: seq[string]
    passC*: seq[string]
    importSnippets*: TokenBuf ## NIF snippets for import statements (with absolute paths), for use by exprexec
    genericInnerProcs*: HashSet[SymId] # these are special in that they must be instantiated in specific places
    expanded*: TokenBuf
    forwardDecls*: Table[StrId, seq[SymId]] # forward declaration candidates by name
    routineSigCache*: Table[SymId, string] # canonical routine signatures for redef checks
    compiledMacros*: Table[SymId, string] # mapping macro SymId to compiled plugin path
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
    of SymbolDef:
      # Param names inside proctypes get fresh symIds per declaration,
      # but param names do not affect type identity. Use a fixed marker
      # so that e.g. two `seq[proc(x: int)]` type trees produce the same
      # canonical key regardless of the internal symId allocation for `x`.
      result.add " !symdef"
    of Symbol:
      # An instantiated sym like `seq.0.Iabc.modA` has its module suffix
      # appended at *creation* time, so the same logical instantiation
      # `seq[Foo]` can appear as `seq.0.Iabc.modA` or `seq.0.Iabc.modB`
      # depending on where in the program it was first instantiated.
      # When such a sym appears as a *typeArg* to another generic
      # instantiation, the two forms have different `symId`s but are
      # semantically the same — DCE merges them at link time via
      # `removeModule(name)` as the key, and `instToSuffix` (the hash
      # that builds the *new* instantiation's name) likewise strips the
      # module. Without matching canonicalization here, the proc-instance
      # cache (`c.instantiatedProcs`) misses for typeArgs that differ
      # only in module suffix, while `newInstSymId` still produces the
      # same name — so we get two definitions of the same proc.
      let s = pool.syms[buf[i].symId]
      if isInstantiation(s):
        result.add " s\""
        result.add removeModule(s)
        result.add '"'
      else:
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

