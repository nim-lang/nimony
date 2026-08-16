#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## "Can a value of this symbol's type hold a pointer?", answered over the
## symbol's **Nimony** declaration with `programs.tryLoadSym`'s on-demand
## loading.
##
## `funcsummary` asks this about *Leng* types, and for a type declared in the
## module being summarized the Leng body is right there in the buffer. For an
## imported one it is not: hexer runs per module, and a dependency's Leng output
## is not an input of that run — only its sem index (`.s.idx.nif`) is. Calling
## every imported type pointer-bearing would make `string` opaque, so no copy of
## one could be proved non-aliasing and code would optimize worse purely because
## it was split across modules. "Put it in one file and it gets faster" is not a
## cost model a programmer can work with, so the question is answered where the
## information *is* available on demand: the Nimony declaration, reached through
## the same loader every other hexer pass already uses.
##
## There is no "could not resolve, so assume it points somewhere" answer for an
## imported symbol. By the time hexer runs, everything a Leng slot can name that
## belongs to another module has been through sem and has a declaration in that
## module's `.s.nif`; a lookup that fails is a bug in this resolver or in the
## build graph, and it says so rather than degrading into lost optimization.
## Nor is there an exception for a symbol with no module at all: a local is not
## this module's question. Its type is registered in the summary walk's own
## scope, `typenav` reads it from there, and a local that reached here would
## mean the caller peeled past a step it could have resolved.
##
## The traversal deliberately mirrors `nimony/sizeof.getSize` — same
## nominal-resolution loop, same object/case/tuple walk, same `(inheritable)`
## rule for the RTTI pointer — because the answer has to agree with what
## `lengcgen` lowers these types to, and `getSize` is the pass that already
## encodes that layout. The *structural* reading stays one-sided: only kinds
## that provably contain no pointer answer `false`, and a routine or any of the
## pointer constructors answers `true`. A kind that cannot appear at all — an
## abstract or generic type, an error node — is reported, not absorbed: being
## wrong towards `true` costs an optimization, being wrong towards `false`
## costs a missed invalidation, which is a miscompile.
##
## Lives apart from `funcsummary` because that module decodes **Leng** tags:
## `leng_model` and `nimony_model` both define `typeKind`, so the two cannot
## share a compilation unit. Only `SymId` and `bool` cross the boundary.

import std / [assertions, tables, syncio]   # syncio: `quit`
from std / os import fileExists
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser            # extractModule
import ".." / nimony / [nimony_model, decls, programs]

type
  PointerTypeCache = Table[SymId, bool]
    ## Memoizes the answer per nominal type. Worth having: without it the walk
    ## re-loads and re-walks `string`'s declaration for every parameter that
    ## mentions it, and a summary pass asks about every parameter of every proc
    ## in the module.

var gCache: PointerTypeCache = initTable[SymId, bool]()
  ## Process-wide, because the answer is one: a type symbol is fully qualified
  ## by module, and hexer compiles one module per process. Keeping it here also
  ## lets the entry point be a plain `{.nimcall.}` hook (see `pointerbearing`).

var gReadable: Table[string, bool] = initTable[string, bool]()

var gContext = ""
  ## The routine currently being analyzed, for diagnostics: a symbol that cannot
  ## be resolved is a bug, and the first thing anyone will want to know is where
  ## it was named.

proc setPointerTypeContext*(s: string) =
  gContext = s

proc ctx(): string =
  if gContext.len == 0: "" else: " (in " & gContext & ")"

proc moduleIsReadable(s: SymId): bool =
  ## `tryLoadSym` opens the owning module's `.s.nif` unconditionally, and
  ## `nifreader.open` on a file that is not there is fatal — which would report
  ## the miss as a reader crash rather than as what it is. Checking first buys a
  ## diagnostic that names the symbol. Cached per module suffix: one `stat` per
  ## dependency, not per symbol.
  let m = extractModule(pool.syms[s])
  if m.len == 0: return false
  if gReadable.hasKey(m): return gReadable.getOrQuit(m)
  result = fileExists(suffixToNif(m))
  gReadable[m] = result

proc declOf(s: SymId): Cursor =
  ## The Nimony declaration of `s`. Failing to find one is not an answer: the
  ## symbol came out of a Leng slot this build just produced, so its declaration
  ## exists and is reachable through the module index.
  if extractModule(pool.syms[s]).len == 0:
    quit "hexer/pointertypes: " & pool.syms[s] & " is a local symbol; it names no " &
         "module to load a declaration from — its type is in the walk's scope" & ctx()
  if not moduleIsReadable(s):
    quit "hexer/pointertypes: no `.s.nif` for the module declaring " & pool.syms[s] & ctx()
  let sym = tryLoadSym(s)
  if sym.status != LacksNothing:
    quit "hexer/pointertypes: cannot load the declaration of " & pool.syms[s] & ctx()
  result = sym.decl

const MaxDepth = 100
  ## Not a bail-out bound. A type can only nest into itself through a `ref`/`ptr`,
  ## and those answer without recursing, so a by-value chain is finite; this only
  ## keeps a malformed cycle from spinning forever, and reports if it ever trips.

proc isInheritableDecl(pragmas: Cursor): bool =
  ## An object with no explicit base but `(inheritable)` still gets the RTTI
  ## pointer in the backend's layout, so it is pointer-bearing even when every
  ## declared field is a scalar. `sizeof` accounts for exactly the same word.
  result = false
  var n = pragmas
  if n.substructureKind == PragmasU:
    n.into PragmasU:
      while n.hasMore:
        if n.pragmaKind == InheritableP: result = true
        skip n

proc typeMayHoldPointer(n: Cursor; cache: var PointerTypeCache;
                        inheritable: bool; depth: int): bool

proc symMayHoldPointer(s: SymId; cache: var PointerTypeCache; depth: int): bool =
  ## Resolve a nominal type — an alias, an instantiated generic, a `distinct`,
  ## an imported one — to its structural body and answer from that.
  if cache.hasKey(s): return cache.getOrQuit(s)
  if depth > MaxDepth:
    quit "hexer/pointertypes: type nesting too deep at " & pool.syms[s]
  let decl = declOf(s)
  let d = asTypeDecl(decl)
  if d.kind != TypeY:
    quit "hexer/pointertypes: " & pool.syms[s] & " is used as a type but declares a " &
         $symKind(decl)
  result = typeMayHoldPointer(d.body, cache, isInheritableDecl(d.pragmas),
                              depth+1)
  cache[s] = result

proc typeMayHoldPointer(n: Cursor; cache: var PointerTypeCache;
                        inheritable: bool; depth: int): bool =
  ## `inheritable` is the `(inheritable)` pragma of the declaration `n` is the
  ## body of — it only matters for a rootless object, and is false for every
  ## inline (undeclared) type.
  if depth > MaxDepth:
    quit "hexer/pointertypes: type nesting too deep"
  if cursorIsNil(n):
    quit "hexer/pointertypes: empty type slot"
  if n.isSymbol: return symMayHoldPointer(n.symId, cache, depth+1)
  case n.typeKind
  of IntT, UIntT, FloatT, CharT, BoolT, VoidT, EnumT, HoleyEnumT, AnumT, SetT:
    # `set` lowers to a bitset — an array of bytes, never a pointer.
    result = false
  of RangetypeT, SinkT, DistinctT, ArrayT:
    result = typeMayHoldPointer(n.childCursor, cache, false, depth+1)
  of RefT, PtrT, MutT, OutT, LentT, NiltT, CstringT, PointerT, UarrayT,
     VarargsT:
    result = true
  of ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT, TemplateT,
     ProctypeT, ItertypeT:
    result = true                  # a proc value is a code pointer, a closure two
  of TupleT:
    result = false
    var m = n
    m.into:
      while m.hasMore:
        if typeMayHoldPointer(getTupleFieldType(m), cache, false, depth+1):
          result = true
        skip m
  of ObjectT:
    result = inheritable           # the RTTI pointer of a rootless inheritable
    var m = sub(n)                 # bounded copy; `nextField` needs the scope
    if not m.isDotToken:           # explicit base type
      if typeMayHoldPointer(m, cache, false, depth+1): result = true
    skip m
    # `keepCase = false` flattens case-object branches into one field stream: a
    # pointer in any branch makes the object pointer-bearing, and which branch
    # is active is not a question this analysis asks.
    var iter = initObjFieldIter()
    while nextField(iter, m, keepCase = false):
      let f = takeLocal(m, SkipFinalParRi)
      if typeMayHoldPointer(f.typ, cache, false, depth+1): result = true
  of NoType, ErrT, AtT, AndT, OrT, NotT, StaticT, ConceptT, AutoT, SymkindT,
     TypekindT, TypedescT, UntypedT, TypedT, OrdinalT:
    # Abstract, generic or unresolved. None of these survive into a type slot
    # that a *Leng* type reached: sem instantiated the generics and the errors
    # never got this far. Saying "pointer-bearing" here would quietly turn a
    # resolver bug into lost optimization.
    quit "hexer/pointertypes: not a concrete type: " & $n.typeKind

proc objectBodyOf(s: SymId; depth: int): Cursor =
  ## The `(object …)` / `(union …)` body a nominal type resolves to, following
  ## aliases and `distinct`. Fields live on the object, so a field lookup has to
  ## get there first.
  if depth > MaxDepth:
    quit "hexer/pointertypes: type nesting too deep at " & pool.syms[s]
  let d = asTypeDecl(declOf(s))
  if d.kind != TypeY:
    quit "hexer/pointertypes: " & pool.syms[s] & " is used as a type but declares a " &
         $symKind(declOf(s))
  result = d.body
  if result.isSymbol: return objectBodyOf(result.symId, depth+1)
  if result.typeKind == DistinctT: 
    var inner = result.childCursor
    if inner.isSymbol: return objectBodyOf(inner.symId, depth+1)
    result = inner

proc fieldMayHoldPointer(owner: SymId; field: string; depth: int): bool =
  ## `owner.field`, answered from the Nimony declaration of `owner`. Leng field
  ## symbols are local names — they carry no module, so unlike every other
  ## symbol a field cannot be loaded on its own and has to be found on the type
  ## that declares it.
  if depth > MaxDepth:
    quit "hexer/pointertypes: inheritance chain too deep at " & pool.syms[owner]
  let body = objectBodyOf(owner, 0)
  if body.typeKind != ObjectT:   # a Nimony `union` is an object with a pragma
    quit "hexer/pointertypes: " & pool.syms[owner] & " has no field " & field &
         " (it is a " & $body.typeKind & ")"
  let obj = asObjectDecl(body)
  var m = sub(body)
  skip m                                  # inheritance slot
  var iter = initObjFieldIter()
  while nextField(iter, m, keepCase = false):
    let f = takeLocal(m, SkipFinalParRi)
    if f.name.isSymbolDef and pool.syms[f.name.symId] == field:
      return typeMayHoldPointer(f.typ, gCache, false, depth+1)
  # Not here: it belongs to a base object.
  if obj.parentType.isSymbol:
    return fieldMayHoldPointer(obj.parentType.symId, field, depth+1)
  # Nor there: `field` is one lowering introduced (a seq's data pointer, a
  # closure's environment), so it has no Nimony declaration to read. The owner's
  # own type still bounds it — the field is part of that value — so answer from
  # the whole object rather than inventing a verdict for the slice.
  result = symMayHoldPointer(owner, gCache, depth+1)

proc nimonyFieldMayHoldPointer*(owner: SymId; field: string): bool {.nimcall.} =
  ## The `pointerbearing.ForeignFieldResolver` `funcsummary` installs.
  fieldMayHoldPointer(owner, field, 0)

proc nimonyMayHoldPointer*(s: SymId): bool {.nimcall.} =
  ## The `pointerbearing.ForeignSymResolver` `funcsummary` installs. Called for
  ## every symbol whose declaration is not in the Leng buffer being annotated —
  ## which is every *imported* symbol, since that buffer is the module hexer is
  ## producing. Not a fallback: this is the normal path for `string`, `stdout`
  ## and everything else a program imports.
  ##
  ## `s` may name a type or a value (a global, a constant, a field): the
  ## declaration says which, and a value is answered from its declared type.
  ##
  ## A *local* never gets here: it carries no module, so `declOf` reports it
  ## rather than inventing an answer. Its type is in the summary walk's own
  ## scope, and that is where it is read (`typenav`) — see `declOf`.
  if gCache.hasKey(s): return gCache.getOrQuit(s)
  let decl = declOf(s)
  let sk = symKind(decl)
  if sk == TypeY:
    result = symMayHoldPointer(s, gCache, 0)
  elif isLocal(sk):
    result = typeMayHoldPointer(asLocal(decl).typ, gCache, false, 0)
  elif sk in {ProcY, FuncY, IteratorY, ConverterY, MethodY, MacroY, TemplateY}:
    result = true                  # taking a routine as a value yields a pointer
  else:
    quit "hexer/pointertypes: " & pool.syms[s] & " declares a " & $sk &
         ", which has no value type"
  gCache[s] = result
