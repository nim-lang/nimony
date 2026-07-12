#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helpers for declarative constructs like `let` statements or `proc` declarations.

import std / [assertions, syncio]
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, reporters]

include ".." / lib / compat2

template reportImpl(msg: string; c: Cursor; level: string) =
  when defined(debug) and not defined(nimony):
    writeStackTrace()
  write stdout, level
  if isValid(c.info):
    write stdout, infoToStr(c.info)
    write stdout, " "
  write stdout, msg
  writeLine stdout, toString(c, false)
  quit 1

template reportImpl(msg: string; level: string) =
  when defined(debug) and not defined(nimony):
    writeStackTrace()
  write stdout, level
  writeLine stdout, msg
  quit 1

proc error*(msg: string; c: Cursor) {.noreturn.} =
  reportImpl(msg, c, "[Error] ")

proc error*(msg: string) {.noreturn.} =
  reportImpl(msg, "[Error] ")

proc bug*(msg: string; c: Cursor) {.noreturn.} =
  when not defined(nimony):
    writeStackTrace()
  reportImpl(msg, c, "[Bug] ")

proc bug*(msg: string) {.noreturn.} =
  when not defined(nimony):
    writeStackTrace()
  reportImpl(msg, "[Bug] ")

proc isRoutine*(t: SymKind): bool {.inline.} =
  t in {ProcY, FuncY, IteratorY, MacroY, TemplateY, ConverterY, MethodY}

proc isValidFnHead*(t: SymKind): bool {.inline.} =
  ## A caller `fn` expression can resolve to any symbol except a module:
  ## routines, but also a `TypeY`/`TypevarY` (a type conversion like `int(x)`
  ## or `T(1)`) or a proc-typed value. Only modules are never callable, so
  ## they must stay out of overload sym choices where a same-named proc exists
  ## (nim-lang/nimony#2130).
  t != ModuleY

proc isLocal*(t: SymKind): bool {.inline.} =
  t in {LetY, VarY, ResultY, ConstY, ParamY, TypevarY, StaticTypevarY, CursorY, PatternvarY, FldY, GfldY, EfldY, GletY, TletY, GvarY, TvarY}

proc isTypevarLike*(t: SymKind): bool {.inline.} =
  ## generic parameters: ordinary type variables and value ("static") parameters
  t in {TypevarY, StaticTypevarY}

proc isNominal*(t: TypeKind): bool {.inline.} =
  ## type kinds that should stay as symbols, see sigmatch.matchSymbol
  t in {ObjectT, EnumT, OnumT, AnumT, DistinctT, ConceptT}

proc skipToParams*(c: var Cursor) =
  ## In-place: advance `c` past the prefix slots of a proctype or
  ## proc-decl-shaped cursor so it points at the params slot.
  ## Proctype/itertype layout: `(<tag> <NilTag> (params...) RetType <Pragmas>)`.
  ## Proc-decl layout: `(proc Name ExportMarker Pattern Typevars (params...) ...)`.
  let kind = c.typeKind
  let sk = c.symKind
  inc c # skip ParLe
  if kind in {ProctypeT, ItertypeT}:
    skip c # nilability tag
  elif kind in RoutineTypes or sk in RoutineKinds:
    skip c # name
    skip c # export marker
    skip c # pattern
    skip c # generics

proc skipRoutineDeclPrefix*(n: var Cursor; parentKind: TypeKind) =
  ## `n` sits just past a type's opening `ParLe`. Advance past the leading
  ## bookkeeping that is not part of the type's identity, so two same-tag
  ## types can be compared structurally from a common point. Only routine
  ## *decls* carry such slots (name/export/pattern/generics before the
  ## params); proctype/itertype keep their nilability tag in slot 0 (it
  ## participates in the comparison) and every other type starts at its
  ## first structural child.
  if parentKind in RoutineTypes and parentKind notin {ProctypeT, ItertypeT}:
    skip n # name
    skip n # export marker
    skip n # pattern
    skip n # generics

proc skipProcTypeToParams*(t: Cursor): Cursor =
  ## Pure version: returns a cursor advanced past the prefix slots.
  result = t
  if result.typeKind in RoutineTypes or result.symKind in RoutineKinds:
    skipToParams result

const
  LocalPragmasPos* = 2
  LocalTypePos* = 3
  LocalValuePos* = 4

type
  SkipMode* = enum
    SkipExclBody,
    SkipInclBody,
    SkipFinalParRi

  Local* = object
    kind*: SymKind
    name*: Cursor
    exported*: Cursor
    pragmas*: Cursor
    typ*: Cursor
    val*: Cursor

proc takeLocal*(c: var Cursor; mode: SkipMode): Local =
  let kind = symKind c
  result = Local(kind: kind)
  if isLocal(kind):
    if mode == SkipFinalParRi:
      c.into:
        result.name = c
        skip c
        result.exported = c
        skip c
        result.pragmas = c
        skip c
        result.typ = c
        skip c
        result.val = c
        skip c
    else:
      inc c
      result.name = c
      skip c
      result.exported = c
      skip c
      result.pragmas = c
      skip c
      result.typ = c
      if mode >= SkipInclBody:
        skip c
        result.val = c
        skip c

proc asLocal*(c: Cursor; mode = SkipInclBody): Local =
  var c = c
  result = takeLocal(c, mode)

proc asTypevar*(c: Cursor): Local {.inline.} =
  result = asLocal(c)

type
  Routine* = object
    kind*: SymKind
    name*: Cursor
    exported*: Cursor
    pattern*: Cursor # for TR templates/macros
    typevars*: Cursor # generic parameters
    params*: Cursor
    retType*: Cursor
    pragmas*: Cursor
    effects*: Cursor
    body*: Cursor

proc isGeneric*(r: Routine): bool {.inline.} =
  r.typevars.substructureKind == TypevarsU

proc takeRoutine*(c: var Cursor; mode: SkipMode): Routine =
  let kind = symKind c
  result = Routine(kind: kind)
  if isRoutine(kind):
    if mode == SkipFinalParRi:
      c.into:
        result.name = c
        skip c
        result.exported = c
        skip c
        result.pattern = c
        skip c
        result.typevars = c
        skip c
        result.params = c
        skip c
        result.retType = c
        skip c
        result.pragmas = c
        skip c
        result.effects = c
        skip c
        result.body = c
        skip c
    else:
      inc c
      result.name = c
      skip c
      result.exported = c
      skip c
      result.pattern = c
      skip c
      result.typevars = c
      skip c
      result.params = c
      skip c
      result.retType = c
      skip c
      result.pragmas = c
      skip c
      result.effects = c
      if mode >= SkipInclBody:
        skip c
        result.body = c
        skip c

const
  TypevarsPos* = 3
  ParamsPos* = 4
  ReturnTypePos* = 5
  ProcPragmasPos* = 6
  BodyPos* = 8

proc asRoutine*(c: Cursor; mode = SkipExclBody): Routine =
  var c = c
  result = takeRoutine(c, mode)

type
  TypeDecl* = object
    kind*: SymKind
    name*: Cursor
    exported*: Cursor
    typevars*: Cursor
    pragmas*: Cursor
    body*: Cursor

proc isGeneric*(r: TypeDecl): bool {.inline.} =
  r.typevars.substructureKind == TypevarsU

proc takeTypeDecl*(c: var Cursor; mode: SkipMode): TypeDecl =
  let kind = symKind c
  result = TypeDecl(kind: kind)
  if kind == TypeY:
    if mode == SkipFinalParRi:
      c.into:
        result.name = c
        skip c
        result.exported = c
        skip c
        result.typevars = c
        skip c
        result.pragmas = c
        skip c
        result.body = c
        skip c
    else:
      inc c
      result.name = c
      skip c
      result.exported = c
      skip c
      result.typevars = c
      skip c
      result.pragmas = c
      if mode >= SkipInclBody:
        skip c
        result.body = c

proc asTypeDecl*(c: Cursor): TypeDecl =
  var c = c
  result = takeTypeDecl(c, SkipInclBody)

type
  ObjectDecl* = object
    kind*: TypeKind
    parentType*: Cursor
    body*: Cursor
      ## Cursor at the (object …) / (union …) parent ParLe. Walk the fields
      ## via `body.into:` — for ObjectT remember to `skip body` past the
      ## inheritance slot first; for UnionT `body.into:` is enough.

proc asObjectDecl*(c: Cursor): ObjectDecl =
  let kind = typeKind c
  result = ObjectDecl(kind: kind, body: c)
  if kind == ObjectT:
    var x = c
    inc x
    result.parentType = x

type ObjFieldIter* = object
  nested: int
  scopes: seq[Cursor]

proc initObjFieldIter*(): ObjFieldIter =
  result = ObjFieldIter(nested: 1)

proc nextField*(iter: var ObjFieldIter, n: var Cursor, keepCase = false): bool =
  ## Resumable walk over an object body's fields. The caller must have
  ## entered the object scope (e.g. via `into`/`peekInto`) so that the
  ## outermost scope is bounded; nested case/when/of scopes are managed
  ## here via `sub`/`skip`.
  result = false
  while iter.nested != 0:
    if not n.hasMore:
      dec iter.nested
      if iter.nested != 0:
        n = iter.scopes.pop(); skip n
    else:
      case n.substructureKind
      of CaseU:
        if keepCase:
          result = true
          break
        else:
          inc iter.nested
          iter.scopes.add n; n = sub(n)
      of WhenU, StmtsU, NilU, ElseU:
        inc iter.nested
        iter.scopes.add n; n = sub(n)
      of ElifU, OfU:
        inc iter.nested
        iter.scopes.add n; n = sub(n)
        skip n
      of FldU, GfldU:
        result = true
        break
      else:
        error "illformed AST inside object: ", n

type
  EnumDecl* = object
    kind*: TypeKind
    baseType*: Cursor
    body*: Cursor
      ## Cursor at the (enum/onum/anum …) parent ParLe. Walk fields via
      ## `body.into:` — skip the baseType (and, for AnumT, the owner-type
      ## sym) before iterating.

proc asEnumDecl*(c: Cursor): EnumDecl =
  let kind = typeKind c
  result = EnumDecl(kind: kind, body: c)
  if kind in {EnumT, OnumT, AnumT}:
    var x = c
    inc x
    result.baseType = x

type
  TupleField* = object
    kind*: SubstructureKind
    name*: Cursor
    typ*: Cursor

proc asTupleField*(c: Cursor): TupleField =
  var c = c
  let kind = substructureKind c
  result = TupleField(kind: kind)
  case c.substructureKind
  of KvU:
    inc c # tag
    result.name = c
    skip c
    result.typ = c
  else:
    # unnamed
    result.typ = c

proc getTupleFieldType*(c: Cursor): Cursor =
  case c.substructureKind
  of KvU:
    result = c
    inc result # tag
    skip result, SkipName # name
  else:
    result = c

type
  ForStmt* = object
    kind*: StmtKind
    iter*, vars*, body*: Cursor

proc asForStmt*(c: Cursor): ForStmt =
  var c = c
  let kind = stmtKind c
  result = ForStmt(kind: kind)
  if kind == ForS:
    inc c
    result.iter = c
    skip c
    result.vars = c
    skip c
    result.body = c
