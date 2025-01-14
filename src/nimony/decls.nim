#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helpers for declarative constructs like `let` statements or `proc` declarations.

import std / assertions
import nifstreams, nifcursors, nimony_model

proc isRoutine*(t: SymKind): bool {.inline.} =
  t in {ProcY, FuncY, IterY, MacroY, TemplateY, ConverterY, MethodY}

proc isLocal*(t: SymKind): bool {.inline.} =
  t in {LetY, VarY, ResultY, ConstY, ParamY, TypevarY, CursorY, FldY, EfldY}

proc isNominal*(t: TypeKind): bool {.inline.} =
  ## type kinds that should stay as symbols, see sigmatch.matchSymbol
  t in {ObjectT, EnumT, HoleyEnumT, DistinctT, ConceptT}

const
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
      if mode == SkipFinalParRi:
        if c.kind == ParRi:
          inc c
        else:
          raiseAssert "expected ')' inside (" & $result.kind

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
  r.typevars.substructureKind == TypevarsS

proc takeRoutine*(c: var Cursor; mode: SkipMode): Routine =
  let kind = symKind c
  result = Routine(kind: kind)
  if isRoutine(kind):
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
      if mode == SkipFinalParRi:
        if c.kind == ParRi:
          inc c
        else:
          raiseAssert "expected ')' inside (" & $result.kind

const
  TypevarsPos* = 3
  ParamsPos* = 4
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
  r.typevars.substructureKind == TypevarsS

proc asTypeDecl*(c: Cursor): TypeDecl =
  var c = c
  let kind = symKind c
  result = TypeDecl(kind: kind)
  if kind == TypeY:
    inc c
    result.name = c
    skip c
    result.exported = c
    skip c
    result.typevars = c
    skip c
    result.pragmas = c
    skip c
    result.body = c

type
  ObjectDecl* = object
    kind*: TypeKind
    parentType*: Cursor
    firstField*: Cursor

proc asObjectDecl*(c: Cursor): ObjectDecl =
  var c = c
  let kind = typeKind c
  result = ObjectDecl(kind: kind)
  if kind == ObjectT:
    inc c
    result.parentType = c
    skip c
    result.firstField = c

type
  EnumDecl* = object
    kind*: TypeKind
    baseType*: Cursor
    firstField*: Cursor

proc asEnumDecl*(c: Cursor): EnumDecl =
  var c = c
  let kind = typeKind c
  result = EnumDecl(kind: kind)
  if kind in {EnumT, HoleyEnumT}:
    inc c
    result.baseType = c
    skip c
    result.firstField = c

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
