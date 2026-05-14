#       Nimony std/macros
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## This module implements Nim's macro system for Nimony.
## It provides the NimNode type and operations for AST manipulation.

{.feature: "lenientnils".}
import std/[syncio, assertions, strutils, math, formatfloat, cmdline]
import nifreader, nifbuilder

type
  NimNodeKind* = enum
    nnkNone
    nnkEmpty
    nnkIdent
    nnkSym
    nnkIntLit
    nnkInt8Lit
    nnkInt16Lit
    nnkInt32Lit
    nnkInt64Lit
    nnkUIntLit
    nnkUInt8Lit
    nnkUInt16Lit
    nnkUInt32Lit
    nnkUInt64Lit
    nnkFloatLit
    nnkFloat32Lit
    nnkFloat64Lit
    nnkStrLit
    nnkRStrLit
    nnkTripleStrLit
    nnkCharLit
    nnkNilLit
    nnkCall
    nnkCommand
    nnkCallStrLit
    nnkInfix
    nnkPrefix
    nnkPostfix
    nnkHiddenCallConv
    nnkExprEqExpr
    nnkExprColonExpr
    nnkPar
    nnkBracket
    nnkCurly
    nnkTupleConstr
    nnkObjConstr
    nnkTableConstr
    nnkBracketExpr
    nnkCurlyExpr
    nnkDotExpr
    nnkDerefExpr
    nnkHiddenDeref
    nnkAddr
    nnkHiddenAddr
    nnkCast
    nnkConv
    nnkHiddenStdConv
    nnkHiddenSubConv
    nnkIfExpr
    nnkWhenExpr
    nnkStmtList
    nnkStmtListExpr
    nnkBlockExpr
    nnkBlockStmt
    nnkAsgn
    nnkFastAsgn
    nnkVarSection
    nnkLetSection
    nnkConstSection
    nnkIdentDefs
    nnkVarTuple
    nnkIfStmt
    nnkWhenStmt
    nnkElifBranch
    nnkElse
    nnkCaseStmt
    nnkOfBranch
    nnkWhileStmt
    nnkForStmt
    nnkTryStmt
    nnkExceptBranch
    nnkFinally
    nnkReturnStmt
    nnkBreakStmt
    nnkContinueStmt
    nnkYieldStmt
    nnkRaiseStmt
    nnkDiscardStmt
    nnkProcDef
    nnkFuncDef
    nnkMethodDef
    nnkIteratorDef
    nnkMacroDef
    nnkTemplateDef
    nnkConverterDef
    nnkFormalParams
    nnkGenericParams
    nnkPragma
    nnkPragmaExpr
    nnkTypeSection
    nnkTypeDef
    nnkObjectTy
    nnkTupleTy
    nnkEnumTy
    nnkEnumFieldDef
    nnkRecList
    nnkRecCase
    nnkRecWhen
    nnkDistinctTy
    nnkRefTy
    nnkPtrTy
    nnkVarTy
    nnkProcTy
    nnkIteratorTy
    nnkRange
    nnkImportStmt
    nnkExportStmt
    nnkIncludeStmt
    nnkFromImport
    nnkImportExcept
    nnkBind
    nnkMixin
    nnkUsing
    nnkCommentStmt
    nnkStaticStmt
    nnkDefer
    nnkAsm
    nnkDo
    nnkAccQuoted
    nnkError
    # Types
    nnkType

  NimNode* = ref object
    kindField: NimNodeKind
    kids: seq[NimNode]
    # Literal storage:
    strValField: string
    intValField: BiggestInt
    floatValField: BiggestFloat

template isNilNode*(n: NimNode): bool = cast[pointer](n) == nil

# Property accessors
func kind*(n: NimNode): NimNodeKind {.inline.} =
  n.kindField

func `kind=`*(n: NimNode; k: NimNodeKind) {.inline.} =
  n.kindField = k

func len*(n: NimNode): int {.inline.} =
  n.kids.len

func `[]`*(n: NimNode; i: int): NimNode {.inline.} =
  n.kids[i]

func `[]`*(n: NimNode; i: BackwardsIndex): NimNode {.inline.} =
  n.kids[n.kids.len - i.int]

func `[]=`*(n: NimNode; i: int; child: NimNode) {.inline.} =
  n.kids[i] = child

func `[]=`*(n: NimNode; i: BackwardsIndex; child: NimNode) {.inline.} =
  n.kids[n.kids.len - i.int] = child

func add*(n: NimNode; child: NimNode) {.inline.} =
  n.kids.add child

iterator items*(n: NimNode): NimNode =
  for i in 0 ..< n.kids.len:
    yield n.kids[i]

iterator pairs*(n: NimNode): (int, NimNode) =
  for i in 0 ..< n.kids.len:
    yield (i, n.kids[i])

proc strVal*(n: NimNode): string {.inline.} =
  assert n.kind in {nnkIdent, nnkSym, nnkStrLit, nnkRStrLit, nnkTripleStrLit, nnkCommentStmt}
  n.strValField

func `strVal=`*(n: NimNode; s: string) {.inline.} =
  n.strValField = s

proc intVal*(n: NimNode): BiggestInt {.inline.} =
  assert n.kind in {nnkIntLit, nnkInt8Lit, nnkInt16Lit, nnkInt32Lit, nnkInt64Lit,
                    nnkUIntLit, nnkUInt8Lit, nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit,
                    nnkCharLit}
  n.intValField

func `intVal=`*(n: NimNode; i: BiggestInt) {.inline.} =
  n.intValField = i

proc floatVal*(n: NimNode): BiggestFloat {.inline.} =
  assert n.kind in {nnkFloatLit, nnkFloat32Lit, nnkFloat64Lit}
  n.floatValField

func `floatVal=`*(n: NimNode; f: BiggestFloat) {.inline.} =
  n.floatValField = f

# Constructors
func newNimNode*(kind: NimNodeKind): NimNode =
  NimNode(kindField: kind, kids: @[])

func newEmptyNode*(): NimNode =
  newNimNode(nnkEmpty)

func newIdentNode*(ident: string): NimNode =
  result = newNimNode(nnkIdent)
  result.strValField = ident

func newStrLitNode*(s: string): NimNode =
  result = newNimNode(nnkStrLit)
  result.strValField = s

func newIntLitNode*(i: BiggestInt): NimNode =
  result = newNimNode(nnkIntLit)
  result.intValField = i

func newFloatLitNode*(f: BiggestFloat): NimNode =
  result = newNimNode(nnkFloatLit)
  result.floatValField = f

func newNilLit*(): NimNode =
  newNimNode(nnkNilLit)

func newTree*(kind: NimNodeKind; children: openArray[NimNode]): NimNode =
  result = newNimNode(kind)
  for c in children:
    result.kids.add c

func newCall*(fn: NimNode; args: openArray[NimNode]): NimNode =
  result = newNimNode(nnkCall)
  result.add fn
  for a in args:
    result.add a

func newCall*(fn: NimNode): NimNode =
  result = newNimNode(nnkCall)
  result.add fn

func newCall*(fn: string; args: openArray[NimNode]): NimNode =
  result = newNimNode(nnkCall)
  result.add newIdentNode(fn)
  for a in args:
    result.add a

func newCall*(fn: string): NimNode =
  result = newNimNode(nnkCall)
  result.add newIdentNode(fn)

func newDotExpr*(a, b: NimNode): NimNode =
  newTree(nnkDotExpr, [a, b])

func newColonExpr*(a, b: NimNode): NimNode =
  newTree(nnkExprColonExpr, [a, b])

func newStmtList*(stmts: openArray[NimNode]): NimNode =
  result = newNimNode(nnkStmtList)
  for s in stmts:
    result.add s

func newStmtList*(): NimNode =
  result = newNimNode(nnkStmtList)

func newBlockStmt*(label: NimNode; body: NimNode): NimNode =
  newTree(nnkBlockStmt, [label, body])

func newBlockStmt*(body: NimNode): NimNode =
  newTree(nnkBlockStmt, [newEmptyNode(), body])

func newVarStmt*(name, value: NimNode): NimNode =
  newTree(nnkVarSection, [newTree(nnkIdentDefs, [name, newEmptyNode(), value])])

func newLetStmt*(name, value: NimNode): NimNode =
  newTree(nnkLetSection, [newTree(nnkIdentDefs, [name, newEmptyNode(), value])])

func newConstStmt*(name, value: NimNode): NimNode =
  newTree(nnkConstSection, [newTree(nnkIdentDefs, [name, newEmptyNode(), value])])

func newAssignment*(lhs, rhs: NimNode): NimNode =
  newTree(nnkAsgn, [lhs, rhs])

func newIfStmt*(cond, body: NimNode): NimNode =
  ## Construct a single-branch if statement. Add more branches with `add`.
  result = newNimNode(nnkIfStmt)
  result.add newTree(nnkElifBranch, [cond, body])

proc `$`*(n: NimNode): string =
  ## Basic tree repr for debugging
  if isNilNode(n):
    return "nil"
  result = $n.kind
  case n.kind
  of nnkIdent, nnkSym:
    result.add "(\"" & n.strValField & "\")"
  of nnkStrLit, nnkRStrLit, nnkTripleStrLit:
    result.add "(\"" & n.strValField & "\")"
  of nnkIntLit..nnkUInt64Lit:
    result.add "(" & $n.intValField & ")"
  of nnkFloatLit..nnkFloat64Lit:
    result.add "(" & $n.floatValField & ")"
  of nnkCharLit:
    result.add "('" & $char(n.intValField) & "')"
  else:
    if n.kids.len > 0:
      result.add "("
      for i, c in n.kids:
        if i > 0: result.add ", "
        result.add $c
      result.add ")"

proc treeRepr*(n: NimNode): string =
  proc aux(n: NimNode; indent: int): string =
    if isNilNode(n):
      return "nil"
    let prefix = repeat("  ", indent)
    result = prefix & $n.kind
    case n.kind
    of nnkIdent, nnkSym:
      result.add " \"" & n.strValField & "\""
    of nnkStrLit, nnkRStrLit, nnkTripleStrLit:
      result.add " \"" & n.strValField & "\""
    of nnkIntLit..nnkUInt64Lit:
      result.add " " & $n.intValField
    of nnkFloatLit..nnkFloat64Lit:
      result.add " " & $n.floatValField
    of nnkCharLit:
      result.add " '" & $char(n.intValField) & "'"
    else:
      discard
    if n.kids.len > 0:
      for c in n.kids:
        result.add "\n" & aux(c, indent + 1)
  aux(n, 0)

func copy*(n: NimNode): NimNode =
  ## Deep copy a NimNode tree
  if isNilNode(n):
    return nil
  result = NimNode(
    kindField: n.kindField,
    kids: @[],
    strValField: n.strValField,
    intValField: n.intValField,
    floatValField: n.floatValField
  )
  for c in n.kids:
    result.kids.add copy(c)

func copyNimTree*(n: NimNode): NimNode =
  copy(n)

func ident*(s: string): NimNode = newIdentNode(s)

# ============================================================================
# NIF <-> NimNode conversion
# ============================================================================

include private/macros_nif

# ============================================================================
# Additional utilities
# ============================================================================

proc expectKind*(n: NimNode; k: NimNodeKind) =
  assert n.kind == k, "Expected " & $k & " but got " & $n.kind

proc expectLen*(n: NimNode; len: int) =
  assert n.len == len, "Expected len " & $len & " but got " & $n.len

proc expectMinLen*(n: NimNode; len: int) =
  assert n.len >= len, "Expected min len " & $len & " but got " & $n.len

func `==`*(a, b: NimNode): bool =
  ## Structural equality
  if isNilNode(a) and isNilNode(b): return true
  if isNilNode(a) or isNilNode(b): return false
  if a.kind != b.kind: return false
  case a.kind
  of nnkIdent, nnkSym, nnkStrLit, nnkRStrLit, nnkTripleStrLit, nnkCommentStmt:
    if a.strValField != b.strValField: return false
  of nnkIntLit..nnkUInt64Lit, nnkCharLit:
    if a.intValField != b.intValField: return false
  of nnkFloatLit..nnkFloat64Lit:
    if a.floatValField != b.floatValField: return false
  else:
    discard
  if a.len != b.len: return false
  for i in 0 ..< a.len:
    if a[i] != b[i]: return false
  return true

func sameTree*(a, b: NimNode): bool = a == b

func eqIdent*(a: NimNode; b: string): bool =
  ## Compare ident or sym to string (case insensitive)
  if a.kind notin {nnkIdent, nnkSym}: return false
  # Simple ASCII lowercase comparison
  let s = a.strValField
  if s.len != b.len: return false
  for i in 0 ..< s.len:
    var c1 = s[i]
    var c2 = b[i]
    if c1 in {'A'..'Z'}: c1 = char(ord(c1) + 32)
    if c2 in {'A'..'Z'}: c2 = char(ord(c2) + 32)
    if c1 != c2: return false
  return true

func eqIdent*(a, b: NimNode): bool =
  if a.kind notin {nnkIdent, nnkSym}: return false
  if b.kind notin {nnkIdent, nnkSym}: return false
  eqIdent(a, b.strValField)

# Get the first identifier in a potentially complex expression
func basename*(n: NimNode): NimNode =
  case n.kind
  of nnkIdent, nnkSym:
    result = n
  of nnkPostfix, nnkPrefix:
    result = basename(n[1])
  of nnkPragmaExpr:
    result = basename(n[0])
  else:
    result = n
