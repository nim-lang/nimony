## API for plugins.

import std / [strutils, syncio]
from std / os import paramStr
import ".." / ".." / "lib" / [bitabs, nifcursors, nifstreams, lineinfos, nifreader]

import ".." / [nimony_model]
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther

export NoLineInfo, inc, skip, toString, typeKind, stmtKind, exprKind, info

type
  Tree* = object
    buf: TokenBuf

  LineInfo* = PackedLineInfo

  Node* = Cursor

proc otherKind*(n: Node): NimonyOther {.inline.} =
  n.substructureKind

proc createTree*(): Tree =
  Tree(buf: createTokenBuf())

template withTree*(t: var Tree; kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma; info: LineInfo; body: untyped) =
  t.buf.addParLe(kind, info)
  body
  t.buf.addParRi()

proc takeTree*(t: var Tree; n: var Node) =
  t.buf.takeTree(n)

proc addDotToken*(t: var Tree) =
  t.buf.addDotToken()

proc addStrLit*(t: var Tree; s: string) =
  t.buf.addStrLit(s)

proc addIntLit*(t: var Tree; i: BiggestInt) =
  t.buf.addIntLit(i)

proc addUIntLit*(t: var Tree; i: BiggestUInt) =
  t.buf.addUIntLit(i)

proc addIdent*(t: var Tree; ident: string) =
  t.buf.addIdent(ident)

proc loadTree*(filename = paramStr(1)): Tree =
  var inp = nifstreams.open(filename)
  try:
    result = Tree(buf: fromStream(inp))
  finally:
    close(inp)

proc beginRead*(tree: var Tree): Node {.inline.} =
  result = beginRead(tree.buf)

proc descriptiveNodeRepr(n: Node): string =
  case n.kind
  of ParLe:
    result = pool.tags[n.tagId]
  of DotToken:
    result = "Empty"
  of Ident:
    result = "Ident " & escape(pool.strings[n.litId])
  of Symbol:
    result = "Symbol " & escape(pool.syms[n.symId])
  of SymbolDef:
    result = "SymbolDef " & escape(pool.syms[n.symId])
  of StringLit:
    result = "StringLit " & escape(pool.strings[n.litId])
  of CharLit:
    result = "CharLit " & escape($n.charLit, prefix = "'", suffix = "'")
  of IntLit:
    result = "IntLit " & $pool.integers[n.intId]
  of UIntLit:
    result = "UIntLit " & $pool.uintegers[n.uintId]
  of FloatLit:
    result = "FloatLit " & $pool.floats[n.floatId]
  of UnknownToken:
    result = "UnknownToken " & $n.getInt28
  of EofToken:
    result = "EofToken"
  of ParRi:
    result = "ParRi"

proc treeTraverse(n: Node; res: var string; level: int) =
  if level > 0:
    res.add '\n'
    for _ in 0..<level:
      res.add "  "

  res.add descriptiveNodeRepr(n)

  if n.kind == ParLe:
    var child = n
    inc child
    while child.kind != ParRi:
      treeTraverse(child, res, level + 1)
      skip child

proc treeRepr*(n: Node): string =
  result = ""
  treeTraverse(n, result, 0)

proc treeRepr*(tree: var Tree): string =
  var n = tree.beginRead()
  result = n.treeRepr

proc saveTree*(tree: Tree) =
  writeFile paramStr(2), toString(tree.buf)

proc saveTree*(tree: Tree; filename: string) =
  writeFile filename, toString(tree.buf)
