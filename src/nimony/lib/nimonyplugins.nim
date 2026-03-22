## API for plugins.

import std / syncio
from std / os import paramStr
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, nifbuilder]

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

proc createTree(buf: sink TokenBuf): Tree =
  Tree(buf: buf)

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

proc saveTree*(tree: Tree) =
  writeFile paramStr(2), toString(tree.buf)

proc saveTree*(tree: Tree; filename: string) =
  writeFile filename, toString(tree.buf)

type
  NifIdent* = distinct string

  NifNode* = distinct string

  NifBinding = tuple[name: string, value: string]

proc ident*(s: string): NifIdent =
  NifIdent(s)

proc parseNifFragment(text: string): Tree =
  var buf = parseFromBuffer(text, "")
  if buf.len > 0 and buf[buf.len-1].kind == EofToken:
    buf.shrink(buf.len-1)
  result = createTree(buf)

proc strLitNode(s: string): NifNode =
  var b = nifbuilder.open(s.len + 4)
  b.addStrLit(s)
  result = NifNode(b.extract())

proc identNode(s: string): NifNode =
  var b = nifbuilder.open(s.len + 4)
  b.addIdent(s)
  result = NifNode(b.extract())

proc charLitNode(c: char): NifNode =
  var b = nifbuilder.open(8)
  b.addCharLit(c)
  result = NifNode(b.extract())

proc intLitNode(i: BiggestInt): NifNode =
  var b = nifbuilder.open(24)
  b.addIntLit(i)
  result = NifNode(b.extract())

proc uintLitNode(u: BiggestUInt): NifNode =
  var b = nifbuilder.open(24)
  b.addUIntLit(u)
  result = NifNode(b.extract())

proc floatLitNode(f: BiggestFloat): NifNode =
  var b = nifbuilder.open(32)
  b.addFloatLit(f)
  result = NifNode(b.extract())

proc boolNode(v: bool): NifNode =
  var b = nifbuilder.open(8)
  b.addKeyw(if v: "true" else: "false")
  result = NifNode(b.extract())

proc `~`*(src: NifNode): NifNode {.inline.} =
  src

proc `~`*(src: Cursor): NifNode =
  NifNode(toString(src, false))

proc `~`*(src: TokenBuf): NifNode =
  NifNode(toString(src, false))

proc `~`*(src: Tree): NifNode =
  NifNode(toString(src.buf, false))

proc `~`*(src: PackedToken): NifNode =
  NifNode(nifstreams.toString([src], false))

proc `~`*(src: string): NifNode =
  strLitNode(src)

proc `~`*(src: NifIdent): NifNode =
  identNode(string(src))

proc `~`*(src: char): NifNode =
  charLitNode(src)

proc `~`*(src: int): NifNode =
  intLitNode(BiggestInt(src))

proc `~`*(src: int8): NifNode =
  intLitNode(BiggestInt(src))

proc `~`*(src: int16): NifNode =
  intLitNode(BiggestInt(src))

proc `~`*(src: int32): NifNode =
  intLitNode(BiggestInt(src))

proc `~`*(src: int64): NifNode =
  intLitNode(BiggestInt(src))

proc `~`*(src: uint): NifNode =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint8): NifNode =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint16): NifNode =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint32): NifNode =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint64): NifNode =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: float32): NifNode =
  floatLitNode(BiggestFloat(src))

proc `~`*(src: float64): NifNode =
  floatLitNode(BiggestFloat(src))

proc `~`*(src: bool): NifNode =
  boolNode(src)

proc lookupBinding(bindings: openArray[NifBinding]; name: string): string =
  var i = bindings.len - 1
  while i >= 0:
    if bindings[i].name == name:
      return bindings[i].value
    dec i
  quit "missing nif substitution: " & name

proc isNifIdentStart(c: char): bool {.inline.} =
  c in {'a'..'z', 'A'..'Z', '_'}

proc isNifIdentChar(c: char): bool {.inline.} =
  c in {'a'..'z', 'A'..'Z', '0'..'9', '_'}

proc expandNifSnippet(spec: string; bindings: openArray[NifBinding]): string =
  result = newStringOfCap(spec.len + bindings.len * 8)
  var i = 0
  while i < spec.len:
    if spec[i] != '$':
      result.add spec[i]
      inc i
    elif i + 1 >= spec.len:
      quit "invalid nif substitution syntax"
    elif spec[i + 1] == '$':
      result.add '$'
      inc i, 2
    elif spec[i + 1] == '{':
      quit "nif runtime substitution does not support ${expr}; use $name bindings"
    elif isNifIdentStart(spec[i + 1]):
      let start = i + 1
      i = start + 1
      while i < spec.len and isNifIdentChar(spec[i]):
        inc i
      result.add lookupBinding(bindings, substr(spec, start, i - 1))
    else:
      quit "invalid nif substitution syntax"

proc nif*(spec: string; bindings: openArray[(string, NifNode)]): Tree =
  var converted = newSeqOfCap[NifBinding](bindings.len)
  for entry in bindings:
    converted.add (name: entry[0], value: string(entry[1]))
  parseNifFragment(expandNifSnippet(spec, converted))

proc nif*(spec: string): Tree =
  parseNifFragment(spec)
