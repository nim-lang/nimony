## API for plugins.

import std / syncio
from std / os import paramStr
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, nifbuilder]

import ".." / [nimony_model]
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther

export NoLineInfo, inc, skip, typeKind, stmtKind, exprKind, info

type
  TreeStorage = ref object
    buf: TokenBuf

  Tree* = distinct TreeStorage
  FrozenTree* = distinct TreeStorage

  LineInfo* = PackedLineInfo

  Node* = object
    owner: FrozenTree
    cursor: Cursor

converter toCursor*(n: Node): Cursor {.inline.} =
  n.cursor

proc storage(tree: Tree): TreeStorage {.inline.} =
  TreeStorage(tree)

proc storage(tree: FrozenTree): TreeStorage {.inline.} =
  TreeStorage(tree)

proc createStorage(buf: sink TokenBuf; frozen: bool): TreeStorage =
  new(result)
  result.buf = buf
  if frozen:
    freeze(result.buf)

proc otherKind*(n: Node): NimonyOther {.inline.} =
  n.substructureKind

proc createTree*(): Tree =
  Tree(createStorage(createTokenBuf(), frozen = false))

proc createFrozenTree(buf: sink TokenBuf): FrozenTree =
  FrozenTree(createStorage(buf, frozen = true))

proc snapshot*(tree: Tree): FrozenTree =
  var buf = createTokenBuf(storage(tree).buf.len)
  buf.add storage(tree).buf
  result = createFrozenTree(buf)

template withTree*(t: var Tree; kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma; info: LineInfo; body: untyped) =
  storage(t).buf.addParLe(kind, info)
  body
  storage(t).buf.addParRi()

proc takeTree*(t: var Tree; n: var Node) =
  storage(t).buf.takeTree(n.cursor)

proc addDotToken*(t: var Tree) =
  storage(t).buf.addDotToken()

proc addStrLit*(t: var Tree; s: string) =
  storage(t).buf.addStrLit(s)

proc addIntLit*(t: var Tree; i: BiggestInt) =
  storage(t).buf.addIntLit(i)

proc addUIntLit*(t: var Tree; i: BiggestUInt) =
  storage(t).buf.addUIntLit(i)

proc addIdent*(t: var Tree; ident: string) =
  storage(t).buf.addIdent(ident)

proc add*(t: var TokenBuf; n: Node) =
  t.add n.cursor.load

proc takeTree*(t: var TokenBuf; n: var Node) =
  t.takeTree(n.cursor)

proc inc*(n: var Node) =
  inc n.cursor

proc skip*(n: var Node) =
  skip n.cursor

proc setInfo*(n: var Node; info: PackedLineInfo) {.inline.} =
  n.cursor.setInfo(info)

proc loadTree*(filename = paramStr(1)): FrozenTree =
  var inp = nifstreams.open(filename)
  try:
    result = createFrozenTree(fromStream(inp))
  finally:
    close(inp)

proc beginRead*(tree: FrozenTree): Node {.inline.} =
  result = Node(owner: tree, cursor: beginReadonly(storage(tree).buf))

proc beginRead*(tree: Tree): Node {.inline.} =
  result = beginRead(snapshot(tree))

proc saveTree*(tree: Tree) =
  writeFile paramStr(2), toString(storage(tree).buf)

proc saveTree*(tree: Tree; filename: string) =
  writeFile filename, toString(storage(tree).buf)

proc saveTree*(tree: FrozenTree) =
  writeFile paramStr(2), toString(storage(tree).buf)

proc saveTree*(tree: FrozenTree; filename: string) =
  writeFile filename, toString(storage(tree).buf)

type
  NifIdent* = distinct string
  NifBinding = tuple[name: string, value: string]

proc ident*(s: string): NifIdent =
  NifIdent(s)

proc parseNifFragment(text: string): FrozenTree =
  var buf = parseFromBuffer(text, "")
  if buf.len > 0 and buf[buf.len-1].kind == EofToken:
    buf.shrink(buf.len-1)
  result = createFrozenTree(buf)

proc createNode(text: string): Node =
  result = beginRead(parseNifFragment(text))

proc renderNode(n: Node): string =
  result = toString(n.cursor, false)

proc strLitNode(s: string): Node =
  var b = nifbuilder.open(s.len + 8)
  b.addStrLit(s)
  result = createNode(b.extract())

proc identNode(s: string): Node =
  var b = nifbuilder.open(s.len + 6)
  b.addIdent(s)
  result = createNode(b.extract())

proc charLitNode(c: char): Node =
  var b = nifbuilder.open(8)
  b.addCharLit(c)
  result = createNode(b.extract())

proc intLitNode(i: BiggestInt): Node =
  var b = nifbuilder.open(24)
  b.addIntLit(i)
  result = createNode(b.extract())

proc uintLitNode(u: BiggestUInt): Node =
  var b = nifbuilder.open(24)
  b.addUIntLit(u)
  result = createNode(b.extract())

proc floatLitNode(f: BiggestFloat): Node =
  var b = nifbuilder.open(32)
  b.addFloatLit(f)
  result = createNode(b.extract())

proc boolNode(v: bool): Node =
  var b = nifbuilder.open(8)
  b.addKeyw(if v: "true" else: "false")
  result = createNode(b.extract())

proc `~`*(src: Node): Node {.inline.} =
  src

proc `~`*(src: Cursor): Node =
  createNode(toString(src, false))

proc `~`*(src: TokenBuf): Node =
  createNode(toString(src, false))

proc `~`*(src: Tree): Node =
  beginRead(snapshot(src))

proc `~`*(src: FrozenTree): Node =
  beginRead(src)

proc `~`*(src: PackedToken): Node =
  createNode(nifstreams.toString([src], false))

proc `~`*(src: string): Node =
  strLitNode(src)

proc `~`*(src: NifIdent): Node =
  identNode(string(src))

proc `~`*(src: char): Node =
  charLitNode(src)

proc `~`*(src: int): Node =
  intLitNode(BiggestInt(src))

proc `~`*(src: int8): Node =
  intLitNode(BiggestInt(src))

proc `~`*(src: int16): Node =
  intLitNode(BiggestInt(src))

proc `~`*(src: int32): Node =
  intLitNode(BiggestInt(src))

proc `~`*(src: int64): Node =
  intLitNode(BiggestInt(src))

proc `~`*(src: uint): Node =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint8): Node =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint16): Node =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint32): Node =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint64): Node =
  uintLitNode(BiggestUInt(src))

proc `~`*(src: float32): Node =
  floatLitNode(BiggestFloat(src))

proc `~`*(src: float64): Node =
  floatLitNode(BiggestFloat(src))

proc `~`*(src: bool): Node =
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

proc nif*(spec: string; bindings: openArray[(string, Node)]): FrozenTree =
  var converted = newSeqOfCap[NifBinding](bindings.len)
  for entry in bindings:
    converted.add (name: entry[0], value: renderNode(entry[1]))
  parseNifFragment(expandNifSnippet(spec, converted))

proc nif*(spec: string): FrozenTree =
  parseNifFragment(spec)
