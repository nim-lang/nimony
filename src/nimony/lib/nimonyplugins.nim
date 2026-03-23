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

  Tree* = object
    storage: TreeStorage

  LineInfo* = PackedLineInfo

  Node* = object
    storage: TreeStorage
    cursor: Cursor
    remaining: int
    spanLen: int

converter toCursor*(n: Node): Cursor {.inline.} =
  n.cursor

proc rawSpan(n: Node): int {.inline.} =
  if n.remaining == 0:
    result = 0
  else:
    result = n.spanLen

proc refreshSpan(n: var Node) {.inline.} =
  if n.remaining == 0:
    n.spanLen = 0
  else:
    n.spanLen = span(n.cursor)

proc createStorage(buf: sink TokenBuf): TreeStorage =
  new(result)
  result.buf = buf

proc otherKind*(n: Node): NimonyOther {.inline.} =
  n.substructureKind

proc createTree*(): Tree =
  Tree(storage: createStorage(createTokenBuf()))

proc createTree(buf: sink TokenBuf): Tree =
  Tree(storage: createStorage(buf))

template withTree*(t: var Tree; kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma; info: LineInfo; body: untyped) =
  t.storage.buf.addParLe(kind, info)
  body
  t.storage.buf.addParRi()

proc takeTree*(t: var Tree; n: var Node) =
  let count = n.rawSpan
  var cursor = n.cursor
  for _ in 0..<count:
    t.storage.buf.add cursor.load
    inc cursor
  n.cursor = cursor
  n.remaining -= count
  n.refreshSpan()

proc addDotToken*(t: var Tree) =
  t.storage.buf.addDotToken()

proc addStrLit*(t: var Tree; s: string) =
  t.storage.buf.addStrLit(s)

proc addIntLit*(t: var Tree; i: BiggestInt) =
  t.storage.buf.addIntLit(i)

proc addUIntLit*(t: var Tree; i: BiggestUInt) =
  t.storage.buf.addUIntLit(i)

proc addIdent*(t: var Tree; ident: string) =
  t.storage.buf.addIdent(ident)

proc add*(t: var TokenBuf; n: Node) =
  t.add n.cursor.load

proc takeTree*(t: var TokenBuf; n: var Node) =
  let count = n.rawSpan
  var cursor = n.cursor
  for _ in 0..<count:
    t.add cursor.load
    inc cursor
  n.cursor = cursor
  n.remaining -= count
  n.refreshSpan()

proc inc*(n: var Node) =
  inc n.cursor
  dec n.remaining
  n.refreshSpan()

proc skip*(n: var Node) =
  let count = n.rawSpan
  skip n.cursor
  n.remaining -= count
  n.refreshSpan()

proc setInfo*(n: var Node; info: PackedLineInfo) {.inline.} =
  n.cursor.setInfo(info)

proc loadTree*(filename = paramStr(1)): Tree =
  var inp = nifstreams.open(filename)
  try:
    result = createTree(fromStream(inp))
  finally:
    close(inp)

proc beginRead*(tree: Tree): Node {.inline.} =
  result = Node(
    storage: tree.storage,
    cursor: beginRead(tree.storage.buf),
    remaining: tree.storage.buf.len,
    spanLen: tree.storage.buf.len)

proc saveTree*(tree: Tree) =
  writeFile paramStr(2), toString(tree.storage.buf)

proc saveTree*(tree: Tree; filename: string) =
  writeFile filename, toString(tree.storage.buf)

type
  NifIdent* = distinct string
  NifBinding = tuple[name: string, value: string]

proc ident*(s: string): NifIdent =
  NifIdent(s)

proc parseNifFragment(text: string): Tree =
  var buf = parseFromBuffer(text, "")
  if buf.len > 0 and buf[buf.len-1].kind == EofToken:
    buf.shrink(buf.len-1)
  result = createTree(buf)

proc createNode(text: string): Node =
  result = beginRead(parseNifFragment(text))

proc renderNode(n: Node): string =
  let count = n.rawSpan
  if count == 0:
    result = ""
  else:
    var buf = createTokenBuf(count)
    var cursor = n.cursor
    for _ in 0..<count:
      buf.add cursor.load
      inc cursor
    result = toString(buf, false)

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
  createNode(toString(src.storage.buf, false))

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

proc nif*(spec: string; bindings: openArray[(string, Node)]): Tree =
  var converted = newSeqOfCap[NifBinding](bindings.len)
  for entry in bindings:
    converted.add (name: entry[0], value: renderNode(entry[1]))
  parseNifFragment(expandNifSnippet(spec, converted))

proc nif*(spec: string): Tree =
  parseNifFragment(spec)
