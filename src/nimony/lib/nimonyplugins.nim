## API for plugins.

import std / syncio
from std / os import paramStr
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, nifbuilder]

import ".." / [nimony_model]
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther

export NoLineInfo, inc, skip, typeKind, stmtKind, exprKind, info

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

  NifBinding = tuple[name: string, value: NifNode]

proc ident*(s: string): NifIdent =
  NifIdent(s)

proc parseNifFragment(text: string): Tree =
  var buf = parseFromBuffer(text, "")
  if buf.len > 0 and buf[buf.len-1].kind == EofToken:
    buf.shrink(buf.len-1)
  result = createTree(buf)

template buildNifText(sizeHint: int; body: untyped): string =
  block:
    var b {.inject.} = nifbuilder.open(sizeHint)
    body
    b.extract()

template buildNifNode(sizeHint: int; body: untyped): NifNode =
  block:
    let s = buildNifText(sizeHint):
      body
    NifNode(s)

template toNifNode(src: untyped): NifNode =
  when src is Cursor:
    NifNode(toString(src, false))
  elif src is TokenBuf:
    NifNode(toString(src, false))
  elif src is Tree:
    NifNode(toString(src.buf, false))
  elif src is PackedToken:
    NifNode(nifstreams.toString([src], false))
  elif src is string:
    buildNifNode(src.len + 4):
      b.addStrLit(src)
  elif src is NifIdent:
    let s = string(src)
    buildNifNode(s.len + 4):
      b.addIdent(s)
  elif src is char:
    buildNifNode(8):
      b.addCharLit(src)
  elif src is BiggestInt:
    buildNifNode(24):
      b.addIntLit(src)
  elif src is BiggestUInt:
    buildNifNode(24):
      b.addUIntLit(src)
  elif src is BiggestFloat:
    buildNifNode(32):
      b.addFloatLit(src)
  elif src is SymId:
    buildNifNode(32):
      b.addSymbol(pool.syms[src])
  elif src is SomeSignedInt:
    buildNifNode(24):
      b.addIntLit(BiggestInt(src))
  elif src is SomeUnsignedInt:
    buildNifNode(24):
      b.addUIntLit(BiggestUInt(src))
  elif src is SomeFloat:
    buildNifNode(32):
      b.addFloatLit(BiggestFloat(src))
  elif src is bool:
    buildNifNode(8):
      b.addKeyw(if src: "true" else: "false")
  else:
    {.error: "unsupported nif interpolation type".}

template `~`*(src: untyped): NifNode =
  toNifNode(src)

proc nifBind(name: string; value: NifNode): NifBinding =
  (name: name, value: value)

proc lookupBinding(bindings: openArray[NifBinding]; name: string): string =
  var i = bindings.len - 1
  while i >= 0:
    if bindings[i].name == name:
      return string(bindings[i].value)
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
    converted.add nifBind(entry[0], entry[1])
  parseNifFragment(expandNifSnippet(spec, converted))

proc nif*(spec: string): Tree =
  parseNifFragment(spec)
