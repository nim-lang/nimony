## API for plugins.

import std / syncio
import std / parseutils
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

  NifSnippet* = distinct string

  NifBinding* = object
    name: string
    value: string

proc ident*(s: string): NifIdent =
  NifIdent(s)

proc nif*(s: string): NifSnippet =
  NifSnippet(s)

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

template toNifFragment(src: untyped): string =
  when src is Cursor:
    toString(src, false)
  elif src is TokenBuf:
    toString(src, false)
  elif src is Tree:
    toString(src.buf, false)
  elif src is PackedToken:
    nifstreams.toString([src], false)
  elif src is string:
    buildNifText(src.len + 4):
      b.addStrLit(src)
  elif src is NifIdent:
    let s = string(src)
    buildNifText(s.len + 4):
      b.addIdent(s)
  elif src is char:
    buildNifText(8):
      b.addCharLit(src)
  elif src is BiggestInt:
    buildNifText(24):
      b.addIntLit(src)
  elif src is BiggestUInt:
    buildNifText(24):
      b.addUIntLit(src)
  elif src is BiggestFloat:
    buildNifText(32):
      b.addFloatLit(src)
  elif src is SymId:
    buildNifText(32):
      b.addSymbol(pool.syms[src])
  elif src is SomeSignedInt:
    buildNifText(24):
      b.addIntLit(BiggestInt(src))
  elif src is SomeUnsignedInt:
    buildNifText(24):
      b.addUIntLit(BiggestUInt(src))
  elif src is SomeFloat:
    buildNifText(32):
      b.addFloatLit(BiggestFloat(src))
  elif src is bool:
    buildNifText(8):
      b.addKeyw(if src: "true" else: "false")
  else:
    {.error: "unsupported nif interpolation type".}

proc nifBind*[T](name: string; value: T): NifBinding =
  NifBinding(name: name, value: toNifFragment(value))

proc `=>`*[T](name: string; value: T): NifBinding =
  nifBind(name, value)

proc tree*(spec: NifSnippet): Tree =
  parseNifFragment(string(spec))

converter toTree*(spec: NifSnippet): Tree =
  tree(spec)

proc lookupBinding(bindings: openArray[NifBinding]; name: string): string =
  var i = bindings.len - 1
  while i >= 0:
    if bindings[i].name == name:
      return bindings[i].value
    dec i
  quit "missing nif substitution: " & name

proc expandNifSnippet(spec: NifSnippet; bindings: openArray[NifBinding]): string =
  result = newStringOfCap(string(spec).len + bindings.len * 8)
  for kind, value in interpolatedFragments(string(spec)):
    case kind
    of ikStr:
      result.add value
    of ikDollar:
      result.add '$'
    of ikVar:
      result.add lookupBinding(bindings, value)
    of ikExpr:
      quit "nif runtime substitution does not support ${expr}; use $name bindings"

proc subst*(spec: NifSnippet; bindings: varargs[NifBinding]): Tree =
  parseNifFragment(expandNifSnippet(spec, bindings))

proc `%`*(spec: NifSnippet; bindings: openArray[NifBinding]): Tree =
  parseNifFragment(expandNifSnippet(spec, bindings))
