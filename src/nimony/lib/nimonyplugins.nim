## API for plugins.

import std / syncio
from std / os import paramStr
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, nifbuilder]

import ".." / [nimony_model]
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther

export NoLineInfo

type
  Tree* = ref object ## Mutable NIF builder used by plugins to assemble output
                     ## before freezing it into a `Node`.
    buf: TokenBuf

  LineInfo* = PackedLineInfo ## Packed source location metadata attached to NIF
                             ## tokens. Use `NoLineInfo` for synthetic output.

  Node* = object ## Read handle into a frozen NIF tree.
                 ## A `Node` behaves like a cursor positioned at one token or
                 ## subtree. Copying a `Node` retains the underlying tree
                 ## automatically; the tree is released when the last `Node`
                 ## that references it is destroyed.
    owner: Tree
    cursor: Cursor

proc createTree(buf: sink TokenBuf): Tree =
  result = Tree(buf: buf)

proc hasOwner(n: Node): bool {.inline.} =
  n.owner != nil

proc `=destroy`(n: var Node) =
  if hasOwner(n):
    endRead(n.owner.buf)

proc `=copy`(dest: var Node; src: Node) =
  if cast[pointer](addr dest) == cast[pointer](addr src):
    return
  `=destroy`(dest)
  wasMoved(dest)
  if hasOwner(src):
    dest.owner = src.owner
    dest.cursor = shareRead(dest.owner.buf, src.cursor)

proc kind*(n: Node): NifKind {.inline.} =
  ## Returns the raw NIF token kind at the current position.
  n.cursor.kind

proc info*(n: Node): PackedLineInfo {.inline.} =
  ## Returns the packed line info stored on the current token.
  n.cursor.info

proc symId*(n: Node): SymId {.inline.} =
  ## Returns the symbol id of the current token.
  ## The current token must be a `Symbol` or `SymbolDef`.
  n.cursor.symId

proc litId*(n: Node): StrId {.inline.} =
  ## Returns the literal/string-table id of the current token.
  ## The current token must be an `Ident` or `StringLit`.
  n.cursor.litId

proc charLit*(n: Node): char {.inline.} =
  ## Returns the character stored in the current `CharLit` token.
  n.cursor.charLit

proc intId*(n: Node): IntId {.inline.} =
  ## Returns the integer-pool id of the current `IntLit` token.
  n.cursor.intId

proc uintId*(n: Node): UIntId {.inline.} =
  ## Returns the unsigned-integer-pool id of the current `UIntLit` token.
  n.cursor.uintId

proc floatId*(n: Node): FloatId {.inline.} =
  ## Returns the float-pool id of the current `FloatLit` token.
  n.cursor.floatId

proc tagId*(n: Node): TagId {.inline.} =
  ## Returns the raw tag id of the current token.
  ## The current token must be a `ParLe`.
  n.cursor.tagId

proc tag*(n: Node): TagId {.inline.} =
  ## Returns the raw tag id for the current tree node, or `ErrT` if the current
  ## token is not a `ParLe`.
  n.cursor.tag

proc stmtKind*(n: Node): NimonyStmt {.inline.} =
  ## Returns the current statement kind, or `NoStmt` when the current token is
  ## not a statement node.
  n.cursor.stmtKind

proc exprKind*(n: Node): NimonyExpr {.inline.} =
  ## Returns the current expression kind, or `NoExpr` when the current token is
  ## not an expression node.
  n.cursor.exprKind

proc typeKind*(n: Node): NimonyType {.inline.} =
  ## Returns the current type kind.
  ## `DotToken` is treated as `VoidT`; non-type nodes return `NoType`.
  n.cursor.typeKind

proc otherKind*(n: Node): NimonyOther {.inline.} =
  ## Returns the current "other/substructure" kind, or `NoSub` for non-matching
  ## nodes.
  n.cursor.substructureKind

proc createTree*(): Tree =
  ## Creates an empty mutable `Tree`.
  createTree(createTokenBuf())

proc renderNode(n: Node): string

proc freeze*(tree: sink Tree): Node =
  ## Freezes a mutable `Tree` and returns a root `Node` for reading it.
  ##
  ## The returned `Node` keeps the underlying tree alive automatically. After
  ## freezing, the tree should be treated as no longer writable.
  let owner = tree
  result = Node(owner: owner, cursor: beginRead(owner.buf))

template withTree*(t: var Tree; kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma; info: LineInfo; body: untyped) =
  ## Appends a tree node of `kind` to `t`, runs `body` to emit its children, and
  ## closes the node afterwards.
  t.buf.addParLe(kind, info)
  body
  t.buf.addParRi()

proc takeTree*(t: var Tree; n: var Node) =
  ## Copies the current token or subtree from `n` into `t` and advances `n`
  ## past the copied fragment.
  t.buf.takeTree(n.cursor)

proc addDotToken*(t: var Tree) =
  ## Appends a dot placeholder token (`.`) to `t`.
  t.buf.addDotToken()

proc addStrLit*(t: var Tree; s: string) =
  ## Appends a string literal atom to `t`.
  t.buf.addStrLit(s)

proc addIntLit*(t: var Tree; i: BiggestInt) =
  ## Appends a signed integer literal atom to `t`.
  t.buf.addIntLit(i)

proc addUIntLit*(t: var Tree; i: BiggestUInt) =
  ## Appends an unsigned integer literal atom to `t`.
  t.buf.addUIntLit(i)

proc addIdent*(t: var Tree; ident: string) =
  ## Appends an identifier atom to `t`.
  t.buf.addIdent(ident)

proc add*(t: var TokenBuf; n: Node) =
  ## Appends only the current token from `n` to `t`.
  ## This does not copy an entire subtree.
  t.add n.cursor.load

proc takeTree*(t: var TokenBuf; n: var Node) =
  ## Copies the current token or subtree from `n` into `t` and advances `n`
  ## past the copied fragment.
  t.takeTree(n.cursor)

proc inc*(n: var Node) =
  ## Advances `n` by one token.
  inc n.cursor

proc skip*(n: var Node) =
  ## Skips the current token or, if positioned on `ParLe`, the entire subtree.
  skip n.cursor

proc setInfo*(n: var Node; info: PackedLineInfo) {.inline.} =
  ## Rewrites the line info of the current token in place.
  n.cursor.setInfo(info)

proc loadTree*(filename = paramStr(1)): Node =
  ## Loads a NIF file and returns a `Node` positioned at its first top-level
  ## fragment.
  var inp = nifstreams.open(filename)
  try:
    result = freeze(createTree(fromStream(inp)))
  finally:
    close(inp)

proc saveTree*(tree: Tree) =
  ## Writes the complete contents of a mutable `Tree` to `paramStr(2)`.
  writeFile paramStr(2), toString(tree.buf)

proc saveTree*(tree: Tree; filename: string) =
  ## Writes the complete contents of a mutable `Tree` to `filename`.
  writeFile filename, toString(tree.buf)

proc saveTree*(tree: Node) =
  ## Writes the current token or subtree addressed by `tree` to `paramStr(2)`.
  writeFile paramStr(2), renderNode(tree)

proc saveTree*(tree: Node; filename: string) =
  ## Writes the current token or subtree addressed by `tree` to `filename`.
  writeFile filename, renderNode(tree)

type
  NifIdent* = distinct string ## Marker type used with `~` to request an
                              ## identifier node instead of a string literal
                              ## node.
  NifBinding = tuple[name: string, value: string]

proc ident*(s: string): NifIdent =
  ## Marks `s` so that `~s` produces an identifier node rather than a string
  ## literal node.
  NifIdent(s)

proc parseNifFragment(text: string): Node =
  var buf = parseFromBuffer(text, "")
  if buf.len > 0 and buf[buf.len-1].kind == EofToken:
    buf.shrink(buf.len-1)
  result = freeze(createTree(buf))

proc createNode(text: string): Node =
  result = parseNifFragment(text)

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
  ## Returns `src` unchanged.
  src

proc `~`*(src: Cursor): Node =
  ## Copies the current token or subtree from `src` into a new owned `Node`.
  createNode(toString(src, false))

proc `~`*(src: TokenBuf): Node =
  ## Parses the contents of `src` into a new owned `Node`.
  ## Intended for buffers that contain one NIF fragment.
  createNode(toString(src, false))

proc `~`*(src: PackedToken): Node =
  ## Turns a single packed token into a new owned `Node`.
  createNode(nifstreams.toString([src], false))

proc `~`*(src: string): Node =
  ## Converts `src` into a string literal node.
  strLitNode(src)

proc `~`*(src: NifIdent): Node =
  ## Converts `src` into an identifier node.
  identNode(string(src))

proc `~`*(src: char): Node =
  ## Converts `src` into a character literal node.
  charLitNode(src)

proc `~`*(src: int): Node =
  ## Converts `src` into a signed integer literal node.
  intLitNode(BiggestInt(src))

proc `~`*(src: int8): Node =
  ## Converts `src` into a signed integer literal node.
  intLitNode(BiggestInt(src))

proc `~`*(src: int16): Node =
  ## Converts `src` into a signed integer literal node.
  intLitNode(BiggestInt(src))

proc `~`*(src: int32): Node =
  ## Converts `src` into a signed integer literal node.
  intLitNode(BiggestInt(src))

proc `~`*(src: int64): Node =
  ## Converts `src` into a signed integer literal node.
  intLitNode(BiggestInt(src))

proc `~`*(src: uint): Node =
  ## Converts `src` into an unsigned integer literal node.
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint8): Node =
  ## Converts `src` into an unsigned integer literal node.
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint16): Node =
  ## Converts `src` into an unsigned integer literal node.
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint32): Node =
  ## Converts `src` into an unsigned integer literal node.
  uintLitNode(BiggestUInt(src))

proc `~`*(src: uint64): Node =
  ## Converts `src` into an unsigned integer literal node.
  uintLitNode(BiggestUInt(src))

proc `~`*(src: float32): Node =
  ## Converts `src` into a floating-point literal node.
  floatLitNode(BiggestFloat(src))

proc `~`*(src: float64): Node =
  ## Converts `src` into a floating-point literal node.
  floatLitNode(BiggestFloat(src))

proc `~`*(src: bool): Node =
  ## Converts `src` into a `true` or `false` keyword node.
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
    elif isNifIdentStart(spec[i + 1]):
      let start = i + 1
      i = start + 1
      while i < spec.len and isNifIdentChar(spec[i]):
        inc i
      result.add lookupBinding(bindings, substr(spec, start, i - 1))
    else:
      quit "invalid nif substitution syntax"

proc nif*(spec: string; bindings: openArray[(string, Node)]): Node =
  ## Parses `spec` as a NIF fragment after expanding `$name` placeholders with
  ## the rendered nodes from `bindings`.
  ##
  ## Use `$$` for a literal dollar sign. `${...}` syntax is not supported.
  var converted = newSeqOfCap[NifBinding](bindings.len)
  for entry in bindings:
    converted.add (name: entry[0], value: renderNode(entry[1]))
  parseNifFragment(expandNifSnippet(spec, converted))

proc nif*(spec: string): Node =
  ## Parses `spec` as a literal NIF fragment and returns it as an owned `Node`.
  parseNifFragment(spec)
