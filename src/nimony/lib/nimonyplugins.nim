## API for plugins.

import std / syncio
from std / os import paramStr
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, bitabs]

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

template sameReader(a, b: Node): bool =
  a.owner == b.owner and toUniqueId(a.cursor) == toUniqueId(b.cursor)

proc `=destroy`*(n: Node) =
  if n.owner != nil:
    endRead(n.owner.buf)

proc `=wasMoved`*(n: var Node) =
  n.owner = nil
  n.cursor = default(Cursor)

proc `=copy`*(dest: var Node; src: Node) =
  if not sameReader(dest, src):
    `=destroy`(dest)
    `=wasMoved`(dest)
    if src.owner != nil:
      dest.owner = src.owner
      dest.cursor = shareRead(dest.owner.buf, src.cursor)

proc createTree(buf: sink TokenBuf): Tree =
  result = Tree(buf: buf)

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

proc identText*(n: Node): string {.inline.} =
  ## Returns the identifier text of the current `Ident` token.
  pool.strings[n.cursor.litId]

proc stringValue*(n: Node): string {.inline.} =
  ## Returns the string contents of the current `StringLit` token.
  pool.strings[n.cursor.litId]

proc charLit*(n: Node): char {.inline.} =
  ## Returns the character stored in the current `CharLit` token.
  n.cursor.charLit

proc intValue*(n: Node): BiggestInt {.inline.} =
  ## Returns the integer value stored in the current `IntLit` token.
  pool.integers[n.cursor.intId]

proc uintValue*(n: Node): BiggestUInt {.inline.} =
  ## Returns the unsigned integer value stored in the current `UIntLit` token.
  pool.uintegers[n.cursor.uintId]

proc floatValue*(n: Node): BiggestFloat {.inline.} =
  ## Returns the floating-point value stored in the current `FloatLit` token.
  pool.floats[n.cursor.floatId]

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

proc renderNode*(n: Node): string

proc freeze*(tree: sink Tree): Node =
  ## Freezes a mutable `Tree` and returns a root `Node` for reading it.
  ##
  ## The returned `Node` keeps the underlying tree alive automatically. After
  ## freezing, the tree should be treated as no longer writable.
  let owner = tree
  result = Node(owner: owner, cursor: beginRead(owner.buf))

template withTree*(t: Tree; kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma; info: LineInfo; body: untyped) =
  ## Appends a tree node of `kind` to `t`, runs `body` to emit its children, and
  ## closes the node afterwards.
  t.buf.addParLe(kind, info)
  body
  t.buf.addParRi()

proc tagId*(n: Node): TagId {.inline.} =
  ## Returns the raw tag id of the current token.
  ## The current token must be a `ParLe`.
  n.cursor.tagId

proc tag*(n: Node): TagId {.inline.} =
  ## Returns the raw tag id for the current tree node, or `ErrT` if the current
  ## token is not a `ParLe`.
  n.cursor.tag

proc addParLe*(t: Tree; tag: TagId; info: LineInfo = NoLineInfo) =
  ## Appends a raw opening tree token with tag `tag` to `t`.
  t.buf.addParLe(tag, info)

proc addParRi*(t: Tree) =
  ## Appends a closing tree token (`)`) to `t`.
  t.buf.addParRi()

proc takeTree*(t: Tree; n: var Node) =
  ## Copies the current token or subtree from `n` into `t` and advances `n`
  ## past the copied fragment.
  t.buf.takeTree(n.cursor)

proc addDotToken*(t: Tree) =
  ## Appends a dot placeholder token (`.`) to `t`.
  t.buf.addDotToken()

proc addStrLit*(t: Tree; s: string) =
  ## Appends a string literal atom to `t`.
  t.buf.addStrLit(s)

proc addIntLit*(t: Tree; i: BiggestInt) =
  ## Appends a signed integer literal atom to `t`.
  t.buf.addIntLit(i)

proc addUIntLit*(t: Tree; i: BiggestUInt) =
  ## Appends an unsigned integer literal atom to `t`.
  t.buf.addUIntLit(i)

proc addIdent*(t: Tree; ident: string) =
  ## Appends an identifier atom to `t`.
  t.buf.addIdent(ident)

proc addCharLit*(t: Tree; c: char) =
  ## Appends a character literal atom to `t`.
  t.buf.addCharLit(c)

proc addFloatLit*(t: Tree; f: BiggestFloat) =
  ## Appends a floating-point literal atom to `t`.
  t.buf.addFloatLit(f)

proc addSymUse*(t: Tree; s: SymId; info: LineInfo = NoLineInfo) =
  ## Appends a symbol-use atom to `t`.
  t.buf.addSymUse(s, info)

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

type
  NifIdent* = distinct string ## Marker type used with `~` to request an
                              ## identifier node instead of a string literal
                              ## node.
  NifBinding* = tuple[name: string, value: Node]

proc ident*(s: string): NifIdent =
  ## Marks `s` so that `~s` produces an identifier node rather than a string
  ## literal node.
  NifIdent(s)

proc isNifIdentStart(c: char): bool =
  c in {'a'..'z', 'A'..'Z', '_'}

proc isNifIdentChar(c: char): bool =
  c in {'a'..'z', 'A'..'Z', '0'..'9', '_'}

proc createNode(buf: sink TokenBuf): Node =
  result = freeze(createTree(buf))

proc parseNifBuffer(text: string): TokenBuf =
  result = parseFromBuffer(text, "")
  if result.len > 0 and result[result.len-1].kind == EofToken:
    result.shrink(result.len-1)

proc parseNifFragment(text: string): Node =
  result = createNode(parseNifBuffer(text))

proc lookupBinding(bindings: openArray[NifBinding]; name: string): int =
  var i = bindings.len - 1
  while i >= 0:
    if bindings[i].name == name:
      return i
    dec i
  quit "missing nif substitution: " & name

proc appendParsedText(dest: var TokenBuf; text: string) =
  if text.len > 0:
    dest.add parseNifBuffer(text)

proc appendNode(dest: var TokenBuf; n: Node) =
  var c = n.cursor
  dest.takeTree(c)

proc parseNifTemplate(spec: string; bindings: openArray[NifBinding]): Node =
  var buf = createTokenBuf(spec.len + bindings.len * 4)
  var literal = newStringOfCap(spec.len)
  var i = 0
  while i < spec.len:
    if spec[i] != '$':
      literal.add spec[i]
      inc i
    elif i + 1 >= spec.len:
      quit "invalid nif substitution syntax"
    elif spec[i + 1] == '$':
      literal.add '$'
      inc i, 2
    elif isNifIdentStart(spec[i + 1]):
      let start = i + 1
      i = start + 1
      while i < spec.len and isNifIdentChar(spec[i]):
        inc i
      appendParsedText(buf, literal)
      setLen(literal, 0)
      appendNode(buf, bindings[lookupBinding(bindings, substr(spec, start, i - 1))].value)
    else:
      quit "invalid nif substitution syntax"

  appendParsedText(buf, literal)
  result = createNode(buf)

proc renderNode*(n: Node): string =
  result = toString(n.cursor, false)

proc strLitNode(s: string): Node =
  var buf = createTokenBuf(1)
  buf.addStrLit(s)
  result = createNode(buf)

proc identNode(s: string): Node =
  var buf = createTokenBuf(1)
  buf.addIdent(s)
  result = createNode(buf)

proc charLitNode(c: char): Node =
  var buf = createTokenBuf(1)
  buf.addCharLit(c)
  result = createNode(buf)

proc intLitNode(i: BiggestInt): Node =
  var buf = createTokenBuf(1)
  buf.addIntLit(i)
  result = createNode(buf)

proc uintLitNode(u: BiggestUInt): Node =
  var buf = createTokenBuf(1)
  buf.addUIntLit(u)
  result = createNode(buf)

proc floatLitNode(f: BiggestFloat): Node =
  var buf = createTokenBuf(1)
  buf.addFloatLit(f)
  result = createNode(buf)

proc boolNode(v: bool): Node =
  var buf = createTokenBuf(2)
  buf.addParLe(if v: TrueX else: FalseX)
  buf.addParRi()
  result = createNode(buf)

template `~`*(src: Node): Node =
  ## Returns `src` unchanged.
  src

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

proc nif*(spec: string; bindings: openArray[NifBinding]): Node =
  ## Parses `spec` as a NIF fragment after expanding `$name` placeholders with
  ## the corresponding nodes from `bindings`.
  ##
  ## Use `$$` for a literal dollar sign. `${...}` syntax is not supported.
  parseNifTemplate(spec, bindings)

proc nif*(spec: string): Node =
  ## Parses `spec` as a literal NIF fragment and returns it as an owned `Node`.
  parseNifFragment(spec)
