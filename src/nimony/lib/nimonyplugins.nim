## API for plugins.

import std / [assertions, hashes, syncio]
from std / os import paramStr
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, bitabs]

import ".." / [nimony_model]
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther, NifKind, NoLineInfo

type
  TreePayload = object
    counter: int
    buf: TokenBuf

  Tree* = object ## Mutable NIF builder used by plugins to assemble output.
                 ## Copying a tree shares the underlying payload until the next
                 ## mutation detaches it.
    p: ptr TreePayload

  LineInfo* = PackedLineInfo ## Packed source location metadata attached to NIF
                             ## tokens. Use `NoLineInfo` for synthetic output.

  SourcePos* = object ## Decoded source position for plugin-facing APIs.
    line*: int
    col*: int

  Node* = object ## Read handle into a frozen NIF tree.
                 ## A `Node` behaves like a cursor positioned at one token or
                 ## subtree. Copying a `Node` retains the underlying tree
                 ## snapshot automatically via the Cursor's COW mechanism.
    cursor: Cursor

  SymId* = object ## Stable plugin-facing symbol handle.
                  ## This intentionally avoids exposing the compiler's raw
                  ## numeric symbol ids as plain integers.
    raw: nifstreams.SymId

  TagId* = nifstreams.TagId ## Raw plugin-facing tag id.
                            ## Tag ids stay numeric because plugins read and
                            ## write NIF as text; these ordinals never cross
                            ## process boundaries.

proc `=destroy`*(x: Tree) =
  if x.p != nil:
    if x.p.counter == 0:
      `=destroy`(x.p[].buf)
      dealloc(x.p)
    else:
      dec x.p.counter

proc `=wasMoved`*(x: var Tree) =
  x.p = nil

proc `=copy`*(dest: var Tree; src: Tree) =
  if dest.p != src.p:
    `=destroy`(dest)
    if src.p != nil:
      inc src.p.counter
    dest.p = src.p

proc `=dup`*(x: Tree): Tree {.nodestroy.} =
  result = Tree(p: x.p)
  if result.p != nil:
    inc result.p.counter

proc initPayload(buf: sink TokenBuf): ptr TreePayload =
  result = cast[ptr TreePayload](alloc0(sizeof(TreePayload)))
  result.counter = 0
  result.buf = ensureMove(buf)

proc copyBuffer(buf: TokenBuf): TokenBuf =
  result = createTokenBuf(max(buf.len, 4))
  result.add buf

proc hasSubtree(n: Node): bool {.inline.} =
  hasCurrentToken(n.cursor) and n.cursor.kind != ParRi

proc createTree(buf: sink TokenBuf): Tree =
  result = Tree(p: initPayload(buf))

proc prepareMutation(t: var Tree) =
  if t.p == nil:
    t = createTree(createTokenBuf())
  elif t.p.counter > 0:
    let oldP = t.p
    t.p = initPayload(copyBuffer(oldP.buf))
    dec oldP.counter

proc isEmpty*(tree: Tree): bool {.inline.} =
  ## Returns true when `tree` does not currently contain any tokens.
  tree.p == nil or tree.p[].buf.len == 0

proc kind*(n: Node): NifKind {.inline.} =
  ## Returns the raw NIF token kind at the current position.
  n.cursor.kind

proc info*(n: Node): LineInfo {.inline.} =
  ## Returns the packed line info stored on the current token.
  n.cursor.info

proc isValid*(info: LineInfo): bool {.inline.} =
  ## Returns true when `info` refers to a real source location.
  lineinfos.isValid(info)

proc filePath*(info: LineInfo): string =
  ## Returns the source path stored in `info`, or `""` when unavailable.
  if info.isValid:
    let rawInfo = unpack(pool.man, info)
    if rawInfo.file.isValid:
      result = pool.files[rawInfo.file]
    else:
      result = ""
  else:
    result = ""

proc lineCol*(info: LineInfo): SourcePos =
  ## Returns the 1-based `(line, col)` stored in `info`, or `(0, 0)` when
  ## unavailable.
  if info.isValid:
    let rawInfo = unpack(pool.man, info)
    result = SourcePos(line: int(rawInfo.line), col: int(rawInfo.col))
  else:
    result = SourcePos(line: 0, col: 0)

proc `==`*(a, b: SymId): bool {.inline.} =
  a.raw == b.raw

proc hash*(x: SymId): Hash {.inline.} =
  hash(x.raw)

proc `$`*(x: SymId): string {.inline.} =
  pool.syms[x.raw]

proc `$`*(x: TagId): string {.inline.} =
  ## Renders `x` as its textual NIF tag name.
  pool.tags[x]

proc symText*(s: SymId): string {.inline.} =
  ## Returns the symbol text stored in the plugin-facing symbol handle.
  pool.syms[s.raw]

proc tagText*(t: TagId): string {.inline.} =
  ## Returns the textual NIF tag name for `t`.
  pool.tags[t]

proc symId*(n: Node): SymId {.inline.} =
  ## Returns the symbol id of the current token as an opaque handle.
  ## The current token must be a `Symbol` or `SymbolDef`.
  SymId(raw: n.cursor.symId)

proc symText*(n: Node): string {.inline.} =
  ## Returns the symbol text of the current `Symbol` or `SymbolDef` token.
  pool.syms[n.cursor.symId]

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

proc pragmaKind*(n: Node): NimonyPragma {.inline.} =
  ## Returns the current pragma kind, or `NoPragma` for non-matching nodes.
  n.cursor.pragmaKind

proc createTree*(): Tree =
  ## Creates an empty mutable `Tree`.
  createTree(createTokenBuf())

proc snapshot*(tree: var Tree): Node =
  ## Returns a read-only snapshot positioned at the first top-level token of
  ## `tree`.
  ##
  ## The returned `Node` keeps the underlying data alive automatically via
  ## the Cursor's COW mechanism. The original tree remains writable and
  ## detaches on the next mutation.
  ##
  ## `tree` must not be empty; use `isEmpty` first when that is expected.
  assert not tree.isEmpty, "cannot snapshot empty Tree"
  result = Node(cursor: beginRead(tree.p[].buf))

template withTree*(t: var Tree; kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma; info: LineInfo; body: untyped) =
  ## Appends a tree node of `kind` to `t`, runs `body` to emit its children, and
  ## closes the node afterwards.
  prepareMutation(t)
  t.p[].buf.addParLe(kind, info)
  body
  t.p[].buf.addParRi()

proc tagId*(n: Node): TagId {.inline.} =
  ## Returns the raw tag id of the current token.
  ## The current token must be a `ParLe`.
  n.cursor.tagId

proc tagText*(n: Node): string {.inline.} =
  ## Returns the tag text of the current `ParLe` token.
  pool.tags[n.cursor.tagId]

proc tag*(n: Node): TagId {.inline.} =
  ## Returns the raw tag id for the current tree node, or `ErrT` if the
  ## current token is not a `ParLe`.
  n.cursor.tag

proc addParLe*(t: var Tree; tag: TagId; info: LineInfo = NoLineInfo) =
  ## Appends an opening tree token with raw tag id `tag` to `t`.
  ## Use `addParLe(tagText, ...)` when constructing nodes from textual tag
  ## names instead of existing ids.
  prepareMutation(t)
  t.p[].buf.addParLe(tag, info)

proc addParLe*(t: var Tree; tag: string; info: LineInfo = NoLineInfo) =
  ## Appends an opening tree token with textual tag `tag` to `t`.
  prepareMutation(t)
  t.p[].buf.addParLe(pool.tags.getOrIncl(tag), info)

proc addParRi*(t: var Tree) =
  ## Appends a closing tree token (`)`) to `t`.
  prepareMutation(t)
  t.p[].buf.addParRi()

proc takeTree*(t: var Tree; n: var Node) =
  ## Copies the current token or subtree from `n` into `t` and advances `n`
  ## past the copied fragment.
  prepareMutation(t)
  t.p[].buf.takeTree(n.cursor)

proc addSubtree*(t: var Tree; n: Node) =
  ## Copies the current token or subtree from `n` into `t` without advancing it.
  prepareMutation(t)
  t.p[].buf.addSubtree(n.cursor)

proc add*(t: var Tree; child: Tree) =
  ## Appends the complete contents of `child` to `t`.
  if not child.isEmpty:
    prepareMutation(t)
    t.p[].buf.add child.p[].buf

proc addDotToken*(t: var Tree) =
  ## Appends a dot placeholder token (`.`) to `t`.
  prepareMutation(t)
  t.p[].buf.addDotToken()

proc addStrLit*(t: var Tree; s: string) =
  ## Appends a string literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addStrLit(s)

proc addIntLit*(t: var Tree; i: BiggestInt) =
  ## Appends a signed integer literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addIntLit(i)

proc addUIntLit*(t: var Tree; i: BiggestUInt) =
  ## Appends an unsigned integer literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addUIntLit(i)

proc addIdent*(t: var Tree; ident: string) =
  ## Appends an identifier atom to `t`.
  prepareMutation(t)
  t.p[].buf.addIdent(ident)

proc addCharLit*(t: var Tree; c: char) =
  ## Appends a character literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addCharLit(c)

proc addFloatLit*(t: var Tree; f: BiggestFloat) =
  ## Appends a floating-point literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addFloatLit(f)

proc addSymUse*(t: var Tree; s: SymId; info: LineInfo = NoLineInfo) =
  ## Appends a symbol-use atom named by the opaque handle `s` to `t`.
  prepareMutation(t)
  t.p[].buf.addSymUse(s.raw, info)

proc addSymUse*(t: var Tree; s: string; info: LineInfo = NoLineInfo) =
  ## Appends a symbol-use atom named `s` to `t`.
  prepareMutation(t)
  t.p[].buf.addSymUse(pool.syms.getOrIncl(s), info)

proc addEmptyNode*(t: var Tree; info: LineInfo = NoLineInfo) =
  ## Appends a single empty placeholder node (`.`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty(info)

proc addEmptyNode2*(t: var Tree; info: LineInfo = NoLineInfo) =
  ## Appends two empty placeholder nodes (`. .`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty2(info)

proc addEmptyNode3*(t: var Tree; info: LineInfo = NoLineInfo) =
  ## Appends three empty placeholder nodes (`. . .`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty3(info)

proc addEmptyNode4*(t: var Tree; info: LineInfo = NoLineInfo) =
  ## Appends four empty placeholder nodes (`. . . .`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty3(info)
  t.p[].buf.addEmpty(info)

proc inc*(n: var Node) =
  ## Advances `n` by one token.
  inc n.cursor

proc skip*(n: var Node) =
  ## Skips the current token or, if positioned on `ParLe`, the entire subtree.
  skip n.cursor

proc eqIdent*(n: Node; name: string): bool =
  ## Returns true when `n` matches `name` exactly.
  case n.kind
  of Ident:
    result = n.identText == name
  of Symbol, SymbolDef:
    result = n.symText == name
  else:
    result = false

proc loadPluginInput*(filename = paramStr(1)): Node =
  ## Loads a NIF file and returns a root `Node` for reading it.
  var inp = nifstreams.open(filename)
  try:
    var tree = createTree(fromStream(inp))
    result = snapshot(tree)
    # tree is destroyed here, but Node's cursor keeps data alive via COW
  finally:
    close(inp)

proc renderTree*(tree: Tree): string =
  ## Renders the complete contents of `tree` as raw NIF text for debugging.
  ## Unlike `saveTree`, this omits line info and may contain multiple
  ## top-level fragments when the tree is still under construction.
  if tree.p == nil:
    result = ""
  else:
    result = toString(tree.p[].buf, false)

proc saveTree*(tree: Tree; filename: string) =
  ## Writes the complete contents of a mutable `Tree` to `filename`.
  ## This preserves line info because it is intended for `.nif` output.
  if tree.p == nil:
    writeFile filename, ""
  else:
    writeFile filename, toString(tree.p[].buf)

proc saveTree*(tree: Tree) =
  ## Writes the complete contents of a mutable `Tree` to `paramStr(2)`.
  ## This preserves line info because it is intended for `.nif` output.
  saveTree(tree, paramStr(2))

type
  NifIdent* = distinct string ## Marker type used with `~` to request an
                              ## identifier node instead of a string literal
                              ## node.
  NifBinding* = tuple[name: string, value: Tree]

proc ident*(s: string): NifIdent =
  ## Marks `s` so that `~s` produces an identifier node rather than a string
  ## literal node.
  NifIdent(s)

proc isNifIdentStart(c: char): bool {.inline.} =
  c in {'a'..'z', 'A'..'Z', '_'}

proc isNifIdentChar(c: char): bool {.inline.} =
  c in {'a'..'z', 'A'..'Z', '0'..'9', '_'}

proc validateConstructedTree(tree: sink Tree): Tree

proc parseNifBuffer(text: string): TokenBuf =
  result = parseFromBuffer(text, "")
  if result.len > 0 and result[result.len-1].kind == EofToken:
    result.shrink(result.len-1)

type
  ChildShape = enum
    AnyChild, ExprChild, StmtChild, TypeChild, OtherChild, SymUseChild,
    SymDefChild, IntLitChild, StringLitChild, CharLitChild

  ValidationError = object
    found: bool
    info: PackedLineInfo
    msg: string
    orig: Cursor

proc validationError(info: PackedLineInfo; msg: string; orig: Cursor): ValidationError =
  ValidationError(found: true, info: info, msg: msg, orig: orig)

proc createErrorTree(info: PackedLineInfo; msg: string; orig: Cursor): Tree =
  var buf = createTokenBuf(8)
  buf.buildTree ErrT, info:
    buf.addSubtree(orig)
    buf.addStrLit(msg, info)
  result = createTree(buf)

proc createErrorTree(info: PackedLineInfo; msg: string): Tree =
  var orig = createTokenBuf(1)
  orig.addDotToken()
  result = createErrorTree(info, msg, cursorAt(orig, 0))

proc errorInfo(n: Node): PackedLineInfo =
  if hasSubtree(n):
    n.info
  else:
    NoLineInfo

proc errorTree*(msg: string): Tree =
  ## Produces an `ErrT` tree with synthetic line info.
  createErrorTree(NoLineInfo, msg)

proc errorTree*(msg: string; at: Node): Tree =
  ## Produces an `ErrT` tree located at `at` and embeds `at` as source.
  if hasSubtree(at):
    createErrorTree(at.info, msg, at.cursor)
  else:
    createErrorTree(NoLineInfo, msg)

proc errorTree*(msg: string; at, orig: Node): Tree =
  ## Produces an `ErrT` tree located at `at` and embeds `orig` as source.
  if hasSubtree(orig):
    createErrorTree(errorInfo(at), msg, orig.cursor)
  else:
    createErrorTree(errorInfo(at), msg)

template isSupportedTag(n: Cursor): bool =
  let raw = tagEnum(n)
  rawTagIsNimonyExpr(raw) or rawTagIsNimonyStmt(raw) or
  rawTagIsNimonyType(raw) or rawTagIsNimonyOther(raw) or
  rawTagIsNimonyPragma(raw) or rawTagIsNimonySym(raw) or
  rawTagIsControlFlowKind(raw) or rawTagIsCallConv(raw)

proc describeShape(shape: ChildShape): string =
  case shape
  of AnyChild: "node"
  of ExprChild: "expression"
  of StmtChild: "statement"
  of TypeChild: "type"
  of OtherChild: "sub-node"
  of SymUseChild: "symbol use"
  of SymDefChild: "symbol definition"
  of IntLitChild: "integer literal"
  of StringLitChild: "string literal"
  of CharLitChild: "character literal"

proc tagText(n: Cursor): string {.inline.} =
  pool.tags[n.tagId]

proc hasSubtree(n: Cursor): bool {.inline.} =
  hasCurrentToken(n) and n.kind != ParRi

proc errorInfo(n: Cursor): PackedLineInfo =
  if hasSubtree(n):
    n.info
  else:
    NoLineInfo

proc matchesShape(n: Cursor; shape: ChildShape): bool =
  if n.kind == ParLe and n.tagId == ErrT:
    return true
  case shape
  of AnyChild:
    result = true
  of ExprChild:
    case n.kind
    of ParLe:
      result = rawTagIsNimonyExpr(tagEnum(n))
    of Symbol, Ident, IntLit, UIntLit, FloatLit, StringLit, CharLit:
      result = true
    else:
      result = false
  of StmtChild:
    result = n.kind == ParLe and
      (rawTagIsNimonyStmt(tagEnum(n)) or rawTagIsControlFlowKind(tagEnum(n)))
  of TypeChild:
    result = n.kind == DotToken or (n.kind == ParLe and rawTagIsNimonyType(tagEnum(n)))
  of OtherChild:
    result = n.kind == ParLe and rawTagIsNimonyOther(tagEnum(n))
  of SymUseChild:
    result = n.kind in {Symbol, Ident}
  of SymDefChild:
    result = n.kind in {SymbolDef, Symbol, Ident}
  of IntLitChild:
    result = n.kind in {IntLit, UIntLit}
  of StringLitChild:
    result = n.kind == StringLit
  of CharLitChild:
    result = n.kind == CharLit

proc consumeRemainingChildren(n: var Cursor) =
  while n.kind != ParRi:
    skip n
  inc n

proc validateChildren(n: var Cursor; parent: Cursor; shapes: openArray[ChildShape];
    allowMore = false): ValidationError =
  result = default(ValidationError)
  for i in 0 ..< shapes.len:
    if n.kind == ParRi:
      return validationError(parent.info,
        "missing child " & $(i + 1) & " for '" & parent.tagText &
        "': expected " & describeShape(shapes[i]), parent)
    if not matchesShape(n, shapes[i]):
      return validationError(errorInfo(n),
        "invalid child " & $(i + 1) & " for '" & parent.tagText &
        "': expected " & describeShape(shapes[i]), parent)
    skip n

  if not allowMore and n.kind != ParRi:
    return validationError(errorInfo(n),
      "unexpected child " & $(shapes.len + 1) & " for '" & parent.tagText & "'", parent)

  consumeRemainingChildren(n)

proc validateExpr(n: var Cursor; parent: Cursor): ValidationError =
  result = default(ValidationError)
  case parent.exprKind
  of AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX,
      EqX, NeqX, LeX, LtX, AshrX, EqsetX, LesetX, LtsetX, InsetX:
    result = validateChildren(n, parent, [TypeChild, ExprChild, ExprChild])
  of BitnotX, CastX, ConvX, HconvX, DconvX, CardX:
    result = validateChildren(n, parent, [TypeChild, ExprChild])
  of AtX, PatX, AndX, OrX, XorX, CurlyatX, CopyX, SinkhX, TraceX, IsX:
    result = validateChildren(n, parent, [ExprChild, ExprChild])
  of NotX, NegX, DerefX, AddrX, ParX, EmoveX, DestroyX, DupX, WasmovedX,
      CompilesX, DeclaredX, DefinedX, AstToStrX, HighX, LowX, EnumtostrX,
      InternalTypeNameX, FailedX:
    result = validateChildren(n, parent, [ExprChild])
  of SizeofX, AlignofX, NewrefX, DefaultobjX, DefaulttupX:
    result = validateChildren(n, parent, [TypeChild])
  of OffsetofX, InstanceofX, EnvpX:
    result = validateChildren(n, parent, [TypeChild, ExprChild])
  of TypeofX, FieldsX, FieldpairsX:
    result = validateChildren(n, parent, [TypeChild, ExprChild], allowMore = true)
  of CallX, CmdX, HcallX, ProccallX, CallstrlitX:
    result = validateChildren(n, parent, [ExprChild], allowMore = true)
  else:
    consumeRemainingChildren(n)

proc validateStmt(n: var Cursor; parent: Cursor): ValidationError =
  result = default(ValidationError)
  case parent.stmtKind
  of VarS, LetS, ConstS, GvarS, TvarS, GletS, TletS, CursorS, PatternvarS, ProcS,
      FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS,
      ResultS:
    result = validateChildren(n, parent, [SymDefChild], allowMore = true)
  of BlockS:
    result = validateChildren(n, parent, [AnyChild, StmtChild])
  else:
    consumeRemainingChildren(n)

proc validateType(n: var Cursor; parent: Cursor): ValidationError =
  result = default(ValidationError)
  case parent.typeKind
  of ArrayT, RangetypeT:
    result = validateChildren(n, parent, [TypeChild, ExprChild, ExprChild])
  of PtrT, RefT, MutT, OutT, LentT, SinkT, DistinctT, TypedescT,
      UarrayT, SetT:
    result = validateChildren(n, parent, [TypeChild])
  of ObjectT:
    result = validateChildren(n, parent, [TypeChild], allowMore = true)
  else:
    consumeRemainingChildren(n)

proc validateSubstructure(n: var Cursor; parent: Cursor): ValidationError =
  result = default(ValidationError)
  case parent.substructureKind
  of RangeU:
    result = validateChildren(n, parent, [ExprChild, ExprChild])
  of ParamU, TypevarU, FldU, EfldU:
    result = validateChildren(n, parent, [SymDefChild], allowMore = true)
  else:
    consumeRemainingChildren(n)

proc validatePragma(n: var Cursor; parent: Cursor): ValidationError =
  result = default(ValidationError)
  case parent.pragmaKind
  of PragmaP:
    result = validateChildren(n, parent, [SymDefChild], allowMore = true)
  else:
    consumeRemainingChildren(n)

proc validateShape(n: Cursor): ValidationError =
  result = default(ValidationError)
  let parent = n
  var child = n
  inc child
  if parent.exprKind != NoExpr:
    result = validateExpr(child, parent)
  elif parent.stmtKind != NoStmt:
    result = validateStmt(child, parent)
  elif parent.typeKind != NoType:
    result = validateType(child, parent)
  elif parent.substructureKind != NoSub:
    result = validateSubstructure(child, parent)
  elif parent.pragmaKind != NoPragma:
    result = validatePragma(child, parent)
  else:
    consumeRemainingChildren(child)

proc validateConstructedNode(n: var Cursor): ValidationError =
  result = default(ValidationError)
  var nested = 0
  while true:
    case n.kind
    of ParLe:
      if n.tagId != ErrT:
        if not isSupportedTag(n):
          return validationError(n.info, "unsupported NIF tag '" & n.tagText & "'", n)
        result = validateShape(n)
        if result.found:
          return
      inc nested
      inc n
    of ParRi:
      dec nested
      inc n
      if nested == 0:
        break
    else:
      inc n
      if nested == 0:
        break

proc validateConstructedTree(tree: sink Tree): Tree =
  if tree.isEmpty:
    return tree
  var tree = tree
  var n = snapshot(tree)
  var current = n.cursor
  let err = validateConstructedNode(current)
  if err.found:
    result = createErrorTree(err.info, err.msg, err.orig)
  else:
    result = ensureMove(tree)

proc parseNifFragment(text: string): Tree =
  validateConstructedTree(createTree(parseNifBuffer(text)))

proc createTree*[K: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma](
    kind: K; children: varargs[Tree]): Tree =
  ## Produces a new tree node of `kind` containing `children`.
  result = createTree()
  result.withTree kind, NoLineInfo:
    for child in children:
      result.add(child)
  result = validateConstructedTree(result)

proc createTree*[K: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma](
    kind: K; info: LineInfo; children: varargs[Tree]): Tree =
  ## Produces a new tree node of `kind` and line info `info` containing `children`.
  result = createTree()
  result.withTree kind, info:
    for child in children:
      result.add(child)
  result = validateConstructedTree(result)

proc lookupBinding(bindings: openArray[NifBinding]; name: string): int =
  var i = bindings.len - 1
  while i >= 0:
    if bindings[i].name == name:
      return i
    dec i
  result = -1

proc appendParsedText(dest: var TokenBuf; text: string) =
  if text.len > 0:
    dest.add parseNifBuffer(text)

proc appendTree(dest: var TokenBuf; tree: Tree) =
  if not tree.isEmpty:
    dest.add tree.p[].buf

proc parseNifTemplate(spec: string; bindings: openArray[NifBinding]): Tree =
  var buf = createTokenBuf(spec.len + bindings.len * 4)
  var literal = newStringOfCap(spec.len)
  var i = 0
  while i < spec.len:
    if spec[i] != '$':
      literal.add spec[i]
      inc i
    elif i + 1 >= spec.len:
      return createErrorTree(NoLineInfo, "invalid nif substitution syntax")
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
      let binding = lookupBinding(bindings, substr(spec, start, i - 1))
      if binding < 0:
        return createErrorTree(NoLineInfo,
          "missing nif substitution: " & substr(spec, start, i - 1))
      appendTree(buf, bindings[binding].value)
    else:
      return createErrorTree(NoLineInfo, "invalid nif substitution syntax")

  appendParsedText(buf, literal)
  result = validateConstructedTree(createTree(buf))

proc renderNode*(n: Node): string =
  ## Renders the current token or subtree as raw NIF text for debugging.
  ## This omits line info and only covers the subtree rooted at `n`.
  if not hasSubtree(n):
    result = "<bug: empty>"
  else:
    result = toString(n.cursor, false)

proc strLitTree(s: string): Tree =
  var buf = createTokenBuf(1)
  buf.addStrLit(s)
  result = createTree(buf)

proc identTree(s: string): Tree =
  var buf = createTokenBuf(1)
  buf.addIdent(s)
  result = createTree(buf)

proc charLitTree(c: char): Tree =
  var buf = createTokenBuf(1)
  buf.addCharLit(c)
  result = createTree(buf)

proc intLitTree(i: BiggestInt): Tree =
  var buf = createTokenBuf(1)
  buf.addIntLit(i)
  result = createTree(buf)

proc uintLitTree(u: BiggestUInt): Tree =
  var buf = createTokenBuf(1)
  buf.addUIntLit(u)
  result = createTree(buf)

proc floatLitTree(f: BiggestFloat): Tree =
  var buf = createTokenBuf(1)
  buf.addFloatLit(f)
  result = createTree(buf)

proc boolTree(v: bool): Tree =
  var buf = createTokenBuf(2)
  buf.addParLe(if v: TrueX else: FalseX)
  buf.addParRi()
  result = createTree(buf)

proc `~`*(src: Node): Tree =
  ## Copies the subtree rooted at `src` into a fresh `Tree`.
  result = createTree()
  result.addSubtree(src)

template `~`*(src: Tree): Tree =
  ## Returns `src` unchanged.
  src

proc `~`*(src: string): Tree =
  ## Converts `src` into a string literal tree fragment.
  strLitTree(src)

proc `~`*(src: NifIdent): Tree =
  ## Converts `src` into an identifier tree fragment.
  identTree(string(src))

proc `~`*(src: char): Tree =
  ## Converts `src` into a character literal tree fragment.
  charLitTree(src)

proc `~`*(src: int): Tree =
  ## Converts `src` into a signed integer literal tree fragment.
  intLitTree(BiggestInt(src))

proc `~`*(src: int8): Tree =
  ## Converts `src` into a signed integer literal tree fragment.
  intLitTree(BiggestInt(src))

proc `~`*(src: int16): Tree =
  ## Converts `src` into a signed integer literal tree fragment.
  intLitTree(BiggestInt(src))

proc `~`*(src: int32): Tree =
  ## Converts `src` into a signed integer literal tree fragment.
  intLitTree(BiggestInt(src))

proc `~`*(src: int64): Tree =
  ## Converts `src` into a signed integer literal tree fragment.
  intLitTree(BiggestInt(src))

proc `~`*(src: uint): Tree =
  ## Converts `src` into an unsigned integer literal tree fragment.
  uintLitTree(BiggestUInt(src))

proc `~`*(src: uint8): Tree =
  ## Converts `src` into an unsigned integer literal tree fragment.
  uintLitTree(BiggestUInt(src))

proc `~`*(src: uint16): Tree =
  ## Converts `src` into an unsigned integer literal tree fragment.
  uintLitTree(BiggestUInt(src))

proc `~`*(src: uint32): Tree =
  ## Converts `src` into an unsigned integer literal tree fragment.
  uintLitTree(BiggestUInt(src))

proc `~`*(src: uint64): Tree =
  ## Converts `src` into an unsigned integer literal tree fragment.
  uintLitTree(BiggestUInt(src))

proc `~`*(src: float32): Tree =
  ## Converts `src` into a floating-point literal tree fragment.
  floatLitTree(BiggestFloat(src))

proc `~`*(src: float64): Tree =
  ## Converts `src` into a floating-point literal tree fragment.
  floatLitTree(BiggestFloat(src))

proc `~`*(src: bool): Tree =
  ## Converts `src` into a `true` or `false` keyword tree fragment.
  boolTree(src)

proc `%~`*(spec: string; bindings: openArray[NifBinding]): Tree =
  ## Parses `spec` as a NIF fragment after expanding `$name` placeholders with
  ## the corresponding tree fragments from `bindings`.
  ##
  ## Use `$$` for a literal dollar sign.
  parseNifTemplate(spec, bindings)

proc nifFragment*(spec: string): Tree =
  ## Parses `spec` as a literal NIF fragment and returns it as a `Tree`.
  parseNifFragment(spec)
