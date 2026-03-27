## API for plugins.

import std / [syncio, assertions]
from std / os import paramStr
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, bitabs]

import ".." / [nimony_model]
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther
export NifKind

export NoLineInfo

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

  Node* = object ## Read handle into a frozen NIF tree.
                 ## A `Node` behaves like a cursor positioned at one token or
                 ## subtree. Copying a `Node` retains the underlying tree
                 ## snapshot automatically.
    owner: Tree
    cursor: Cursor

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

proc `=destroy`*(n: Node) =
  if n.owner.p != nil:
    endRead(n.owner.p[].buf)
  `=destroy`(n.owner)

proc `=wasMoved`*(n: var Node) =
  `=wasMoved`(n.owner)
  n.cursor = default(Cursor)

template sameReader(a, b: Node): bool =
  a.owner.p == b.owner.p and toUniqueId(a.cursor) == toUniqueId(b.cursor)

proc `=copy`*(dest: var Node; src: Node) =
  if not sameReader(dest, src):
    `=destroy`(dest)
    if src.owner.p != nil:
      dest.owner = src.owner
      dest.cursor = shareRead(dest.owner.p[].buf, src.cursor)

proc `=dup`*(src: Node): Node =
  result = default(Node)
  if src.owner.p != nil:
    result.owner = src.owner
    result.cursor = shareRead(result.owner.p[].buf, src.cursor)

proc initPayload(buf: sink TokenBuf): ptr TreePayload =
  result = cast[ptr TreePayload](alloc0(sizeof(TreePayload)))
  result.counter = 0
  result.buf = move(buf)

proc copyBuffer(buf: TokenBuf): TokenBuf =
  result = createTokenBuf(max(buf.len, 4))
  result.add buf

proc prepareMutation(t: var Tree)

proc prepareMutation(n: var Node) =
  assert n.owner.p != nil, "cannot mutate default Node"
  assert hasCurrentToken(n.cursor), "cannot mutate exhausted Node"
  let pos = cursorToPosition(n.owner.p[].buf, n.cursor)
  endRead(n.owner.p[].buf)
  prepareMutation(n.owner)
  n.cursor = cursorAt(n.owner.p[].buf, pos)

proc createTree(buf: sink TokenBuf): Tree =
  result = Tree(p: initPayload(buf))

proc prepareMutation(t: var Tree) =
  if t.p == nil:
    t = createTree(createTokenBuf())
  elif t.p.counter > 0:
    let oldP = t.p
    t.p = initPayload(copyBuffer(oldP.buf))
    dec oldP.counter

proc kind*(n: Node): NifKind {.inline.} =
  ## Returns the raw NIF token kind at the current position.
  n.cursor.kind

proc info*(n: Node): PackedLineInfo {.inline.} =
  ## Returns the packed line info stored on the current token.
  n.cursor.info

proc filePath*(info: LineInfo): string =
  ## Returns the source path stored in `info`, or `""` when unavailable.
  let rawInfo = unpack(pool.man, info)
  if info.isValid and rawInfo.file.isValid:
    result = pool.files[rawInfo.file]
  else:
    result = ""

proc symId*(n: Node): SymId {.inline.} =
  ## Returns the symbol id of the current token.
  ## The current token must be a `Symbol` or `SymbolDef`.
  n.cursor.symId

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

proc snapshot*(tree: Tree): Node =
  ## Returns a read-only snapshot of `tree` as a root `Node`.
  ##
  ## The returned `Node` keeps the underlying tree alive automatically. The
  ## original tree remains writable and detaches on the next mutation.
  result = Node(owner: tree, cursor: default(Cursor))
  result.cursor = beginRead(result.owner.p[].buf)

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
  ## Returns the raw tag id for the current tree node, or `ErrT` if the current
  ## token is not a `ParLe`.
  n.cursor.tag

proc addParLe*(t: var Tree; tag: TagId; info: LineInfo = NoLineInfo) =
  ## Appends a raw opening tree token with tag `tag` to `t`.
  prepareMutation(t)
  t.p[].buf.addParLe(tag, info)

proc addParRi*(t: var Tree) =
  ## Appends a closing tree token (`)`) to `t`.
  prepareMutation(t)
  t.p[].buf.addParRi()

proc takeTree*(t: var Tree; n: var Node) =
  ## Copies the current token or subtree from `n` into `t` and advances `n`
  ## past the copied fragment.
  prepareMutation(t)
  t.p[].buf.takeTree(n.cursor)

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
  ## Appends a symbol-use atom to `t`.
  prepareMutation(t)
  t.p[].buf.addSymUse(s, info)

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

proc add*(t: var Tree; child: Node): Tree {.discardable.} =
  ## Appends `child` to `t` without advancing the caller's `Node`.
  var copy = child
  t.takeTree(copy)
  result = t

proc add*(t: var Tree; children: varargs[Node]): Tree {.discardable.} =
  ## Appends every child in `children` to `t`.
  for child in children:
    discard t.add(child)
  result = t

proc inc*(n: var Node) =
  ## Advances `n` by one token.
  inc n.cursor

proc skip*(n: var Node) =
  ## Skips the current token or, if positioned on `ParLe`, the entire subtree.
  skip n.cursor

proc setInfo*(n: var Node; info: PackedLineInfo) {.inline.} =
  ## Rewrites the line info of the current token in place.
  prepareMutation(n)
  n.cursor.setInfo(info)

proc len*(n: Node): int =
  ## Returns the number of immediate children of the current node.
  result = 0
  if n.owner.p != nil and hasCurrentToken(n.cursor) and n.kind == ParLe:
    var it = n.cursor
    inc it
    while it.kind != ParRi:
      skip it
      inc result

proc infoToStr(info: PackedLineInfo): string =
  let rawInfo = unpack(pool.man, info)
  if not info.isValid or not rawInfo.file.isValid:
    result = "???"
  else:
    result = info.filePath
    result.add "("
    result.addInt rawInfo.line
    result.add ", "
    result.addInt rawInfo.col + 1
    result.add ")"

proc currentInfo(n: Node): PackedLineInfo =
  if n.owner.p != nil and hasCurrentToken(n.cursor):
    result = n.info
  else:
    result = NoLineInfo

proc writeDiagnostic(level, msg: string; info: PackedLineInfo) =
  stdout.writeLine infoToStr(info) & " " & level & msg

proc warning*(msg: string; n: Node = default(Node)) =
  ## Emits a warning tied to `n`'s line information.
  writeDiagnostic("Warning: ", msg, currentInfo(n))

proc error*(msg: string; n: Node = default(Node)) =
  ## Emits an error tied to `n`'s line information and aborts the plugin.
  writeDiagnostic("Error: ", msg, currentInfo(n))
  quit 1

proc expectKind*(n: Node; k: NifKind) =
  ## Checks that `n` has raw token kind `k`.
  if n.kind != k:
    error("Expected a node of kind " & $k & ", got " & $n.kind, n)

proc expectKind*(n: Node; kinds: set[NifKind]) =
  ## Checks that `n` has a raw token kind in `kinds`.
  if n.kind notin kinds:
    error("Expected a node of one of the kinds " & $kinds & ", got " & $n.kind, n)

proc expectKind*(n: Node; k: NimonyType) =
  ## Checks that `n` is a type node of kind `k`.
  if n.typeKind != k:
    error("Expected a node of kind " & $k & ", got " & $n.typeKind, n)

proc expectKind*(n: Node; k: NimonyExpr) =
  ## Checks that `n` is an expression node of kind `k`.
  if n.exprKind != k:
    error("Expected a node of kind " & $k & ", got " & $n.exprKind, n)

proc expectKind*(n: Node; k: NimonyStmt) =
  ## Checks that `n` is a statement node of kind `k`.
  if n.stmtKind != k:
    error("Expected a node of kind " & $k & ", got " & $n.stmtKind, n)

proc expectKind*(n: Node; k: NimonyOther) =
  ## Checks that `n` is an "other/substructure" node of kind `k`.
  if n.otherKind != k:
    error("Expected a node of kind " & $k & ", got " & $n.otherKind, n)

proc expectKind*(n: Node; k: NimonyPragma) =
  ## Checks that `n` is a pragma node of kind `k`.
  if n.pragmaKind != k:
    error("Expected a node of kind " & $k & ", got " & $n.pragmaKind, n)

proc expectMinLen*(n: Node; min: int) =
  ## Checks that `n` has at least `min` children.
  let actual = n.len
  if actual < min:
    error("Expected a node with at least " & $min & " children, got " & $actual, n)

proc expectLen*(n: Node; len: int) =
  ## Checks that `n` has exactly `len` children.
  let actual = n.len
  if actual != len:
    error("Expected a node with " & $len & " children, got " & $actual, n)

proc expectLen*(n: Node; min, max: int) =
  ## Checks that `n` has a number of children in the range `min..max`.
  let actual = n.len
  if actual < min or actual > max:
    error("Expected a node with " & $min & ".." & $max & " children, got " & $actual, n)

proc eqIdent*(n: Node; name: string): bool =
  ## Returns true when `n` matches `name` exactly.
  case n.kind
  of Ident:
    result = n.identText == name
  of Symbol, SymbolDef:
    result = n.symText == name
  else:
    result = false

proc expectIdent*(n: Node; name: string) =
  ## Checks that `eqIdent(n, name)` holds.
  if not eqIdent(n, name):
    error("Expected identifier to be `" & name & "` here", n)

proc loadNode*(filename = paramStr(1)): Node =
  ## Loads a NIF file and returns a root `Node` for reading it.
  var inp = nifstreams.open(filename)
  try:
    var tree = createTree(fromStream(inp))
    result = snapshot(tree)
  finally:
    close(inp)

proc saveTree*(tree: Tree) =
  ## Writes the complete contents of a mutable `Tree` to `paramStr(2)`.
  ## This preserves line info because it is intended for `.nif` output.
  writeFile paramStr(2), toString(tree.p[].buf)

proc saveTree*(tree: Tree; filename: string) =
  ## Writes the complete contents of a mutable `Tree` to `filename`.
  ## This preserves line info because it is intended for `.nif` output.
  writeFile filename, toString(tree.p[].buf)

type
  NifIdent* = distinct string ## Marker type used with `~` to request an
                              ## identifier node instead of a string literal
                              ## node.
  NifBinding* = tuple[name: string, value: Node]

proc ident*(s: string): NifIdent =
  ## Marks `s` so that `~s` produces an identifier node rather than a string
  ## literal node.
  NifIdent(s)

proc isNifIdentStart(c: char): bool {.inline.} =
  c in {'a'..'z', 'A'..'Z', '_'}

proc isNifIdentChar(c: char): bool {.inline.} =
  c in {'a'..'z', 'A'..'Z', '0'..'9', '_'}

proc createNode(buf: sink TokenBuf): Node =
  var tree = createTree(buf)
  result = snapshot(tree)

proc parseNifBuffer(text: string): TokenBuf =
  result = parseFromBuffer(text, "")
  if result.len > 0 and result[result.len-1].kind == EofToken:
    result.shrink(result.len-1)

type
  ChildShape = enum
    AnyChild, ExprChild, StmtChild, TypeChild, OtherChild, SymUseChild,
    SymDefChild, IntLitChild, StringLitChild, CharLitChild

template isSupportedTag(n: Node): bool =
  let raw = tagEnum(n.cursor)
  rawTagIsNimonyExpr(raw) or
  rawTagIsNimonyStmt(raw) or
  rawTagIsNimonyType(raw) or
  rawTagIsNimonyOther(raw) or
  rawTagIsNimonyPragma(raw) or
  rawTagIsNimonySym(raw) or
  rawTagIsControlFlowKind(raw) or
  rawTagIsCallConv(raw)

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

proc matchesShape(n: Node; shape: ChildShape): bool =
  case shape
  of AnyChild:
    result = true
  of ExprChild:
    case n.kind
    of ParLe:
      result = rawTagIsNimonyExpr(tagEnum(n.cursor))
    of Symbol, Ident, IntLit, UIntLit, FloatLit, StringLit, CharLit:
      result = true
    else:
      result = false
  of StmtChild:
    result = n.kind == ParLe and
      (rawTagIsNimonyStmt(tagEnum(n.cursor)) or rawTagIsControlFlowKind(tagEnum(n.cursor)))
  of TypeChild:
    result = n.kind == DotToken or
      (n.kind == ParLe and rawTagIsNimonyType(tagEnum(n.cursor)))
  of OtherChild:
    result = n.kind == ParLe and rawTagIsNimonyOther(tagEnum(n.cursor))
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

proc expectChildren(n: Node; shapes: openArray[ChildShape]; allowMore = false) =
  let actual = n.len
  if actual < shapes.len:
    error("missing child " & $(actual + 1) & " for '" & n.tagText &
      "': expected " & describeShape(shapes[actual]), n)
  if not allowMore and actual > shapes.len:
    error("'" & n.tagText & "' takes " & $shapes.len &
      " children, got " & $actual, n)

  var child = n
  inc child
  for i in 0 ..< shapes.len:
    if not matchesShape(child, shapes[i]):
      error("invalid child " & $(i + 1) & " for '" & n.tagText &
        "': expected " & describeShape(shapes[i]), child)
    skip child

proc validateConstructedNode(n: Node)

proc validateShape(n: Node) =
  let expr = n.exprKind
  if expr != NoExpr:
    case expr
    of AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX,
        EqX, NeqX, LeX, LtX, AshrX, EqsetX, LesetX, LtsetX, InsetX:
      expectChildren(n, [TypeChild, ExprChild, ExprChild])
    of BitnotX, CastX, ConvX, HconvX, DconvX, CardX:
      expectChildren(n, [TypeChild, ExprChild])
    of AtX, PatX, AndX, OrX, XorX, CurlyatX, CopyX, SinkhX, TraceX, IsX:
      expectChildren(n, [ExprChild, ExprChild])
    of NotX, NegX, DerefX, AddrX, ParX, EmoveX, DestroyX, DupX, WasmovedX,
        CompilesX, DeclaredX, DefinedX, AstToStrX, HighX, LowX, EnumtostrX,
        InternalTypeNameX, FailedX:
      expectChildren(n, [ExprChild])
    of SizeofX, AlignofX, NewrefX, DefaultobjX, DefaulttupX:
      expectChildren(n, [TypeChild])
    of OffsetofX, InstanceofX, EnvpX:
      expectChildren(n, [TypeChild, ExprChild])
    of TypeofX, FieldsX, FieldpairsX:
      expectChildren(n, [TypeChild, ExprChild], allowMore = true)
    of CallX, CmdX, HcallX, ProccallX, CallstrlitX:
      expectChildren(n, [ExprChild], allowMore = true)
    else:
      discard
  else:
    let stmt = n.stmtKind
    if stmt != NoStmt:
      case stmt
      of VarS, LetS, ConstS, GvarS, TvarS, GletS, TletS, CursorS, ProcS,
          FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS,
          ResultS:
        expectChildren(n, [SymDefChild], allowMore = true)
      of BlockS:
        expectChildren(n, [AnyChild, StmtChild])
      else:
        discard
    else:
      let typ = n.typeKind
      if typ != NoType:
        case typ
        of ArrayT, RangetypeT:
          expectChildren(n, [TypeChild, ExprChild, ExprChild])
        of PtrT, RefT, MutT, OutT, LentT, SinkT, DistinctT, TypedescT,
            UarrayT, SetT:
          expectChildren(n, [TypeChild])
        of ObjectT:
          expectChildren(n, [TypeChild], allowMore = true)
        else:
          discard
      else:
        let other = n.otherKind
        if other != NoSub:
          case other
          of RangeU:
            expectChildren(n, [ExprChild, ExprChild])
          of ParamU, TypevarU, FldU, EfldU:
            expectChildren(n, [SymDefChild], allowMore = true)
          else:
            discard
        else:
          let pragma = n.pragmaKind
          if pragma != NoPragma:
            case pragma
            of PragmaP:
              expectChildren(n, [SymDefChild], allowMore = true)
            else:
              discard

proc validateConstructedNode(n: Node) =
  if n.kind == ParLe:
    if not isSupportedTag(n):
      error("unsupported NIF tag '" & n.tagText & "'", n)
    validateShape(n)
    var child = n
    inc child
    while child.kind != ParRi:
      validateConstructedNode(child)
      skip child

proc parseNifFragment(text: string): Node =
  result = createNode(parseNifBuffer(text))
  validateConstructedNode(result)

proc add*[K: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma](
    kind: K; children: varargs[Node]): Node =
  ## Produces a new tree node of `kind` containing `children`.
  var tree = createTree()
  tree.withTree kind, NoLineInfo:
    discard tree.add(children)
  result = snapshot(tree)

proc add*[K: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma](
    kind: K; info: LineInfo; children: varargs[Node]): Node =
  ## Produces a new tree node of `kind` and line info `info` containing `children`.
  var tree = createTree()
  tree.withTree kind, info:
    discard tree.add(children)
  result = snapshot(tree)

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
  validateConstructedNode(result)

proc renderTree*(tree: Tree): string =
  ## Renders the complete contents of `tree` as raw NIF text for debugging.
  ## Unlike `saveTree`, this omits line info and may contain multiple
  ## top-level fragments when the tree is still under construction.
  if tree.p == nil:
    result = ""
  else:
    result = toString(tree.p[].buf, false)

proc renderNode*(n: Node): string =
  ## Renders the current token or subtree as raw NIF text for debugging.
  ## This omits line info and only covers the subtree rooted at `n`.
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
  ## Use `$$` for a literal dollar sign.
  parseNifTemplate(spec, bindings)

proc nif*(spec: string): Node =
  ## Parses `spec` as a literal NIF fragment and returns it as an owned `Node`.
  parseNifFragment(spec)
