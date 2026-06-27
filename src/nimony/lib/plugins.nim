## API for Nimony plugins.
##
## Plugins are out-of-process executables that nimony compiles itself
## (separate `bin/nimony c` invocation per plugin) and runs at sem time to
## transform NIF trees attached via the `.plugin` pragma. Authors write
## `import plugins` and use the `Replacer` API (preferred) or the
## lower-level `NifBuilder` / `NifCursor` primitives.
##
## See `doc/plugins.md` for the full guide.

{.feature: "lenientnils".}

import std / [assertions, hashes, syncio, cmdline, formatfloat]
import ".." / ".." / "lib" / [nifcursors, nifstreams, lineinfos, bitabs]

import ".." / [nimony_model, nif_annotations]
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther, NifKind, NoLineInfo
export nif_annotations

type
  NifBuilderObj = object
    rc: int
    buf: TokenBuf

  NifBuilderOwner = ptr NifBuilderObj

  NifBuilder* = object ## Mutable NIF builder used by plugins to assemble output.
                 ## Copying a tree shares the underlying payload until the next
                 ## mutation detaches it.
    p: NifBuilderOwner

  LineInfo* = PackedLineInfo ## Packed source location metadata attached to NIF
                             ## tokens. Use `NoLineInfo` for synthetic output.

  SourcePos* = object ## Decoded source position for plugin-facing APIs.
    line*: int
    col*: int

  NifCursor* = object ## Read handle into a frozen NIF tree.
                 ## A `NifCursor` behaves like a cursor positioned at one token or
                 ## subtree. Copying a `NifCursor` retains the underlying tree
                 ## snapshot automatically.
    cursor: Cursor

  SymId* = object ## Symbol identifier. Use with `addSymUse` / `addSymDef` to
                  ## emit symbol references and definitions in the output.
    raw: nifstreams.SymId

  TagId* = object ## Tag identifier for NIF tree nodes (e.g. `Stmt`, `Expr`).
                  ## Use with `parLeToken` to open a tagged subtree.
    raw: nifstreams.TagId

proc `=destroy`*(x: NifBuilder) =
  if x.p != nil:
    dec x.p.rc
    if x.p.rc == 0:
      `=destroy`(x.p[].buf)
      dealloc(x.p)

proc `=wasMoved`*(x: var NifBuilder) =
  x.p = nil

proc `=copy`*(dest: var NifBuilder; src: NifBuilder) =
  if dest.p != src.p:
    `=destroy`(dest)
    if src.p != nil:
      inc src.p.rc
    dest.p = src.p

proc `=dup`*(x: NifBuilder): NifBuilder {.nodestroy.} =
  result = NifBuilder(p: x.p)
  if result.p != nil:
    inc result.p.rc

proc initNifBuilderObj(buf: sink TokenBuf): NifBuilderOwner =
  result = cast[NifBuilderOwner](alloc0(sizeof(NifBuilderObj)))
  result.rc = 1
  result.buf = ensureMove(buf)

proc copyBuffer(buf: TokenBuf): TokenBuf =
  result = createTokenBuf(max(buf.len, 4))
  result.add buf

proc hasSubtree(n: NifCursor): bool {.inline.} =
  n.cursor.hasMore

proc createTree(buf: sink TokenBuf): NifBuilder =
  result = NifBuilder(p: initNifBuilderObj(buf))

proc prepareMutation(t: var NifBuilder) =
  if t.p == nil:
    t = createTree(createTokenBuf())
  elif t.p.rc > 1:
    let oldP = t.p
    t.p = initNifBuilderObj(copyBuffer(oldP.buf))
    dec oldP.rc

proc isEmpty*(tree: NifBuilder): bool {.inline.} =
  ## Returns true when `tree` does not currently contain any tokens.
  tree.p == nil or tree.p[].buf.len == 0

proc kind*(n: NifCursor): NifKind {.inline.} =
  ## Returns the raw NIF token kind at the current position.
  n.cursor.kind

proc info*(n: NifCursor): LineInfo {.inline.} =
  ## Returns the packed line info stored on the current token.
  n.cursor.info

proc isValid*(info: LineInfo): bool {.inline.} =
  ## Returns true when `info` refers to a real source location.
  lineinfos.isValid(info)

proc filePath*(info: LineInfo): string =
  ## Returns the source path stored in `info`, or `""` when unavailable.
  if lineinfos.isValid(info):
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
  if lineinfos.isValid(info):
    let rawInfo = unpack(pool.man, info)
    result = SourcePos(line: int(rawInfo.line), col: int(rawInfo.col))
  else:
    result = SourcePos(line: 0, col: 0)

func `==`*(a, b: SymId): bool {.inline.} =
  a.raw == b.raw

func hash*(x: SymId): Hash {.inline.} =
  hash(x.raw)

proc `$`*(x: SymId): string {.inline.} =
  pool.syms[x.raw]

proc `$`*(x: TagId): string {.inline.} =
  ## Renders `x` as its textual NIF tag name.
  pool.tags[x.raw]

proc symText*(s: SymId): string {.inline.} =
  ## Returns the symbol text stored in the plugin-facing symbol handle.
  pool.syms[s.raw]

proc tagText*(t: TagId): string {.inline.} =
  ## Returns the textual NIF tag name for `t`.
  pool.tags[t.raw]

proc symId*(n: NifCursor): SymId {.inline.} =
  ## Returns the symbol id of the current token as an opaque handle.
  ## The current token must be a `Symbol` or `SymbolDef`.
  SymId(raw: n.cursor.symId)

proc symText*(n: NifCursor): string {.inline.} =
  ## Returns the symbol text of the current `Symbol` or `SymbolDef` token.
  pool.syms[n.cursor.symId]

proc identText*(n: NifCursor): string {.inline.} =
  ## Returns the identifier text of the current `Ident` token.
  pool.strings[n.cursor.litId]

proc stringValue*(n: NifCursor): string {.inline.} =
  ## Returns the string contents of the current `StringLit` token.
  pool.strings[n.cursor.litId]

proc charLit*(n: NifCursor): char {.inline.} =
  ## Returns the character stored in the current `CharLit` token.
  n.cursor.charLit

proc intValue*(n: NifCursor): BiggestInt {.inline.} =
  ## Returns the integer value stored in the current `IntLit` token.
  pool.integers[n.cursor.intId]

proc uintValue*(n: NifCursor): BiggestUInt {.inline.} =
  ## Returns the unsigned integer value stored in the current `UIntLit` token.
  pool.uintegers[n.cursor.uintId]

proc floatValue*(n: NifCursor): BiggestFloat {.inline.} =
  ## Returns the floating-point value stored in the current `FloatLit` token.
  pool.floats[n.cursor.floatId]

proc stmtKind*(n: NifCursor): NimonyStmt {.inline.} =
  ## Returns the current statement kind, or `NoStmt` when the current token is
  ## not a statement node.
  n.cursor.stmtKind

proc exprKind*(n: NifCursor): NimonyExpr {.inline.} =
  ## Returns the current expression kind, or `NoExpr` when the current token is
  ## not an expression node.
  n.cursor.exprKind

proc typeKind*(n: NifCursor): NimonyType {.inline.} =
  ## Returns the current type kind.
  ## `DotToken` is treated as `VoidT`; non-type nodes return `NoType`.
  n.cursor.typeKind

proc otherKind*(n: NifCursor): NimonyOther {.inline.} =
  ## Returns the current "other/substructure" kind, or `NoSub` for non-matching
  ## nodes.
  n.cursor.substructureKind

proc pragmaKind*(n: NifCursor): NimonyPragma {.inline.} =
  ## Returns the current pragma kind, or `NoPragma` for non-matching nodes.
  n.cursor.pragmaKind

proc createTree*(): NifBuilder =
  ## Creates an empty mutable `NifBuilder`.
  createTree(createTokenBuf())

proc snapshot*(tree: var NifBuilder): NifCursor =
  ## Returns a read-only snapshot positioned at the first top-level token of
  ## `tree`.
  ##
  ## The returned `NifCursor` keeps the underlying data alive automatically via
  ## the Cursor's COW mechanism. The original tree remains writable and
  ## detaches on the next mutation.
  ##
  ## `tree` must not be empty; use `isEmpty` first when that is expected.
  assert not tree.isEmpty, "cannot snapshot empty NifBuilder"
  result = NifCursor(cursor: beginRead(tree.p[].buf))

template withTree*(t: var NifBuilder; kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma; info: LineInfo; body: untyped) =
  ## Appends a tree node of `kind` to `t`, runs `body` to emit its children, and
  ## closes the node afterwards.
  prepareMutation(t)
  # Use the generic parLeToken overload from nimony_model that does the
  # TagId conversion via `cast[TagId](kind)` inside its body — nimony's
  # cast rule allows it there.
  t.p[].buf.add parLeToken(kind, info)
  body
  t.p[].buf.addParRi()

proc tagId*(n: NifCursor): TagId {.inline.} =
  ## Returns the raw tag id of the current token.
  ## The current token must be a `ParLe`.
  TagId(raw: n.cursor.tagId)

proc tagText*(n: NifCursor): string {.inline.} =
  ## Returns the tag text of the current `ParLe` token.
  pool.tags[n.cursor.tagId]

proc tag*(n: NifCursor): TagId {.inline.} =
  ## Returns the raw tag id for the current tree node, or `ErrT` if the
  ## current token is not a `ParLe`.
  TagId(raw: n.cursor.tag)

proc addParLe*(t: var NifBuilder; tag: TagId; info: LineInfo = NoLineInfo) =
  ## Appends an opening tree token with raw tag id `tag` to `t`.
  ## Use `addParLe(tagText, ...)` when constructing nodes from textual tag
  ## names instead of existing ids.
  prepareMutation(t)
  t.p[].buf.addParLe(tag.raw, info)

proc addParLe*(t: var NifBuilder; tag: string; info: LineInfo = NoLineInfo) =
  ## Appends an opening tree token with textual tag `tag` to `t`.
  prepareMutation(t)
  t.p[].buf.addParLe(pool.tags.getOrIncl(tag), info)

proc addParRi*(t: var NifBuilder) =
  ## Appends a closing tree token (`)`) to `t`.
  prepareMutation(t)
  t.p[].buf.addParRi()

proc takeTree*(t: var NifBuilder; n: var NifCursor) =
  ## Copies the current token or subtree from `n` into `t` and advances `n`
  ## past the copied fragment.
  prepareMutation(t)
  t.p[].buf.takeTree(n.cursor)

template copyInto*(t: var NifBuilder; n: var NifCursor; body: untyped) =
  ## Copies the opening tag from `n` into `t`, processes children via `body`
  ## (which must consume them through `take*` / `skip` / nested `copyInto`),
  ## then closes the node in `t` and advances `n` past the matching `)`.
  ##
  ## Delegates to nifprims's `into`, so it remains correct under virtual
  ## ParRi (sealed-jump) and overflow scopes.
  ##
  ## .. code-block:: nim
  ##   o.copyInto(n):
  ##     while n.hasMore:
  ##       transform(n, o)
  assert n.kind == ParLe, "copyInto requires cursor at ParLe"
  prepareMutation(t)
  t.p[].buf.add n.cursor
  nifcursors.into n.cursor:
    body
  t.p[].buf.addParRi()

proc addSubtree*(t: var NifBuilder; n: NifCursor) =
  ## Copies the current token or subtree from `n` into `t` without advancing it.
  prepareMutation(t)
  t.p[].buf.addSubtree(n.cursor)

proc add*(t: var NifBuilder; child: NifBuilder) =
  ## Appends the complete contents of `child` to `t`.
  if not child.isEmpty:
    prepareMutation(t)
    t.p[].buf.add child.p[].buf

proc addDotToken*(t: var NifBuilder) =
  ## Appends a dot placeholder token (`.`) to `t`.
  prepareMutation(t)
  t.p[].buf.addDotToken()

proc addStrLit*(t: var NifBuilder; s: string) =
  ## Appends a string literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addStrLit(s)

proc addIntLit*(t: var NifBuilder; i: BiggestInt) =
  ## Appends a signed integer literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addIntLit(i)

proc addUIntLit*(t: var NifBuilder; i: BiggestUInt) =
  ## Appends an unsigned integer literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addUIntLit(i)

proc addIdent*(t: var NifBuilder; ident: string) =
  ## Appends an identifier atom to `t`.
  prepareMutation(t)
  t.p[].buf.addIdent(ident)

proc addCharLit*(t: var NifBuilder; c: char) =
  ## Appends a character literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addCharLit(c)

proc addFloatLit*(t: var NifBuilder; f: BiggestFloat) =
  ## Appends a floating-point literal atom to `t`.
  prepareMutation(t)
  t.p[].buf.addFloatLit(f)

proc addSymUse*(t: var NifBuilder; s: SymId; info: LineInfo = NoLineInfo) =
  ## Appends a symbol-use atom named by the opaque handle `s` to `t`.
  prepareMutation(t)
  t.p[].buf.addSymUse(s.raw, info)

proc addSymUse*(t: var NifBuilder; s: string; info: LineInfo = NoLineInfo) =
  ## Appends a symbol-use atom named `s` to `t`.
  prepareMutation(t)
  t.p[].buf.addSymUse(pool.syms.getOrIncl(s), info)

proc addErrorMessage(t: var NifBuilder; msg: string; info: LineInfo) =
  prepareMutation(t)
  t.p[].buf.addStrLit(msg, info)

proc buildErrorTree(info: LineInfo; msg: string; orig: NifCursor): NifBuilder =
  result = createTree()
  result.addParLe("err", info)
  result.addSubtree(orig)
  result.addErrorMessage(msg, info)
  result.addParRi()

proc buildErrorTree(info: LineInfo; msg: string): NifBuilder =
  result = createTree()
  result.addParLe("err", info)
  result.addDotToken()
  result.addErrorMessage(msg, info)
  result.addParRi()

proc errorTree*(msg: string): NifBuilder =
  ## Builds a compiler error tree with no original expression attached.
  buildErrorTree(NoLineInfo, msg)

proc errorTree*(msg: string; info: LineInfo): NifBuilder =
  ## Builds a compiler error tree at `info` with no original expression attached.
  buildErrorTree(info, msg)

proc errorTree*(msg: string; at: NifCursor): NifBuilder =
  ## Builds a compiler error tree reported at `at`, attaching `at`.
  if hasSubtree(at):
    buildErrorTree(at.info, msg, at)
  else:
    buildErrorTree(at.info, msg)

proc errorTree*(msg: string; at, orig: NifCursor): NifBuilder =
  ## Builds a compiler error tree reported at `at`, attaching `orig`.
  if hasSubtree(orig):
    buildErrorTree(at.info, msg, orig)
  else:
    buildErrorTree(at.info, msg)

proc parseNifBuffer(text: string): TokenBuf =
  # Internal helper used by `bindSymHelper` to parse a NIF source fragment
  # (e.g. `name.0.suffix` or `(cchoice s1 s2 …)`). Strips the trailing
  # `EofToken` so the resulting tokens concatenate cleanly into a builder.
  result = parseFromBuffer(text, "")
  if result.len > 0 and result[result.len-1].kind == EofToken:
    result.shrink(result.len-1)

type
  BindSymRule* = enum
    ## Selector for `bindSym`'s third argument. Mirrors `lib/std/macros.nim`'s
    ## enum of the same name so the macro and plugin paths share semantics.
    brClosed     ## Default: candidates fixed at the plugin's def-site;
                 ## sem at the call site won't add further overloads.
    brOpen       ## Open: sem at the call site augments the choice with
                 ## call-site visible overloads (full Nim-style mixin).
    brForceOpen  ## Always emit a sym-choice subtree, even with one match,
                 ## so call-site sem still augments the candidate set.

proc bindSymHelper*(t: var NifBuilder; nifText: string) =
  ## Runtime helper: parse `nifText` (a NIF source fragment) and append the
  ## resulting tokens to `t`. Called by `bindSym`'s sem rewrite — not intended
  ## for direct use.
  prepareMutation(t)
  t.p[].buf.add parseNifBuffer(nifText)

proc bindSym*(t: var NifBuilder; name: string;
              rule: BindSymRule = brClosed) {.magic: BindSymName.}
  ## Hygienic symbol reference for nimony-compiled plugins.
  ##
  ## At plugin sem time, `name` is resolved in the plugin module's *definition*
  ## scope and the call is rewritten to `bindSymHelper(t, "<NIF text>")` where
  ## the NIF text is either a single fully-qualified symbol (one match) or a
  ## `(cchoice …)` / `(ochoice …)` sym-choice subtree (multiple matches).
  ## At plugin runtime the helper appends those tokens to `t`.
  ##
  ## Single match emits one Symbol atom; multiple matches emit a `(cchoice …)`
  ## (closed) or `(ochoice …)` (open) subtree. `brForceOpen` always wraps in
  ## an `(ochoice …)`, even with one match, so call-site sem can still
  ## augment the candidate set.
  ##
  ## Because the symbol is resolved against the *plugin module's* scope, the
  ## emitted reference bypasses caller-site lookups and survives shadowing in
  ## the user's module.
  ##
  ## `name` must be a string literal — sem rejects non-literal arguments.

proc addEmptyNode*(t: var NifBuilder; info: LineInfo = NoLineInfo) =
  ## Appends a single empty placeholder node (`.`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty(info)

proc addEmptyNode2*(t: var NifBuilder; info: LineInfo = NoLineInfo) =
  ## Appends two empty placeholder nodes (`. .`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty2(info)

proc addEmptyNode3*(t: var NifBuilder; info: LineInfo = NoLineInfo) =
  ## Appends three empty placeholder nodes (`. . .`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty3(info)

proc addEmptyNode4*(t: var NifBuilder; info: LineInfo = NoLineInfo) =
  ## Appends four empty placeholder nodes (`. . . .`) to `t`.
  prepareMutation(t)
  t.p[].buf.addEmpty3(info)
  t.p[].buf.addEmpty(info)

proc skip*(n: var NifCursor) =
  ## Skips the current token or, if positioned on `ParLe`, the entire subtree.
  skip n.cursor

proc firstChild*(n: NifCursor): NifCursor {.inline.} =
  ## Returns a cursor positioned at the first child of `n`. `n` must be at
  ## a `ParLe`. The returned cursor's bounded `rem` is set to the sub-scope's
  ## body count so it can be used both for read-only `~` / `takeTree`
  ## extraction *and* for bounded iteration (`while c.hasMore: …`). `n`
  ## itself is unchanged.
  ## a `ParLe`.
  result = NifCursor(cursor: firstSon(n.cursor))

# ── Traversal templates ──────────────────────────────────────────────────
# Pure traversal helpers for reading/analyzing a tree without producing output.

template hasMore*(n: NifCursor): bool =
  ## True while there are more children before the closing `)`.
  n.cursor.hasMore

template into*(n: var NifCursor; body: untyped) =
  ## Enters the current node, runs `body` to process the children, then
  ## advances past the (real or virtual) closing `)`. Delegates to nifprims
  ## so the bounded-scope and overflow paths are handled correctly.
  ##
  ## .. code-block:: nim
  ##   n.into:
  ##     while n.hasMore:
  ##       analyze(n)
  ##       skip n
  nifcursors.into n.cursor:
    body

template loopInto*(n: var NifCursor; body: untyped) =
  ## Enters a node, iterates all children, then advances past `)`.
  ## The body must advance `n` on every iteration.
  ##
  ## .. code-block:: nim
  ##   n.loopInto:
  ##     analyze(n)
  ##     skip n
  into n:
    while n.hasMore:
      body

template balancedTokens*(n: var NifCursor; body: untyped) =
  ## Deep-scans all `ParLe` nodes in the subtree rooted at `n`.
  ## Inside `body`, `n` is positioned at each `ParLe` node in turn.
  ## `body` must **not** advance `n` — the template handles traversal.
  ##
  ## .. code-block:: nim
  ##   n.balancedTokens:
  ##     if n.stmtKind == IfS:
  ##       foundIf = true
  nifcursors.balancedTokens n.cursor:
    body

proc eqIdent*(n: NifCursor; name: string): bool =
  ## Returns true when `n` matches `name` exactly.
  case n.kind
  of Ident:
    result = n.identText == name
  of Symbol, SymbolDef:
    result = n.symText == name
  else:
    result = false

# ── Replacer API ──────────────────────────────────────────────────────────
# The Replacer bundles a NifBuilder (output) and NifCursor (input) into a
# single object with balanced operations for tree transformations.

type
  ChildKind* = enum
    ## Category of a NIF child for `keep`/`drop` assertions.
    Any       ## matches any token or subtree
    Expr      ## value expression (ParLe with expr tag, or Symbol/literal)
    Type      ## type expression (ParLe with type tag, or DotToken for void)
    Stmt      ## statement (ParLe with stmt tag)
    Def       ## SymbolDef
    Sym       ## Symbol use
    Dot       ## DotToken (empty placeholder)
    Lit       ## literal (IntLit, UIntLit, FloatLit, StringLit, CharLit)
    Nested    ## nested substructure (params, pragmas, etc.)

  Replacer* = object
    ## Primary abstraction for NIF tree transformations. Bundles an output
    ## builder and an input cursor with balanced operations that consume
    ## input and produce output atomically.
    dest*: NifBuilder   ## Output builder — public for direct emit-only access.
    src: NifCursor      ## Input cursor — use getCursor/setCursor for raw access.

proc isAtom*(t: Replacer): bool {.inline.} =
  ## True when the cursor is at a leaf token (not ParLe). Atoms cannot be
  ## entered with `keepTag`.
  t.src.kind != ParLe

# Delegated cursor accessors:
proc kind*(t: Replacer): NifKind {.inline.} = t.src.kind
proc info*(t: Replacer): LineInfo {.inline.} = t.src.info
proc stmtKind*(t: Replacer): NimonyStmt {.inline.} = t.src.stmtKind
proc exprKind*(t: Replacer): NimonyExpr {.inline.} = t.src.exprKind
proc typeKind*(t: Replacer): NimonyType {.inline.} = t.src.typeKind
proc otherKind*(t: Replacer): NimonyOther {.inline.} = t.src.otherKind
proc pragmaKind*(t: Replacer): NimonyPragma {.inline.} = t.src.pragmaKind
proc symId*(t: Replacer): SymId {.inline.} = t.src.symId
proc symText*(t: Replacer): string {.inline.} = t.src.symText
proc identText*(t: Replacer): string {.inline.} = t.src.identText
proc stringValue*(t: Replacer): string {.inline.} = t.src.stringValue
proc charLit*(t: Replacer): char {.inline.} = t.src.charLit
proc intValue*(t: Replacer): BiggestInt {.inline.} = t.src.intValue
proc uintValue*(t: Replacer): BiggestUInt {.inline.} = t.src.uintValue
proc floatValue*(t: Replacer): BiggestFloat {.inline.} = t.src.floatValue
proc tagId*(t: Replacer): TagId {.inline.} = t.src.tagId
proc tagText*(t: Replacer): string {.inline.} = t.src.tagText
proc tag*(t: Replacer): TagId {.inline.} = t.src.tag
proc eqIdent*(t: Replacer; name: string): bool {.inline.} = t.src.eqIdent(name)

# ── Kind checking ─────────────────────────────────────────────────────────

proc matchesChildKind(n: NifCursor; k: ChildKind): bool =
  case k
  of Any: true
  of Expr:
    case n.kind
    of ParLe: n.exprKind != NoExpr
    of Symbol, Ident, IntLit, UIntLit, FloatLit, StringLit, CharLit: true
    else: false
  of Type:
    n.kind == DotToken or (n.kind == ParLe and n.typeKind != NoType)
  of Stmt:
    n.kind == ParLe and n.stmtKind != NoStmt
  of Def:
    n.kind == SymbolDef
  of Sym:
    n.kind in {Symbol, Ident}
  of Dot:
    n.kind == DotToken
  of Lit:
    n.kind in {IntLit, UIntLit, FloatLit, StringLit, CharLit}
  of Nested:
    n.kind == ParLe and n.exprKind == NoExpr and n.stmtKind == NoStmt and
        n.typeKind == NoType

proc assertChild(n: NifCursor; k: ChildKind) {.inline.} =
  assert matchesChildKind(n, k),
    "expected " & $k & " but got kind=" & $n.kind

proc assertTag(n: NifCursor; expected: NimonyStmt) {.inline.} =
  assert n.stmtKind == expected,
    "expected " & $expected & " but got " & $n.stmtKind

proc assertTag(n: NifCursor; expected: NimonyExpr) {.inline.} =
  assert n.exprKind == expected,
    "expected " & $expected & " but got " & $n.exprKind

proc assertTag(n: NifCursor; expected: NimonyType) {.inline.} =
  assert n.typeKind == expected,
    "expected " & $expected & " but got " & $n.typeKind

proc assertTag(n: NifCursor; expected: NimonyPragma) {.inline.} =
  assert n.pragmaKind == expected,
    "expected " & $expected & " but got " & $n.pragmaKind

proc assertTag(n: NifCursor; expected: NimonyOther) {.inline.} =
  assert n.otherKind == expected,
    "expected " & $expected & " but got " & $n.otherKind

# ── Core operations ───────────────────────────────────────────────────────

proc keep*(t: var Replacer; expected: ChildKind) =
  ## Copy one child verbatim from input to output.
  assertChild(t.src, expected)
  t.dest.takeTree(t.src)

proc keep*(t: var Replacer; expected: NimonyStmt) =
  ## Copy one child, asserting a specific statement tag.
  assertTag(t.src, expected)
  t.dest.takeTree(t.src)

proc keep*(t: var Replacer; expected: NimonyExpr) =
  ## Copy one child, asserting a specific expression tag.
  assertTag(t.src, expected)
  t.dest.takeTree(t.src)

proc keep*(t: var Replacer; expected: NimonyType) =
  ## Copy one child, asserting a specific type tag.
  assertTag(t.src, expected)
  t.dest.takeTree(t.src)

proc keep*(t: var Replacer; expected: NimonyPragma) =
  ## Copy one child, asserting a specific pragma tag.
  assertTag(t.src, expected)
  t.dest.takeTree(t.src)

proc keep*(t: var Replacer; expected: NimonyOther) =
  ## Copy one child, asserting a specific substructure tag.
  assertTag(t.src, expected)
  t.dest.takeTree(t.src)

proc drop*(t: var Replacer; expected: ChildKind) =
  ## Skip one child from input without emitting.
  assertChild(t.src, expected)
  t.src.skip()

proc drop*(t: var Replacer; expected: NimonyStmt) =
  ## Skip one child, asserting a specific statement tag.
  assertTag(t.src, expected)
  t.src.skip()

proc drop*(t: var Replacer; expected: NimonyExpr) =
  ## Skip one child, asserting a specific expression tag.
  assertTag(t.src, expected)
  t.src.skip()

proc drop*(t: var Replacer; expected: NimonyType) =
  ## Skip one child, asserting a specific type tag.
  assertTag(t.src, expected)
  t.src.skip()

proc drop*(t: var Replacer; expected: NimonyPragma) =
  ## Skip one child, asserting a specific pragma tag.
  assertTag(t.src, expected)
  t.src.skip()

proc drop*(t: var Replacer; expected: NimonyOther) =
  ## Skip one child, asserting a specific substructure tag.
  assertTag(t.src, expected)
  t.src.skip()

proc replace*(t: var Replacer; expected: ChildKind; replacement: NifCursor) =
  ## Skip one child from input, emit replacement cursor tree instead.
  assertChild(t.src, expected)
  t.src.skip()
  t.dest.addSubtree(replacement)

proc replace*(t: var Replacer; expected: ChildKind; replacement: NifBuilder) =
  ## Skip one child from input, emit replacement builder tree instead.
  assertChild(t.src, expected)
  t.src.skip()
  t.dest.add(replacement)

proc replace*(t: var Replacer; expected: NimonyStmt; replacement: NifCursor) =
  ## Skip one child, asserting a specific statement tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addSubtree(replacement)

proc replace*(t: var Replacer; expected: NimonyStmt; replacement: NifBuilder) =
  ## Skip one child, asserting a specific statement tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.add(replacement)

proc replace*(t: var Replacer; expected: NimonyExpr; replacement: NifCursor) =
  ## Skip one child, asserting a specific expression tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addSubtree(replacement)

proc replace*(t: var Replacer; expected: NimonyExpr; replacement: NifBuilder) =
  ## Skip one child, asserting a specific expression tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.add(replacement)

proc replace*(t: var Replacer; expected: NimonyType; replacement: NifCursor) =
  ## Skip one child, asserting a specific type tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addSubtree(replacement)

proc replace*(t: var Replacer; expected: NimonyType; replacement: NifBuilder) =
  ## Skip one child, asserting a specific type tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.add(replacement)

template keepTag*(t: var Replacer; body: untyped) =
  ## Copy the opening tag from input to output, run `body` for children,
  ## close the node and advance past the input's closing `)`.
  assert t.src.kind == ParLe, "keepTag requires cursor at ParLe"
  t.dest.copyInto(t.src):
    body

template loopKeepTag*(t: var Replacer; body: untyped) =
  ## Copy the opening tag, iterate all children, close the node.
  ## The body must advance the cursor on every iteration.
  keepTag t:
    while t.src.hasMore:
      body

template replaceHead*(t: var Replacer;
                      tag: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma;
                      info: LineInfo; body: untyped) =
  ## Like `keepTag` but emits a new tag instead of copying the input's tag.
  ## Enters the input tree via `into`, runs `body`
  ## (which must consume the children), then closes both input and output
  ## scopes.
  assert t.src.kind == ParLe, "replaceHead requires cursor at ParLe"
  t.dest.withTree(tag, info):
    nifcursors.into t.src.cursor:
      body

# ── Cursor access for analysis ────────────────────────────────────────────

proc getCursor*(t: Replacer): NifCursor {.inline.} =
  ## Returns a copy of the current cursor position for independent analysis
  ## or for later use as a `replace` argument.
  t.src

proc setCursor*(t: var Replacer; c: NifCursor) {.inline.} =
  ## Restores the cursor to a previously saved position.
  t.src = c

template peek*(t: var Replacer; body: untyped) =
  ## Saves the cursor, runs `body` (which may advance for read-ahead
  ## analysis), then restores the cursor.
  ## Inside `body`, use `getCursor(t)` to obtain a `NifCursor` for
  ## analysis procs. Prefer extracting analysis into a separate proc
  ## that takes `NifCursor` rather than manipulating the Replacer directly.
  let savedCursor = t.src
  body
  t.src = savedCursor

# ── Entry points ──────────────────────────────────────────────────────────

proc loadReplacer*(inputFile = paramStr(1)): Replacer =
  ## Loads the input NIF file and returns a `Replacer` ready for
  ## transformation. The cursor is positioned at the root of the input tree.
  var inp = nifstreams.open(inputFile)
  try:
    var tree = createTree(fromStream(inp))
    result = Replacer(dest: createTree(), src: snapshot(tree))
  finally:
    close(inp)

proc saveReplacer*(t: Replacer; filename = paramStr(2)) =
  ## Writes the Replacer's output to `filename` (default: `paramStr(2)`).
  try:
    if t.dest.p == nil:
      writeFile filename, ""
    else:
      writeFile filename, toString(t.dest.p[].buf)
  except:
    quit "FAILURE: cannot write " & filename

# ── Existing API (retained for backwards compatibility) ───────────────────

proc loadPluginInput*(filename = paramStr(1)): NifCursor =
  ## Loads a NIF file and returns a root `NifCursor` for reading it.
  ##
  ## For type plugins, use `loadTypeDefinitions()` to read the second input
  ## file (`paramStr(3)`) that carries the triggering type definitions.
  var inp = nifstreams.open(filename)
  try:
    var tree = createTree(fromStream(inp))
    result = snapshot(tree)
    # tree is destroyed here, but NifCursor's cursor keeps data alive via COW
  finally:
    close(inp)

proc loadTypeDefinitions*(): NifCursor =
  ## Loads the type-definitions input for a type plugin (`paramStr(3)`).
  ##
  ## Type plugins receive two input files: the full module AST via
  ## `loadPluginInput()` and the triggering type definitions via this proc.
  ## The result has the shape `(stmts <type-sym1> <type-sym2> ...)`.
  loadPluginInput(paramStr(3))

proc pluginName*(n: NifCursor): string =
  ## Returns the name of the symbol that triggered this plugin run.
  ##
  ## Template-plugin input has the shape `(stmts <name> <args...>)`.
  ## For-loop-plugin input has the shape
  ## `(forcall <name> (callargs ...) (unpackflat ...) <body>)`.
  ## In both cases the compiler prepends the invoked symbol's name as a bare
  ## identifier so that a single shared plugin can dispatch.
  ##
  ## Returns `""` when the input does not carry a leading identifier
  ## (e.g. for module or type plugins).
  var n = n
  if n.stmtKind == StmtsS or n.otherKind == ForcallU:
    n = firstChild(n)
  result = if n.kind == Ident: n.identText else: ""

proc pluginCallArgs*(n: NifCursor): NifCursor =
  ## Returns a cursor positioned at the first call-site argument of a
  ## template-plugin or for-loop-plugin input. Use `result.hasMore` to iterate.
  ##
  ## For template input `(stmts <name> <arg1> ...)` the result points at
  ## `<arg1>`. For for-loop input `(forcall <name> (callargs <arg1> ...) ...)`
  ## the result points at `<arg1>` inside the `(callargs ...)` group.
  ## When there are no arguments it is positioned at `)`.
  result = n
  if result.otherKind == ForcallU:
    result = firstChild(result) # name
    skip result                  # → (callargs ...
    result = firstChild(result)  # → first arg
  elif result.stmtKind == StmtsS:
    result = firstChild(result)
    skip result # advance past the name to the first real argument

proc forLoopVars*(n: NifCursor): NifCursor =
  ## Returns a cursor at the loop variables of a for-loop plugin input.
  ##
  ## For-loop plugin input has the shape
  ## `(forcall <iter-name> (callargs ...) (unpackflat ...) <body>)`.
  ## The result points directly at the `(unpackflat …)` or `(unpacktup …)`
  ## subtree.
  result = n
  if result.otherKind == ForcallU:
    result = firstChild(result) # name
    skip result                  # (callargs ...)
    skip result                  # → (unpackflat ...) or (unpacktup ...)
  # result is now at the loop vars node, or at ')' if none

proc forLoopBody*(n: NifCursor): NifCursor =
  ## Returns a cursor at the loop body of a for-loop plugin input.
  ##
  ## For-loop plugin input has the shape
  ## `(forcall <iter-name> (callargs ...) (unpackflat ...) <body>)`.
  ## This proc scans past the iter name, call args, and loop vars to reach
  ## the body subtree.
  result = forLoopVars(n)
  if result.kind != ParRi:
    skip result # skip loop vars
    # result is now at the body (or at ')' if there is none)

proc renderTree*(tree: NifBuilder): string =
  ## Renders the complete contents of `tree` as raw NIF text for debugging.
  ## Unlike `saveTree`, this omits line info and may contain multiple
  ## top-level fragments when the tree is still under construction.
  if tree.p == nil:
    result = ""
  else:
    result = toString(tree.p[].buf, false)

proc saveTree*(tree: NifBuilder; filename: string) =
  ## Writes the complete contents of a mutable `NifBuilder` to `filename`.
  ## This preserves line info because it is intended for `.nif` output.
  try:
    if tree.p == nil:
      writeFile filename, ""
    else:
      writeFile filename, toString(tree.p[].buf)
  except:
    quit "FAILURE: cannot write " & filename

proc saveTree*(tree: NifBuilder) =
  ## Writes the complete contents of a mutable `NifBuilder` to `paramStr(2)`.
  ## This preserves line info because it is intended for `.nif` output.
  ##
  ## **Re-semantacking contract**: template and for-loop plugin output is
  ## re-semantacked by the compiler — identifiers are resolved, types are
  ## checked, and calls are instantiated just like normal source. This means
  ## the output can use raw identifiers (e.g. `addIdent "echo"`). Module and
  ## type plugin output is NOT re-semantacked — it must already be fully
  ## semanticked NIF (resolved symbols, typed expressions) because it
  ## directly replaces the module body.
  saveTree(tree, paramStr(2))

proc createTree*[K: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma](
    kind: K; children: openArray[NifBuilder]): NifBuilder =
  ## Produces a new tree node of `kind` containing `children`.
  result = createTree()
  prepareMutation(result)
  result.p[].buf.add parLeToken(kind, NoLineInfo)
  for child in children:
    result.add(child)
  result.p[].buf.addParRi()

proc createTree*[K: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma](
    kind: K; info: LineInfo; children: openArray[NifBuilder]): NifBuilder =
  ## Produces a new tree node of `kind` and line info `info` containing `children`.
  result = createTree()
  prepareMutation(result)
  result.p[].buf.add parLeToken(kind, info)
  for child in children:
    result.add(child)
  result.p[].buf.addParRi()

proc renderNode*(n: NifCursor): string =
  ## Renders the current token or subtree as raw NIF text for debugging.
  ## This omits line info and only covers the subtree rooted at `n`.
  if not hasSubtree(n):
    result = "<bug: empty>"
  else:
    result = toString(n.cursor, false)
