## API for Nimony plugins.
##
## Plugins are out-of-process executables that nimony compiles itself
## (separate `bin/nimony c` invocation per plugin) and runs at sem time to
## transform NIF trees attached via the `.plugin` pragma. Authors write
## `import plugins` and use the `Replacer` API (preferred) or the
## lower-level `NifBuilder` / `NifCursor` primitives.
##
## See `doc/plugins.md` for the full guide.

{.feature: "untyped".}

import std / [assertions, hashes, syncio, cmdline]
import ".." / ".." / "lib" / nifcore except symId, `$`, addSymUse, addSymDef
import ".." / ".." / "lib" / nifcoreparse except symId, `$`, addSymUse, addSymDef
import ".." / ".." / "lib" / [bitabs, nifbuilder, symparser]
import ".." / ".." / "models" / [tags, nimony_tags]
import ".." / nif_annotations
export NimonyType, NimonyExpr, NimonyStmt, NimonyPragma, NimonyOther
export NifKind, SymId, TagId
export DotToken, CharLit, StrLit, IntLit, UIntLit, FloatLit
export Symbol, SymbolDef, Ident, TagLit
export isValid, hash
export skip, hasMore, into, loopInto
export addSubtree, addDotToken, addStrLit, addIntLit, addUIntLit
export addIdent, addCharLit, addFloatLit
export charLit
export nif_annotations

type
  NifBuilder* = nifcore.TokenBuf ## Move-only builder for generated NIF output.
  NifCursor* = nifcore.Cursor ## Reference-counted, bounded NIF read cursor.
  LineInfo* = nifcore.NifLineInfo ## Source location attached to a NIF value.

  SourcePos* = object ## Decoded source position.
    line*: int ## One-based source line, or zero when unavailable.
    col*: int ## One-based source column, or zero when unavailable.

const NoLineInfo* = NoNifLineInfo ## Source location for synthetic output.

proc createPluginTags(): TagPool =
  result = newTagPool()
  for tag in low(TagEnum)..high(TagEnum):
    if tag != InvalidTagId:
      let id = result.registerTag(TagData[tag][0])
      assert uint32(id) == uint32(tag),
        "Nimony tag pool is not ordinal-aligned for " & TagData[tag][0]

let
  pluginPool = newPool()
  pluginTags = createPluginTags()

var
  unusedNameBase = ""
  nextUnusedName = 0

proc appendInfo(buf: var NifBuilder; info: LineInfo) {.inline.} =
  buf.appendLineInfo(info)

proc isEmpty*(tree: NifBuilder): bool {.inline.} =
  ## Returns whether `tree` contains no NIF values.
  tree.len == 0

proc info*(n: NifCursor): LineInfo {.inline.} =
  ## Returns the current value's source location, or `NoLineInfo` at exhaustion.
  n.rawLineInfo

proc filePath*(info: LineInfo): string {.inline.} =
  ## Returns the source path stored in `info`, or `""` when unavailable.
  if info.file.isValid: pluginPool.filenames[info.file] else: ""

proc lineCol*(info: LineInfo): SourcePos {.inline.} =
  ## Decodes the one-based line and column stored in `info`.
  SourcePos(line: int(info.line), col: int(info.col))

proc symText*(n: NifCursor): string {.inline.} =
  ## Returns the current `Symbol` or `SymbolDef` name.
  n.symName

proc symText*(s: SymId): string {.inline.} =
  ## Resolves a plugin-pool symbol handle to its name.
  pluginPool.syms[s]

proc identText*(n: NifCursor): string {.inline.} =
  ## Returns the current `Ident` text.
  n.strVal

proc stringValue*(n: NifCursor): string {.inline.} =
  ## Returns the current `StrLit` contents.
  n.strVal

proc intValue*(n: NifCursor): BiggestInt {.inline.} =
  ## Returns the current signed integer literal.
  BiggestInt(n.intVal)

proc uintValue*(n: NifCursor): BiggestUInt {.inline.} =
  ## Returns the current unsigned integer literal.
  BiggestUInt(n.uintVal)

proc floatValue*(n: NifCursor): BiggestFloat {.inline.} =
  ## Returns the current floating-point literal.
  BiggestFloat(n.floatVal)

proc tagId*(n: NifCursor): TagId {.inline.} =
  ## Returns the current `TagLit`'s raw tag handle.
  n.cursorTagId

proc tagText*(n: NifCursor): string {.inline.} =
  ## Returns the current `TagLit`'s textual tag name.
  n.tags.tags[n.cursorTagId]

proc tagText*(tag: TagId): string {.inline.} =
  ## Resolves a plugin-pool tag handle to its textual name.
  pluginTags.tags[tag]

proc tag*(n: NifCursor): TagId {.inline.} =
  ## Returns the error tag for atoms and exhausted cursors.
  if n.hasMore and n.kind == TagLit:
    n.cursorTagId
  else:
    cast[TagId](ErrTagId)

proc rawTag(n: NifCursor): TagEnum {.inline.} =
  if n.kind != TagLit or n.tags != pluginTags:
    return InvalidTagId
  let id = uint32(n.cursorTagId)
  if id <= uint32(high(TagEnum)): cast[TagEnum](id) else: InvalidTagId

proc stmtKind*(n: NifCursor): NimonyStmt {.inline.} =
  ## Returns the current statement tag, or `NoStmt`.
  let t = rawTag(n)
  if rawTagIsNimonyStmt(t): cast[NimonyStmt](t) else: NoStmt

proc exprKind*(n: NifCursor): NimonyExpr {.inline.} =
  ## Returns the current expression tag, or `NoExpr`.
  let t = rawTag(n)
  if rawTagIsNimonyExpr(t): cast[NimonyExpr](t) else: NoExpr

proc typeKind*(n: NifCursor): NimonyType {.inline.} =
  ## Returns the current type tag; `DotToken` is treated as `VoidT`.
  if n.kind == DotToken: VoidT
  else:
    let t = rawTag(n)
    if rawTagIsNimonyType(t): cast[NimonyType](t) else: NoType

proc otherKind*(n: NifCursor): NimonyOther {.inline.} =
  ## Returns the current substructure tag, or `NoSub`.
  let t = rawTag(n)
  if rawTagIsNimonyOther(t): cast[NimonyOther](t) else: NoSub

proc pragmaKind*(n: NifCursor): NimonyPragma {.inline.} =
  ## Returns the current pragma tag or identifier kind, or `NoPragma`.
  var t = InvalidTagId
  if n.kind == TagLit:
    t = rawTag(n)
  elif n.kind == Ident:
    let id = uint32(pluginTags.tags.getKeyId(n.identText))
    if id <= uint32(high(TagEnum)):
      t = cast[TagEnum](id)
  if rawTagIsNimonyPragma(t): cast[NimonyPragma](t) else: NoPragma

proc createTree*(): NifBuilder =
  ## Creates an empty plugin builder using the shared plugin pools.
  nifcore.createTokenBuf(sharedPool = pluginPool, sharedTags = pluginTags)

proc snapshot*(tree: var NifBuilder): NifCursor =
  ## Returns a stable read cursor at the first value in non-empty `tree`.
  assert not tree.isEmpty, "cannot snapshot empty NifBuilder"
  tree.beginRead()

template withTree*(
    t: var NifBuilder;
    kind: NimonyType|NimonyExpr|NimonyStmt|NimonyOther|NimonyPragma;
    info: LineInfo;
    body: untyped) =
  ## Emits a tagged tree around the values produced by `body`.
  t.openTag(cast[TagId](kind))
  appendInfo(t, info)
  body
  t.closeTag()

proc openTree*(t: var NifBuilder; tag: TagId;
               info: LineInfo = NoLineInfo) =
  ## Opens a tree using an existing plugin-pool tag handle.
  t.openTag(tag)
  appendInfo(t, info)

proc openTree*(t: var NifBuilder; tag: string;
               info: LineInfo = NoLineInfo) =
  ## Opens a tree using a textual tag, interning it when necessary.
  t.openTag(pluginTags.tags.getOrIncl(tag))
  appendInfo(t, info)

proc closeTree*(t: var NifBuilder) =
  ## Seals the most recently opened tree.
  t.closeTag()

proc takeTree*(t: var NifBuilder; n: var NifCursor) =
  ## Copies the current value or subtree into `t` and advances `n`.
  t.addSubtree(n)
  n.skip()

proc enterPluginScope(n: var NifCursor): nifcore.CursorScope =
  nifcore.enterScope(n)

proc leavePluginScope(n: var NifCursor; scope: nifcore.CursorScope) =
  nifcore.leaveScope(n, scope)

template copyInto*(t: var NifBuilder; n: var NifCursor; body: untyped) =
  ## Copies `n`'s tag, transforms its children with `body`, and advances `n`.
  assert n.kind == TagLit, "copyInto requires cursor at TagLit"
  t.openTree(n.tagId, n.info)
  let inputScope = enterPluginScope(n)
  body
  leavePluginScope(n, inputScope)
  t.closeTree()

proc addTree*(t: var NifBuilder; child: NifBuilder) =
  ## Appends every complete top-level value from `child`.
  t.addBufferSamePool(child)

proc addSymUse*(t: var NifBuilder; s: SymId;
                info: LineInfo = NoLineInfo) =
  ## Appends a symbol use from a plugin-pool symbol handle.
  nifcore.addSymUse(t, s)
  appendInfo(t, info)

proc addSymUse*(t: var NifBuilder; s: string;
                info: LineInfo) =
  ## Appends a symbol use from its textual name with source information.
  nifcore.addSymUse(t, s)
  appendInfo(t, info)

proc addSymDef*(t: var NifBuilder; s: SymId;
                info: LineInfo = NoLineInfo) =
  ## Appends a symbol definition from a plugin-pool symbol handle.
  nifcore.addSymDef(t, s)
  appendInfo(t, info)

proc addSymDef*(t: var NifBuilder; s: string;
                info: LineInfo = NoLineInfo) =
  ## Appends a symbol definition from its textual name.
  nifcore.addSymDef(t, s)
  appendInfo(t, info)

proc genSym*(): SymId =
  ## Returns a fresh local symbol supplied by the input's `.unusedname` hint.
  ##
  ## Pass the result to the regular `addSymDef` and `addSymUse` operations.
  assert unusedNameBase.len > 0,
    "genSym requires plugin input with an .unusedname directive"
  result = pluginPool.syms.getOrIncl(
    unusedNameBase & "." & $nextUnusedName)
  inc nextUnusedName

proc addErrorMessage(t: var NifBuilder; msg: string; info: LineInfo) =
  t.addStrLit(msg)
  appendInfo(t, info)

proc buildErrorTree(info: LineInfo; msg: string; orig: NifCursor): NifBuilder =
  result = createTree()
  result.openTree("err", info)
  result.addSubtree(orig)
  result.addErrorMessage(msg, info)
  result.closeTree()

proc buildErrorTree(info: LineInfo; msg: string): NifBuilder =
  result = createTree()
  result.openTree("err", info)
  result.addDotToken()
  result.addErrorMessage(msg, info)
  result.closeTree()

proc errorTree*(msg: string): NifBuilder =
  ## Builds a compiler error tree with no original expression attached.
  buildErrorTree(NoLineInfo, msg)

proc errorTree*(msg: string; info: LineInfo): NifBuilder =
  ## Builds a compiler error tree at `info` with no original expression attached.
  buildErrorTree(info, msg)

proc errorTree*(msg: string; at: NifCursor): NifBuilder =
  ## Builds a compiler error tree reported at `at`, attaching `at`.
  if at.hasMore:
    buildErrorTree(at.info, msg, at)
  else:
    buildErrorTree(at.info, msg)

proc errorTree*(msg: string; at, orig: NifCursor): NifBuilder =
  ## Builds a compiler error tree reported at `at`, attaching `orig`.
  if orig.hasMore:
    buildErrorTree(at.info, msg, orig)
  else:
    buildErrorTree(at.info, msg)

proc parseNifBuffer(text: string): nifcore.TokenBuf =
  # Internal helper used by `bindSymHelper` to parse a NIF source fragment
  # (e.g. `name.0.suffix` or `(cchoice s1 s2 …)`).
  result = parseFromBuffer(text, "", sharedPool = pluginPool,
                           sharedTags = pluginTags)

type
  BindSymRule* = enum ## Controls call-site lookup for `bindSym`.
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
  t.addTree(parseNifBuffer(nifText))

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
  t.addDotToken()
  appendInfo(t, info)

proc addEmptyNode2*(t: var NifBuilder; info: LineInfo = NoLineInfo) =
  ## Appends two empty placeholder nodes (`. .`) to `t`.
  t.addEmptyNode(info)
  t.addEmptyNode(info)

proc addEmptyNode3*(t: var NifBuilder; info: LineInfo = NoLineInfo) =
  ## Appends three empty placeholder nodes (`. . .`) to `t`.
  t.addEmptyNode2(info)
  t.addEmptyNode(info)

proc addEmptyNode4*(t: var NifBuilder; info: LineInfo = NoLineInfo) =
  ## Appends four empty placeholder nodes (`. . . .`) to `t`.
  t.addEmptyNode2(info)
  t.addEmptyNode2(info)

proc firstChild*(n: NifCursor): NifCursor {.inline.} =
  ## Returns a cursor positioned at the first child of `n`. `n` must be at
  ## a `TagLit`. The returned cursor's bounded `rem` is set to the sub-scope's
  ## body count so it can be used both for read-only `~` / `takeTree`
  ## extraction *and* for bounded iteration (`while c.hasMore: …`). `n`
  ## itself is unchanged.
  result = nifcore.childCursor(n)

# ── Traversal templates ──────────────────────────────────────────────────
# Pure traversal helpers for reading/analyzing a tree without producing output.

template linearScan*(n: var NifCursor; body: untyped) =
  ## Deep-scans all `TagLit` nodes in the subtree rooted at `n`.
  ## Inside `body`, `n` is positioned at each `TagLit` node in turn.
  ## `body` must **not** advance `n` — the template handles traversal.
  ##
  ## .. code-block:: nim
  ##   n.linearScan:
  ##     if n.stmtKind == IfS:
  ##       foundIf = true
  let outerCursor = n
  if n.hasMore and n.kind == TagLit:
    var scan = firstChild(n)
    while scan.hasMore:
      if scan.kind == TagLit:
        n = scan
        body
      scan.inc()
    n = outerCursor
    n.skip()

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
  ChildKind* = enum ## Category asserted by `keep`, `drop`, and `replace`.
    Any       ## matches any token or subtree
    Expr      ## value expression (TagLit with expr tag, or Symbol/literal)
    Type      ## type expression (TagLit with type tag, or DotToken for void)
    Stmt      ## statement (TagLit with stmt tag)
    Def       ## SymbolDef
    Sym       ## Symbol use
    Dot       ## DotToken (empty placeholder)
    Lit       ## literal (IntLit, UIntLit, FloatLit, StrLit, CharLit)
    Nested    ## nested substructure (params, pragmas, etc.)

  Replacer* = object ## Input/output state for balanced NIF transformations.
    dest*: NifBuilder   ## Output builder — public for direct emit-only access.
    src: NifCursor      ## Input cursor — use getCursor/setCursor for raw access.

proc isAtom*(t: Replacer): bool {.inline.} =
  ## True when the cursor is at a leaf token (not TagLit). Atoms cannot be
  ## entered with `keepTag`.
  t.src.kind != TagLit

# Delegated cursor accessors:
proc kind*(t: Replacer): NifKind {.inline.} =
  ## Returns the source cursor's raw NIF kind.
  t.src.kind

proc info*(t: Replacer): LineInfo {.inline.} =
  ## Returns the source cursor's location.
  t.src.info

proc stmtKind*(t: Replacer): NimonyStmt {.inline.} =
  ## Returns the source cursor's statement kind.
  t.src.stmtKind

proc exprKind*(t: Replacer): NimonyExpr {.inline.} =
  ## Returns the source cursor's expression kind.
  t.src.exprKind

proc typeKind*(t: Replacer): NimonyType {.inline.} =
  ## Returns the source cursor's type kind.
  t.src.typeKind

proc otherKind*(t: Replacer): NimonyOther {.inline.} =
  ## Returns the source cursor's substructure kind.
  t.src.otherKind

proc pragmaKind*(t: Replacer): NimonyPragma {.inline.} =
  ## Returns the source cursor's pragma kind.
  t.src.pragmaKind

proc symId*(t: Replacer): SymId {.inline.} =
  ## Returns the source cursor's symbol handle.
  t.src.symId

proc symText*(t: Replacer): string {.inline.} =
  ## Returns the source cursor's symbol text.
  t.src.symText

proc identText*(t: Replacer): string {.inline.} =
  ## Returns the source cursor's identifier text.
  t.src.identText

proc stringValue*(t: Replacer): string {.inline.} =
  ## Returns the source cursor's string literal.
  t.src.stringValue

proc charLit*(t: Replacer): char {.inline.} =
  ## Returns the source cursor's character literal.
  t.src.charLit

proc intValue*(t: Replacer): BiggestInt {.inline.} =
  ## Returns the source cursor's signed integer literal.
  t.src.intValue

proc uintValue*(t: Replacer): BiggestUInt {.inline.} =
  ## Returns the source cursor's unsigned integer literal.
  t.src.uintValue

proc floatValue*(t: Replacer): BiggestFloat {.inline.} =
  ## Returns the source cursor's floating-point literal.
  t.src.floatValue

proc tagId*(t: Replacer): TagId {.inline.} =
  ## Returns the source cursor's raw tag handle.
  t.src.tagId

proc tagText*(t: Replacer): string {.inline.} =
  ## Returns the source cursor's textual tag.
  t.src.tagText

proc tag*(t: Replacer): TagId {.inline.} =
  ## Returns the source cursor's tag or the error tag for an atom.
  t.src.tag

proc eqIdent*(t: Replacer; name: string): bool {.inline.} =
  ## Returns whether the source cursor names `name`.
  t.src.eqIdent(name)

# ── Kind checking ─────────────────────────────────────────────────────────

proc matchesChildKind(n: NifCursor; k: ChildKind): bool =
  case k
  of Any: true
  of Expr:
    case n.kind
    of TagLit: n.exprKind != NoExpr
    of Symbol, Ident, IntLit, UIntLit, FloatLit, StrLit, CharLit: true
    else: false
  of Type:
    n.kind == DotToken or (n.kind == TagLit and n.typeKind != NoType)
  of Stmt:
    n.kind == TagLit and n.stmtKind != NoStmt
  of Def:
    n.kind == SymbolDef
  of Sym:
    n.kind in {Symbol, Ident}
  of Dot:
    n.kind == DotToken
  of Lit:
    n.kind in {IntLit, UIntLit, FloatLit, StrLit, CharLit}
  of Nested:
    n.kind == TagLit and n.exprKind == NoExpr and n.stmtKind == NoStmt and
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

proc replace*(t: var Replacer; expected: ChildKind;
              replacement: NifBuilder) =
  ## Skip one child from input, emit replacement builder tree instead.
  assertChild(t.src, expected)
  t.src.skip()
  t.dest.addTree(replacement)

proc replace*(t: var Replacer; expected: NimonyStmt; replacement: NifCursor) =
  ## Skip one child, asserting a specific statement tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addSubtree(replacement)

proc replace*(t: var Replacer; expected: NimonyStmt;
              replacement: NifBuilder) =
  ## Skip one child, asserting a specific statement tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addTree(replacement)

proc replace*(t: var Replacer; expected: NimonyExpr; replacement: NifCursor) =
  ## Skip one child, asserting a specific expression tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addSubtree(replacement)

proc replace*(t: var Replacer; expected: NimonyExpr;
              replacement: NifBuilder) =
  ## Skip one child, asserting a specific expression tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addTree(replacement)

proc replace*(t: var Replacer; expected: NimonyType; replacement: NifCursor) =
  ## Skip one child, asserting a specific type tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addSubtree(replacement)

proc replace*(t: var Replacer; expected: NimonyType;
              replacement: NifBuilder) =
  ## Skip one child, asserting a specific type tag, emit replacement.
  assertTag(t.src, expected)
  t.src.skip()
  t.dest.addTree(replacement)

template keepTag*(t: var Replacer; body: untyped) =
  ## Copy the opening tag from input to output, run `body` for children,
  ## close the output node and advance past the input subtree.
  assert t.src.kind == TagLit, "keepTag requires cursor at TagLit"
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
  assert t.src.kind == TagLit, "replaceHead requires cursor at TagLit"
  t.dest.withTree(tag, info):
    let inputScope = enterPluginScope(t.src)
    body
    leavePluginScope(t.src, inputScope)

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

proc loadPluginTree(filename: string): NifBuilder =
  var hint = ""
  result = parseFromFile(
    filename, hint, sharedPool = pluginPool, sharedTags = pluginTags,
    denseLineInfo = true)
  if hint.len > 0:
    var hintBase = ""
    var hintNumber = 0
    assert splitLocalSymName(hint, hintBase, hintNumber),
      "plugin .unusedname must be a local symbol"
    if unusedNameBase.len == 0:
      unusedNameBase = hintBase
      nextUnusedName = hintNumber
    else:
      assert unusedNameBase == hintBase,
        "plugin inputs must use the same .unusedname base"
      if nextUnusedName < hintNumber:
        nextUnusedName = hintNumber

proc writePluginTree(tree: var NifBuilder; filename: string) =
  var output = nifbuilder.open(filename)
  if unusedNameBase.len > 0:
    output.addRaw "(.unusedname "
    output.addSymbol unusedNameBase & "." & $nextUnusedName
    output.addRaw ")\n"
  tree.appendTo(output)
  try:
    output.close()
  except:
    quit "FAILURE: cannot write " & filename

proc loadReplacer*(inputFile = paramStr(1)): Replacer =
  ## Loads the input NIF file and returns a `Replacer` ready for
  ## transformation. The cursor is positioned at the root of the input tree.
  var tree = loadPluginTree(inputFile)
  result = Replacer(dest: createTree(), src: snapshot(tree))

proc saveReplacer*(t: var Replacer; filename = paramStr(2)) =
  ## Writes the Replacer's output to `filename` (default: `paramStr(2)`).
  writePluginTree(t.dest, filename)

# ── Plugin input helpers ───────────────────────────────────────────────────

proc loadPluginInput*(filename = paramStr(1)): NifCursor =
  ## Loads a NIF file and returns a root `NifCursor` for reading it.
  ##
  ## For type plugins, use `loadTypeDefinitions()` to read the second input
  ## file (`paramStr(3)`) that carries the triggering type definitions.
  var tree = loadPluginTree(filename)
  result = snapshot(tree)

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

proc callArgs*(n: NifCursor): NifCursor =
  ## Returns a cursor at the first call-site argument of a template-plugin
  ## input `(stmts <name> <arg1> <arg2> ...)`. Skips the `(stmts` wrapper
  ## and the leading name. Use `result.hasMore` to iterate.
  ##
  ## For for-loop plugins, use `forLoopCallArgs` instead.
  result = n
  if result.stmtKind == StmtsS:
    result = firstChild(result)
    skip result # advance past the name to the first real argument

proc forLoopCallArgs*(n: NifCursor): NifCursor =
  ## Returns a cursor at the `(callargs ...)` node of a for-loop plugin input.
  ## Enter it with `.into:` to iterate the call arguments:
  ##
  ## .. code-block:: nim
  ##   var c = forLoopCallArgs(n)
  ##   c.into:
  ##     while c.hasMore:
  ##       process(c)
  ##       skip c
  result = n
  if result.otherKind == ForcallU:
    result = firstChild(result) # name
    skip result                  # → (callargs ...

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
  # result is now at the loop vars node, or exhausted if none

proc forLoopBody*(n: NifCursor): NifCursor =
  ## Returns a cursor at the loop body of a for-loop plugin input.
  ##
  ## For-loop plugin input has the shape
  ## `(forcall <iter-name> (callargs ...) (unpackflat ...) <body>)`.
  ## This proc scans past the iter name, call args, and loop vars to reach
  ## the body subtree.
  result = n
  if result.otherKind == ForcallU:
    result = firstChild(result)
    skip result # iterator name
    skip result # call arguments
    skip result # loop variables
  # result is now at the body (or exhausted if there is none)

proc renderTree*(tree: var NifBuilder): string =
  ## Renders the complete contents of `tree` as raw NIF text for debugging.
  ## Unlike `saveTree`, this omits line info and may contain multiple
  ## top-level fragments when the tree is still under construction.
  result = nifcoreparse.toString(tree, includeLineInfo = false)

proc saveTree*(tree: sink NifBuilder; filename: string) =
  ## Writes the complete contents of a mutable `NifBuilder` to `filename`.
  ## This preserves line info because it is intended for `.nif` output.
  var output = ensureMove(tree)
  writePluginTree(output, filename)

proc saveTree*(tree: sink NifBuilder) =
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
  var output = ensureMove(tree)
  writePluginTree(output, paramStr(2))

proc renderNode*(n: NifCursor): string =
  ## Renders the current token or subtree as raw NIF text for debugging.
  ## This omits line info and only covers the subtree rooted at `n`.
  if not n.hasMore:
    result = "<bug: empty>"
  else:
    result = nifcoreparse.toString(n, includeLineInfo = false)
