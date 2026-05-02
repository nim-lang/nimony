#       Dagon
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Dagon is the documentation backend for Nimony, parallel to NIFC for code
## generation. It reads a post-sem `.sc.nif` and emits one rendered file per
## module — either HTML or a NIF tree mirroring the same structure.

import std / [parseopt, os, strutils, syncio, assertions, algorithm, tables]

include ".." / lib / nifprelude
import ".." / lib / [symparser, htmlbuilder, docpaths]
import ".." / nimony / [nimony_model, decls]
import markdown

const
  Version = "0.1.0"
  Usage = "Dagon - NIF documentation backend. Version " & Version & """

  (c) 2026 Andreas Rumpf
Usage:
  dagon [options] [command] [arguments]
Command:
  m|module input.sc.nif output.html out.docidx     render docs for one module
  link out.html docidx...                          combine docidx files
                                                   into a global index page

Options:
  --format:html|nif     output format for `module` (default: html)
  --projectRoot:DIR     source files under DIR map to mirrored relpaths
  --stdlibRoot:DIR      source files under DIR map to mirrored relpaths
                        (paths outside both roots bucket under _external/)
  --version             show the version
  --help                show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

const
  RoutineKinds = {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS}
  LocalDeclKinds = {ConstS, GvarS, TvarS, GletS, TletS}
  AllDeclKinds = RoutineKinds + LocalDeclKinds + {TypeS}

type
  RenderCtx = object
    currentModule: string             ## own modid (e.g. "sysvq0asl")
    currentRelpath: string            ## own page path under outdir (e.g. "std/system.html")
    importMap: Table[string, string]  ## modid → relpath, from own (import (kv …))
    projectRoot, stdlibRoot: string

proc kindLabel(k: NimonyStmt): string =
  case k
  of ProcS: "proc"
  of FuncS: "func"
  of IteratorS: "iterator"
  of ConverterS: "converter"
  of MethodS: "method"
  of MacroS: "macro"
  of TemplateS: "template"
  of TypeS: "type"
  of ConstS: "const"
  of GvarS, TvarS: "var"
  of GletS, TletS: "let"
  else: "?"

proc basename(symName: string): string =
  var work = symName
  extractBasename(work)
  result = work

const UnreservedUrlChar = {'A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'}
proc urlEscape(s: string): string =
  ## Percent-encode anything outside RFC 3986 unreserved chars. Used for both
  ## URL fragments and the `id` attribute target so they always agree byte-
  ## for-byte (a SymId like ``dollar`.Color.0.modhash`` carries a backtick).
  result = newStringOfCap(s.len)
  for c in s:
    if c in UnreservedUrlChar:
      result.add c
    else:
      const Hex = "0123456789ABCDEF"
      result.add '%'
      result.add Hex[(ord(c) shr 4) and 0xF]
      result.add Hex[ord(c) and 0xF]

proc symHref(ctx: RenderCtx; sym: SymId): string =
  ## URL pointing at `sym`'s anchor. Self-module → bare `#…`. Cross-module →
  ## `<relative-path-to-target>#…`, computed as the relative path from the
  ## *current* page to the target page under the shared outdir, so the link
  ## works without any web-server config.
  let s = pool.syms[sym]
  let m = extractModule(s)
  let anchor = urlEscape(s)
  if m.len == 0 or m == ctx.currentModule:
    return "#" & anchor
  let targetRel = ctx.importMap.getOrDefault(m, "")
  if targetRel.len == 0:
    # Symbol from a module we don't have a path for (shouldn't happen if sem
    # only references imported modules). Fall back to the bare modid scheme.
    return m & ".html#" & anchor
  let myDir = parentDir(ctx.currentRelpath)
  let rel =
    if myDir.len == 0: targetRel
    else: relativePath(targetRel, myDir, '/')
  rel & "#" & anchor

proc sourcePath(n: Cursor): string =
  if n.kind != ParLe: return ""
  let raw = unpack(pool.man, n.info)
  if raw.file.isValid: result = pool.files[raw.file]
  else: result = ""

proc docOf(info: PackedLineInfo): string =
  let raw = unpack(pool.man, info)
  if raw.comment != 0'u32: pool.strings[StrId(raw.comment)]
  else: ""

proc emitKw(b: var HtmlBuilder; kw: string) =
  emitClass(b, "kw"):
    emitText(b, kw)

proc emitOp(b: var HtmlBuilder; op: string) =
  emitClass(b, "op"):
    emitText(b, op)

proc emitName(b: var HtmlBuilder; name: string) =
  emitClass(b, "name"):
    emitText(b, name)

proc emitTyperef(b: var HtmlBuilder; ctx: RenderCtx; sym: SymId) =
  ## Cross-module-aware type reference. HTML: `<a class="typeref" href="…">name</a>`.
  ## NIF: `(typeref "href" "name")` — the href becomes the link target if a
  ## downstream tool wants to re-resolve.
  let name = basename(pool.syms[sym])
  let href = symHref(ctx, sym)
  case b.format
  of hofHtml:
    emitTagAttr(b, "a", [("class", "typeref"), ("href", href)]):
      emitText(b, name)
  of hofNif:
    emitTag(b, "typeref"):
      emitText(b, href)
      emitText(b, name)

proc emitPrim(b: var HtmlBuilder; name: string) =
  ## A primitive type rendered as a typeref-class span without a link target.
  emitClass(b, "prim"):
    emitText(b, name)

proc primitiveTypeName(tag: string; bits: int): string =
  ## Map NIF primitive type tags to Nim-ish names. `(i 64)` → `int`, `(i 8)` → `int8`.
  let base = case tag
    of "i": "int"
    of "u": "uint"
    of "f": "float"
    of "c": "char"
    else: tag
  case tag
  of "i", "u":
    if bits == sizeof(int) * 8 or bits <= 0: base
    else: base & $bits
  of "f":
    if bits == 64 or bits <= 0: base
    else: base & $bits
  of "c":
    base
  else:
    base

proc renderTypeExpr(b: var HtmlBuilder; ctx: RenderCtx; n: var Cursor) =
  ## Pretty-print a type expression. Symbols → cross-linked typeref;
  ## primitive `(i 64)` etc. → `int`/`float64`/`char` as `prim` spans;
  ## compound trees → `Tag[children…]`.
  case n.kind
  of Symbol:
    emitTyperef(b, ctx,n.symId)
    inc n
  of Ident:
    emitPrim(b, pool.strings[n.litId])
    inc n
  of IntLit:
    emitText(b, $pool.integers[n.intId])
    inc n
  of DotToken:
    inc n
  of ParLe:
    let tag = pool.tags[n.tagId]
    inc n
    if tag in ["i", "u", "f", "c"]:
      let bits =
        if n.kind == IntLit: int(pool.integers[n.intId])
        else: 0
      emitPrim(b, primitiveTypeName(tag, bits))
      while n.kind != ParRi: skip n
      inc n
      return
    if n.kind == ParRi:
      emitPrim(b, tag)
      inc n
      return
    emitPrim(b, tag)
    emitOp(b, "[")
    var first = true
    while n.kind != ParRi:
      if not first: emitOp(b, ", ")
      first = false
      renderTypeExpr(b, ctx,n)
    emitOp(b, "]")
    inc n
  else:
    skip n

proc emitDocBlock(b: var HtmlBuilder; doc: string) =
  ## Render a per-decl doc comment as Markdown. Wrapped in `<div class="doc">`
  ## so the per-li layout can style it as a block under the signature.
  if doc.len == 0: return
  case b.format
  of hofHtml:
    emitOpenAttr(b, "div", [("class", "doc")])
    renderMarkdown(b, doc)
    emitClose(b, "div")
  of hofNif:
    emitTag(b, "doc"):
      renderMarkdown(b, doc)

type
  DocIdxEntry = object
    kind: string       ## "proc", "type", "const", …
    basename: string   ## user-facing name
    symid: string      ## raw SymId — anchor target before url-escape
    summary: string    ## short blurb extracted from the markdown doc

proc summarise(doc: string): string =
  ## First non-empty line of `doc`, stripped of common markdown markers, used
  ## as the global-index blurb. Cheap and good enough — the link target leads
  ## to the full doc.
  if doc.len == 0: return ""
  for raw in doc.splitLines():
    var line = raw.strip()
    if line.len == 0: continue
    var i = 0
    while i < line.len and line[i] == '#': inc i
    if i > 0 and i < line.len and line[i] == ' ': line = line[i+1 .. ^1]
    var out2 = newStringOfCap(line.len)
    var j = 0
    while j < line.len:
      let c = line[j]
      if c == '*' or c == '_' or c == '`':
        inc j
      elif c == '[':
        # Skip the markdown link decoration; keep the link text.
        let close = line.find(']', j + 1)
        if close >= 0:
          out2.add line[j+1 ..< close]
          j = close + 1
          if j < line.len and line[j] == '(':
            let pclose = line.find(')', j + 1)
            if pclose >= 0:
              j = pclose + 1
        else:
          inc j
      else:
        out2.add c
        inc j
    return out2.strip()
  return ""

proc declAnchor(sym: SymId): string =
  ## Anchor id for a decl. Same value byte-for-byte as the URL fragment
  ## produced by `symHref`, so internal links land cleanly.
  urlEscape(pool.syms[sym])

template emitDeclItem(b: var HtmlBuilder; sym: SymId; body: untyped) =
  ## `<li id="…"><code>…</code></li>` (or NIF `(li "anchor" (code …))`).
  let anchorId = declAnchor(sym)
  case b.format
  of hofHtml:
    emitTagAttr(b, "li", [("id", anchorId)]):
      body
  of hofNif:
    emitTag(b, "li"):
      emitText(b, anchorId)
      body

proc recordEntry(idx: var seq[DocIdxEntry]; sk: NimonyStmt; sym: SymId; doc: string) =
  idx.add DocIdxEntry(
    kind: kindLabel(sk),
    basename: basename(pool.syms[sym]),
    symid: pool.syms[sym],
    summary: summarise(doc))

proc renderRoutine(b: var HtmlBuilder; idx: var seq[DocIdxEntry];
                   ctx: RenderCtx; sk: NimonyStmt;
                   n: var Cursor; doc: string) =
  let r = asRoutine(n)
  recordEntry(idx, sk, r.name.symId, doc)
  emitDeclItem(b, r.name.symId):
    emitTag(b, "code"):
      emitKw(b, kindLabel(sk))
      emitText(b, " ")
      emitName(b, basename(pool.syms[r.name.symId]))
      emitOp(b, "(")
      var p = r.params
      if p.kind == ParLe and p.substructureKind == ParamsU:
        inc p
        var first = true
        while p.kind != ParRi:
          if not first: emitOp(b, "; ")
          first = false
          let local = asLocal(p)
          emitText(b, basename(pool.syms[local.name.symId]))
          emitOp(b, ": ")
          var typ = local.typ
          renderTypeExpr(b, ctx,typ)
          skip p
      emitOp(b, ")")
      var rt = r.retType
      if rt.kind notin {DotToken, EofToken}:
        emitOp(b, ": ")
        renderTypeExpr(b, ctx,rt)
    emitDocBlock(b, doc)
  skip n

proc renderTypeDecl(b: var HtmlBuilder; idx: var seq[DocIdxEntry];
                    ctx: RenderCtx; n: var Cursor; doc: string) =
  let t = asTypeDecl(n)
  recordEntry(idx, TypeS, t.name.symId, doc)
  emitDeclItem(b, t.name.symId):
    emitTag(b, "code"):
      emitKw(b, "type")
      emitText(b, " ")
      emitName(b, basename(pool.syms[t.name.symId]))
      if t.body.kind == ParLe:
        emitOp(b, " = ")
        emitKw(b, pool.tags[t.body.tagId])
    emitDocBlock(b, doc)
  skip n

proc renderLocal(b: var HtmlBuilder; idx: var seq[DocIdxEntry];
                 ctx: RenderCtx; sk: NimonyStmt;
                 n: var Cursor; doc: string) =
  let l = asLocal(n)
  recordEntry(idx, sk, l.name.symId, doc)
  emitDeclItem(b, l.name.symId):
    emitTag(b, "code"):
      emitKw(b, kindLabel(sk))
      emitText(b, " ")
      emitName(b, basename(pool.syms[l.name.symId]))
      var typ = l.typ
      if typ.kind notin {DotToken, EofToken}:
        emitOp(b, ": ")
        renderTypeExpr(b, ctx,typ)
    emitDocBlock(b, doc)
  skip n

proc parseImports(n: var Cursor; ctx: var RenderCtx) =
  ## After the outer `(stmts …)` ParLe is entered, consume all leading
  ## `(import (kv suffix "path") …)` blocks and populate `ctx.importMap`.
  ## Stops at the first non-import child so the regular decl walk picks up
  ## from where we left off.
  while n.kind == ParLe and n.stmtKind == ImportS:
    inc n  # enter (import …)
    while n.kind != ParRi:
      if n.kind == ParLe and n.substructureKind == KvU:
        inc n  # enter (kv …)
        var suffix = ""
        var path = ""
        if n.kind == Ident:
          suffix = pool.strings[n.litId]
          inc n
        if n.kind == StringLit:
          path = pool.strings[n.litId]
          inc n
        if suffix.len > 0 and path.len > 0:
          ctx.importMap[suffix] =
            deriveRelpath(path, ctx.projectRoot, ctx.stdlibRoot)
        while n.kind != ParRi: skip n
        inc n  # closing ')' of (kv …)
      else:
        skip n
    inc n  # closing ')' of (import …)

proc renderDecls(b: var HtmlBuilder; idx: var seq[DocIdxEntry];
                 ctx: RenderCtx; n: var Cursor) =
  ## Walk the children of the outer `(stmts …)`. Caller must have entered the
  ## stmts already (so `parseImports` and this can share the same cursor).
  while n.kind != ParRi:
    if n.kind == ParLe:
      let sk = n.stmtKind
      if sk in AllDeclKinds:
        let info = n.info
        let doc = docOf(info)
        if sk in RoutineKinds:
          renderRoutine(b, idx, ctx, sk, n, doc)
        elif sk == TypeS:
          renderTypeDecl(b, idx, ctx, n, doc)
        else:
          renderLocal(b, idx, ctx, sk, n, doc)
      else:
        skip n
    else:
      inc n

const DefaultCss = """
body { font-family: -apple-system, system-ui, sans-serif; max-width: 60rem; margin: 2rem auto; padding: 0 1rem; color: #1a1a1a; line-height: 1.45; }
h1 { border-bottom: 1px solid #ccc; padding-bottom: .25rem; }
h2, h3, h4 { margin-top: 1.2rem; }
a { color: #2e6e44; }
ul.decls { list-style: none; padding: 0; }
ul.decls > li { padding: .5rem 0; border-bottom: 1px dashed #eee; }
code { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: .92rem; }
.kw { color: #0a55a4; font-weight: 600; }
.name { color: #1a1a1a; font-weight: 600; }
.op { color: #555; }
.prim { color: #6a3a8c; }
.typeref { color: #2e6e44; text-decoration: none; }
.typeref:hover { text-decoration: underline; }
.doc { margin: .35rem 0 0 1.2rem; color: #333; }
.doc p { margin: .25rem 0; }
.doc ul { margin: .25rem 0 .25rem 1.2rem; padding: 0; }
.doc table { border-collapse: collapse; margin: .35rem 0; }
.doc th, .doc td { border: 1px solid #ccc; padding: .15rem .5rem; }
.moduledoc { margin: 0 0 1rem 0; padding: .25rem .8rem; border-left: 3px solid #888; background: #fafafa; }
pre.code { background: #f4f4f4; padding: .5rem .8rem; overflow-x: auto; border-radius: 3px; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: .85rem; }
pre.code .kw { color: #0a55a4; font-weight: 600; }
pre.code .str { color: #b06000; }
pre.code .num { color: #6a3a8c; }
pre.code .com { color: #777; font-style: italic; }
pre.code .esc { color: #b06000; font-weight: 600; }
"""

proc currentModuleOf(input: string): string =
  ## Strip `.sc.nif` / `.s.nif` from the input filename. The base name is the
  ## globally-unique module suffix that appears in every SymId of decls
  ## defined here, so cross-module link resolution can compare against it.
  let mp = splitModulePath(input)
  result = mp.name

proc writeDocIdx(path, currentModule, moduleName, srcPath, relpath: string;
                 entries: seq[DocIdxEntry]) =
  ## Per-module sidecar consumed by the `link` step. NIF-encoded for
  ## consistency with the rest of the pipeline. The `(module …)` clause now
  ## carries the source path and the precomputed relpath so the link step
  ## can synthesise dir-mirrored URLs without re-running the path math.
  var b = nifbuilder.open(path)
  defer: b.close()
  b.addHeader "Dagon", "docidx"
  b.withTree "docidx":
    b.withTree "module":
      b.addStrLit currentModule
      b.addStrLit moduleName
      b.addStrLit srcPath
      b.addStrLit relpath
    for e in entries:
      b.withTree "entry":
        b.addStrLit e.kind
        b.addStrLit e.basename
        b.addStrLit e.symid
        b.addStrLit e.summary

proc renderModule(input, htmlOut, docIdxOut: string; format: HtmlOutFormat;
                  projectRoot, stdlibRoot: string) =
  if not fileExists(input):
    quit "input file not found: " & input
  var buf = parseFromFile(input)
  var n = beginRead(buf)
  let src = sourcePath(n)
  let moduleDoc = docOf(n.info)
  let currentModule = currentModuleOf(input)

  let moduleName =
    if src.len > 0: splitFile(src).name
    else: currentModule
  let subtitle =
    if src.len > 0: src
    else: input

  # Build the render context. Own relpath is computed via the same algorithm
  # deps.nim used to choose where to put us, so cross-page links from sibling
  # modules (with their own currentRelpaths) resolve consistently.
  var ctx = RenderCtx(
    currentModule: currentModule,
    currentRelpath:
      if src.len > 0: deriveRelpath(src, projectRoot, stdlibRoot)
      else: extractFilename(htmlOut),
    importMap: initTable[string, string](),
    projectRoot: projectRoot,
    stdlibRoot: stdlibRoot)

  # Enter (stmts …) and consume any leading (import (kv …)) into the map.
  if n.kind == ParLe: inc n
  parseImports(n, ctx)

  var b = initHtmlBuilder(format)
  var idx: seq[DocIdxEntry] = @[]
  emitTag(b, "html"):
    emitTag(b, "head"):
      emitVoid(b, "meta")
      emitTag(b, "title"):
        emitText(b, moduleName)
      if format == hofHtml:
        emitTag(b, "style"):
          emitText(b, DefaultCss)
    emitTag(b, "body"):
      emitTag(b, "h1"):
        emitText(b, moduleName)
      emitTag(b, "p"):
        emitTag(b, "small"):
          emitText(b, subtitle)
      if moduleDoc.len > 0:
        case b.format
        of hofHtml:
          emitOpenAttr(b, "div", [("class", "moduledoc")])
          renderMarkdown(b, moduleDoc)
          emitClose(b, "div")
        of hofNif:
          emitTag(b, "moduledoc"):
            renderMarkdown(b, moduleDoc)
      case b.format
      of hofHtml:
        emitTagAttr(b, "ul", [("class", "decls")]):
          renderDecls(b, idx, ctx, n)
      of hofNif:
        emitTag(b, "ul"):
          renderDecls(b, idx, ctx, n)

  endRead(buf)
  writeFile(htmlOut, finalize(b))
  writeDocIdx(docIdxOut, currentModule, moduleName, src, ctx.currentRelpath, idx)

type
  IndexEntry = object
    kind: string
    basename: string
    symid: string
    summary: string
    modid: string
    modname: string
    modRelpath: string

proc takeStrLit(n: var Cursor): string =
  if n.kind == StringLit:
    result = pool.strings[n.litId]
    inc n
  else:
    result = ""

proc readDocIdx(path: string; entries: var seq[IndexEntry];
                modules: var seq[(string, string, string)]) =
  ## Parse a `.docidx` sidecar emitted by `dagon module`. Tolerant of unknown
  ## fields so newer schemas are read by older `link` steps without crashing.
  ## `modules` collects `(modid, modname, relpath)` triples for the index.
  if not fileExists(path): return
  var buf = parseFromFile(path)
  var n = beginRead(buf)
  if n.kind != ParLe: endRead(buf); return
  inc n  # enter (docidx …)
  var modid = ""
  var modname = ""
  var modRelpath = ""
  while n.kind != ParRi:
    if n.kind != ParLe:
      inc n
      continue
    let tag = pool.tags[n.tagId]
    inc n
    case tag
    of "module":
      modid = takeStrLit(n)
      modname = takeStrLit(n)
      discard takeStrLit(n)            # srcPath (unused by link step today)
      modRelpath = takeStrLit(n)
      modules.add (modid, modname, modRelpath)
    of "entry":
      var e = IndexEntry(modid: modid, modname: modname, modRelpath: modRelpath)
      e.kind = takeStrLit(n)
      e.basename = takeStrLit(n)
      e.symid = takeStrLit(n)
      e.summary = takeStrLit(n)
      entries.add e
    else: discard
    while n.kind != ParRi: skip n
    inc n  # closing ')'
  endRead(buf)

const IndexCss = """
ul.modules { list-style: none; padding: 0; column-count: 3; column-gap: 1.5rem; }
ul.modules > li { padding: .15rem 0; }
ul.index { list-style: none; padding: 0; }
ul.index > li { padding: .25rem 0; border-bottom: 1px dashed #eee; }
.kind { color: #888; font-style: normal; font-size: .85rem; }
.sum  { color: #555; }
"""

proc renderLinkIndex(output: string; idxFiles: openArray[string];
                     format: HtmlOutFormat) =
  var entries: seq[IndexEntry] = @[]
  var modules: seq[(string, string, string)] = @[]
  for f in idxFiles:
    readDocIdx(f, entries, modules)

  entries.sort do (a, b: IndexEntry) -> int:
    let c = cmpIgnoreCase(a.basename, b.basename)
    if c != 0: c else: cmp(a.modid, b.modid)
  modules.sort do (a, b: (string, string, string)) -> int:
    cmpIgnoreCase(a[1], b[1])

  var b = initHtmlBuilder(format)
  emitTag(b, "html"):
    emitTag(b, "head"):
      emitVoid(b, "meta")
      emitTag(b, "title"):
        emitText(b, "Index")
      if format == hofHtml:
        emitTag(b, "style"):
          emitText(b, DefaultCss & IndexCss)
    emitTag(b, "body"):
      emitTag(b, "h1"):
        emitText(b, "Index")
      emitTag(b, "p"):
        emitText(b, $entries.len & " entries across " &
                    $modules.len & " modules")

      emitTag(b, "h2"):
        emitText(b, "Modules")
      case b.format
      of hofHtml:
        emitTagAttr(b, "ul", [("class", "modules")]):
          for m in modules:
            emitTag(b, "li"):
              emitTagAttr(b, "a", [("href", m[2])]):
                emitText(b, m[1])
      of hofNif:
        emitTag(b, "ul"):
          for m in modules:
            emitTag(b, "li"):
              emitTag(b, "a"):
                emitText(b, m[2])
                emitText(b, m[1])

      emitTag(b, "h2"):
        emitText(b, "Symbols")
      case b.format
      of hofHtml:
        emitTagAttr(b, "ul", [("class", "index")]):
          for e in entries:
            emitTag(b, "li"):
              emitTagAttr(b, "a", [
                  ("class", "name"),
                  ("href", e.modRelpath & "#" & urlEscape(e.symid))]):
                emitText(b, e.basename)
              emitText(b, " ")
              emitClass(b, "kind"):
                emitText(b, e.kind)
              emitText(b, " in ")
              emitTagAttr(b, "a", [("href", e.modRelpath)]):
                emitText(b, e.modname)
              if e.summary.len > 0:
                emitText(b, " — ")
                emitClass(b, "sum"):
                  emitText(b, e.summary)
      of hofNif:
        emitTag(b, "ul"):
          for e in entries:
            emitTag(b, "li"):
              emitTag(b, "a"):
                emitText(b, e.modRelpath & "#" & urlEscape(e.symid))
                emitText(b, e.basename)
              emitClass(b, "kind"):
                emitText(b, e.kind)
              emitTag(b, "a"):
                emitText(b, e.modRelpath)
                emitText(b, e.modname)
              if e.summary.len > 0:
                emitClass(b, "sum"):
                  emitText(b, e.summary)

  writeFile(output, finalize(b))

proc handleCmdLine() =
  var action = ""
  var args: seq[string] = @[]
  var format = hofHtml
  var projectRoot = ""
  var stdlibRoot = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if action.len == 0:
        action = key.normalize
      else:
        args.add key
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      of "format":
        case normalize(val)
        of "html": format = hofHtml
        of "nif":  format = hofNif
        else: quit "invalid value for --format; expected 'html' or 'nif'"
      of "projectroot": projectRoot = val
      of "stdlibroot":  stdlibRoot = val
      else: writeHelp()
    of cmdEnd: assert false, "cannot happen"

  if action.len == 0:
    writeHelp()
  case action
  of "m", "module":
    if args.len < 3:
      quit "'module' takes <input.sc.nif> <output.html> <output.docidx>"
    renderModule(args[0], args[1], args[2], format, projectRoot, stdlibRoot)
  of "link":
    if args.len < 2:
      quit "'link' takes <output.html> <docidx>..."
    renderLinkIndex(args[0], args.toOpenArray(1, args.len - 1), format)
  else:
    quit "Invalid action: " & action

when isMainModule:
  handleCmdLine()
