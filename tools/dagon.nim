## Dagon is Nimony's new documentation tool.
## Currently it is very simple: It takes a `.src.md` file and
## outputs a `.md` file. The `.src.md` file contains references to Nim proc
## headers etc that Dagon fills in. This way the prototypes cannot
## get out of sync. References to Nim identifiers are written as `#namehere` (without the backticks).
## Dagon produces HTML code inside the Markdown file so that we are
## independent of Markdown's typically terrible Nim code renderer.

import std / [strutils, tables, os]
import packages / docutils / highlite
import "$nim" / compiler / [lexer, llstream, parser, ast, renderer, pathutils, options, msgs, syntaxes, idents]

type
  Declaration = object
    name: string
    covered: bool
    ast: PNode
  Declarations = ref object
    decls: seq[Declaration]
  Context = object
    nimCode: Table[string, Declarations]

proc createConf(): ConfigRef =
  result = newConfigRef()
  #result.notes.excl hintLineTooLong
  result.errorMax = 1000

proc parseFile(thisfile: string): PNode =
  let stream = llStreamOpen(AbsoluteFile thisfile, fmRead)
  if stream == nil:
    quit "cannot open file: " & thisfile
  else:
    var conf = createConf()
    let fileIdx = fileInfoIdx(conf, AbsoluteFile thisfile)
    var parser: Parser = default(Parser)
    syntaxes.openParser(parser, fileIdx, stream, newIdentCache(), conf)
    result = parseAll(parser)
    closeParser(parser)
    if conf.errorCounter > 0:
      quit QuitFailure

proc getName(n: PNode; exported: var bool): string =
  case n.kind
  of nkPostfix:
    exported = true
    result = getName(n[1], exported)
  of nkPragmaExpr: result = getName(n[0], exported)
  of nkSym: result = n.sym.name.s
  of nkIdent: result = n.ident.s
  of nkAccQuoted:
    for i in 0..<n.len: result.add(getName(n[i], exported))
  of nkOpenSymChoice, nkClosedSymChoice, nkOpenSym:
    result = getName(n[0], exported)
  else:
    result = ""

proc extractDecls(n: PNode; results: var seq[Declaration]) =
  case n.kind
  of nkNone..nkNilLit: discard
  of routineDefs:
    var exported = false
    let name = getName(n[0], exported)
    if exported:
      results.add Declaration(name: name, ast: n)
  of nkIdentDefs, nkConstDef:
    var exported = false
    let name = getName(n[0], exported)
    if exported:
      results.add Declaration(name: name, ast: n)
  of nkWhenStmt:
    # pretend the first branch is what we are interested in:
    extractDecls(n[0], results)
  of nkTypeSection:
    for i in 0..<n.len:
      var exported = false
      let name = getName(n[i][0], exported)
      if exported:
        results.add Declaration(name: name, ast: newTree(nkTypeSection, n[i]))
  else:
    for ch in n: extractDecls(ch, results)

const
  StrLitSpan = "<span style=\"color: #6a737d;\">"

proc nodeToString(n: PNode; hasNewlines: var bool): string =
  result = ""
  var r = initTokRender(n, {renderNoBody, renderNoComments, renderDocComments,
    renderExpandUsing, renderNoPostfix})
  var kind = tkEof
  var tok = ""
  while true:
    getNextTok(r, kind, tok)
    if kind == tkEof:
      break
    if kind in tokKeywordLow..tokKeywordHigh:
      result.add "<b>"
      result.add tok
      result.add "</b>"
    elif kind in tkStrLit..tkCharLit:
      result.add StrLitSpan
      result.add tok
      result.add "</span>"
    else:
      result.add tok
      if kind == tkSpaces and tok.len > 0 and tok[0] == '\n': hasNewlines = true

proc fillinCode(result: var string; c: var Context; ident, currentFile: string) =
  var d = c.nimCode.getOrDefault(currentFile)
  if d == nil:
    let nodes = parseFile(currentFile)
    d = Declarations()
    extractDecls(nodes, d.decls)
    c.nimCode[currentFile] = d
  if d == nil or d.decls.len == 0:
    result.add "ERROR: Could not find declaration for " & ident & " in " & currentFile & "\n"
  else:
    var matches = 0
    for decl in mitems(d.decls):
      if decl.name == ident:
        if matches == 0:
          result.add "<h1>"
          result.add ident
          result.add "</h1>\n"

        decl.covered = true
        var hasNewlines = false
        let s = decl.ast.nodeToString(hasNewlines)
        result.add "<pre><code>"
        result.add s
        result.add "</code></pre>\n"
        inc matches

const
  NimCodePrefix = "```nim"
  NimCodeSuffix = "```"

proc nimCodeToHtml(code: string): string =
  var g: GeneralTokenizer
  initGeneralTokenizer(g, code)
  result = newStringOfCap(code.len * 2)
  while true:
    getNextToken(g, langNim)
    case g.kind
    of gtEof: break
    of gtNone, gtWhitespace: discard
    of gtKeyword:
      result.add "<b>"
    of gtStringLit, gtLongStringLit, gtCharLit:
      result.add StrLitSpan
    else: discard

    add(result, substr(code, g.start, g.length + g.start - 1))
    case g.kind
    of gtKeyword: result.add "</b>"
    of gtStringLit, gtLongStringLit, gtCharLit: result.add "</span>"
    else: discard
  deinitGeneralTokenizer(g)

proc processMarkdown(c: var Context; md: string; currentFile: var string): string =
  var i = 0
  result = newStringOfCap(md.len)
  while i < md.len-1:
    if (i == 0 and md[0] == '@') or (md[i] == '\n' and md[i+1] == '@'):
      # file processing instruction:
      if md[i] == '\n': inc i
      inc i
      currentFile.setLen 0
      while i < md.len and md[i] != '\n':
        currentFile.add md[i]
        inc i
      inc i
    elif md[i] == '#' and md[i+1] notin Whitespace:
      inc i
      var ident = ""
      while i < md.len and md[i] notin Whitespace:
        ident.add md[i]
        inc i

      result.fillinCode(c, ident, currentFile)
    elif md.continuesWith(NimCodePrefix, i):
      # render the code block ourselves to have a shield against broken highlighting:
      inc i, NimCodePrefix.len
      while i < md.len and md[i] == ' ': inc i
      if md.continuesWith("test", i): inc i, len("test")
      while i < md.len and md[i] == ' ': inc i

      var code = ""
      while i < md.len and not md.continuesWith(NimCodeSuffix, i):
        code.add md[i]
        inc i
      inc i, NimCodeSuffix.len
      result.add "<pre><code>"
      result.add nimCodeToHtml(code)
      result.add "</code></pre>"
      # also see if it is followed by ` test`. If so, extract it as a
      # test case. But let hastur run it later.
    else:
      result.add md[i]
      inc i

proc main(infile: string) =
  if not infile.endsWith(".src.md"):
    echo "Input file must end with .src.md"
    quit QuitFailure

  var c = Context()
  let content = readFile(infile)
  var currentFile = "lib/std" / infile.splitFile.name & ".nim"
  let md = processMarkdown(c, content, currentFile)
  writeFile(infile.replace(".src.md", ".md"), md)
  # warn about uncovered declarations:
  for file in c.nimCode.keys:
    for decl in mitems(c.nimCode[file].decls):
      if not decl.covered:
        echo "Warning: `" & decl.name & "` in " & file & " is not documented"

if paramCount() == 0:
  echo "Usage: dagon <input file>"
  quit QuitFailure
main paramStr(1)
