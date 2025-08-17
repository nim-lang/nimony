## Dagon is Nimony's new documentation tool.
## Currently it is very simple: It takes a `.dgn.md` or `.dgn.html` file and
## outputs a `.md` or `.html` file. The input file contains references to Nim symbols that Dagon fills in. References to Nim identifiers are written as `#namehere` (without the backticks) in the input file.
## Dagon produces HTML code that works both within an HTML and a Markdown output file.

import std / [strutils, tables, os, parseopt]
import packages / docutils / highlite
import "$nim" / compiler / [lexer, llstream, parser, ast, renderer, pathutils, options, msgs, syntaxes, idents]

const
  Usage = """
Dagon - Nim documentation preprocessor

Usage: dagon [options] <input-file>

Options:
  -o, --output:<file>     Output file (default: input file with `.dgn.` that is replaced by `.`)
  -h, --help             Show this help message
  --tests:<dir>          Directory to store test files (default: tests/dagon)

Examples:
  dagon docs.dgn.md
  dagon -o:output.md docs.dgn.md
"""

const
  StrLitBegin = "<em>"
  StrLitEnd = "</em>"
  NimCodePrefix = "```nim"
  NimCodeSuffix = "```"

type
  Declaration = object
    name: string
    covered: bool
    ast: PNode
  Declarations = ref object
    decls: seq[Declaration]
  Context = object
    nimCode: Table[string, Declarations]
    nextTestId: int

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
      result.add StrLitBegin
      result.add tok
      result.add StrLitEnd
    else:
      result.add tok
      if kind == tkSpaces and tok.len > 0 and tok[0] == '\n': hasNewlines = true

proc fillinCode(result: var string; c: var Context; ident, currentFile: string; headerDepth: int) =
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
          result.add "<h" & $headerDepth & ">"
          result.add ident
          result.add "</h" & $headerDepth & ">\n"

        decl.covered = true
        var hasNewlines = false
        let s = decl.ast.nodeToString(hasNewlines)
        result.add "<pre><code>"
        result.add s
        result.add "</code></pre>\n"
        inc matches

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
      result.add StrLitBegin
    else: discard

    add(result, substr(code, g.start, g.length + g.start - 1))
    case g.kind
    of gtKeyword: result.add "</b>"
    of gtStringLit, gtLongStringLit, gtCharLit: result.add StrLitEnd
    else: discard
  deinitGeneralTokenizer(g)

proc process(c: var Context; md: string; currentFile: var string; baseDir, testsDir: string): string =
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
      if not isAbsolute(currentFile):
        currentFile = baseDir / currentFile
      inc i
    elif md[i] == '#' and md[i+1] notin Whitespace:
      inc i
      var headerDepth = 1
      while i < md.len and md[i] == '#':
        inc headerDepth
        inc i
      var ident = ""
      while i < md.len and md[i] notin Whitespace:
        ident.add md[i]
        inc i

      if currentFile.len == 0:
        quit "No file specified, use '@file' to specify the file"
      result.fillinCode(c, ident, currentFile, headerDepth)
    elif md.continuesWith(NimCodePrefix, i):
      # render the code block ourselves to have a shield against broken highlighting:
      inc i, NimCodePrefix.len
      while i < md.len and md[i] == ' ': inc i
      var isTest = false
      if md.continuesWith("test", i):
        inc i, len("test")
        isTest = true
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
      if isTest:
        inc c.nextTestId
        createDir(testsDir)
        writeFile(testsDir / "t" & $c.nextTestId & ".nim", code)
    else:
      result.add md[i]
      inc i

proc showHelp() =
  echo Usage

proc main() =
  var
    infile = ""
    outfile = ""
    testsDir = "tests/dagon"

  var p = initOptParser()
  while true:
    next(p)
    case p.kind
    of cmdEnd: break
    of cmdShortOption, cmdLongOption:
      case p.key
      of "o", "output":
        outfile = p.val
      of "h", "help":
        showHelp()
        quit QuitSuccess
      of "tests":
        testsDir = p.val
      else:
        echo "Unknown option: ", p.key
        showHelp()
        quit QuitFailure
    of cmdArgument:
      if infile.len == 0:
        infile = p.key
      else:
        echo "Multiple input files not supported"
        quit QuitFailure

  if infile.len == 0:
    echo "No input file specified"
    showHelp()
    quit QuitFailure

  if outfile.len == 0 and not infile.contains(".dgn."):
    echo "Input file must contain `.dgn.`"
    quit QuitFailure

  var c = Context()
  let content = readFile(infile)
  var currentFile = ""
  let baseDir = infile.splitFile.dir
  let md = process(c, content, currentFile, baseDir, testsDir)
  let outf = if outfile.len == 0: infile.replace(".dgn.", ".") else: outfile
  writeFile(outf, md)

  # warn about uncovered declarations:
  for file in c.nimCode.keys:
    for decl in mitems(c.nimCode[file].decls):
      if not decl.covered:
        echo "Warning: `" & decl.name & "` in " & file & " is not documented"

when isMainModule:
  main()
