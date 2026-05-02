#       Dagon
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Dagon is the documentation backend for Nimony, parallel to NIFC for code
## generation. It reads a post-sem `.s.nif` and writes one `.html` per module.
## This is the step-1 stub: it walks the top-level decls and emits a trivial
## name listing.

import std / [parseopt, os, strutils, syncio, assertions]

include ".." / lib / nifprelude
import ".." / lib / symparser
import ".." / nimony / nimony_model

const
  Version = "0.1.0"
  Usage = "Dagon - NIF documentation backend. Version " & Version & """

  (c) 2026 Andreas Rumpf
Usage:
  dagon [options] [command] [arguments]
Command:
  m|module input.s.nif output.html       render docs for one module

Options:
  --version             show the version
  --help                show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

type
  Decl = object
    kind: NimonyStmt
    sym: SymId
    doc: string  ## raw markdown body from the `##` doc comment, "" if none

const
  DeclKinds = {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS,
               TypeS, ConstS, GvarS, TvarS, GletS, TletS}

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

proc docOf(info: PackedLineInfo): string =
  let raw = unpack(pool.man, info)
  if raw.comment != 0'u32: pool.strings[StrId(raw.comment)]
  else: ""

proc collectDecls(n: var Cursor; result: var seq[Decl]) =
  if n.kind != ParLe: return
  inc n
  while n.kind != ParRi:
    if n.kind == ParLe:
      let sk = n.stmtKind
      if sk in DeclKinds:
        let info = n.info
        var inner = n
        inc inner
        if inner.kind == SymbolDef:
          result.add Decl(kind: sk, sym: inner.symId, doc: docOf(info))
      skip n
    else:
      inc n

proc sourcePath(n: Cursor): string =
  ## Pull the source `.nim` path off the outer `(stmts ...)` token's line info.
  if n.kind != ParLe: return ""
  let raw = unpack(pool.man, n.info)
  if raw.file.isValid:
    result = pool.files[raw.file]
  else:
    result = ""

proc basename(symName: string): string =
  var work = symName
  extractBasename(work)
  result = work

proc renderModule(input, output: string) =
  if not fileExists(input):
    quit "input file not found: " & input
  var buf = parseFromFile(input)
  var n = beginRead(buf)
  let src = sourcePath(n)
  let moduleDoc = docOf(n.info)
  var decls: seq[Decl] = @[]
  collectDecls(n, decls)
  endRead(buf)

  let moduleName =
    if src.len > 0: splitFile(src).name
    else: splitModulePath(input).name
  let subtitle =
    if src.len > 0: src
    else: input

  var html = ""
  html.add "<!DOCTYPE html>\n"
  html.add "<html><head><meta charset=\"utf-8\">"
  html.add "<title>" & moduleName & "</title></head><body>\n"
  html.add "<h1>" & moduleName & "</h1>\n"
  html.add "<p><small>" & subtitle & "</small></p>\n"
  if moduleDoc.len > 0:
    html.add "<blockquote>" & moduleDoc & "</blockquote>\n"
  html.add "<ul>\n"
  for d in decls:
    let symName = pool.syms[d.sym]
    html.add "<li><b>" & kindLabel(d.kind) & "</b> <code>" &
             basename(symName) & "</code> <small>" & symName & "</small>"
    if d.doc.len > 0:
      html.add " — <em>" & d.doc & "</em>"
    html.add "</li>\n"
  html.add "</ul>\n"
  html.add "</body></html>\n"

  writeFile(output, html)

proc handleCmdLine() =
  var action = ""
  var args: seq[string] = @[]
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
      else: writeHelp()
    of cmdEnd: assert false, "cannot happen"

  if action.len == 0:
    writeHelp()
  case action
  of "m", "module":
    if args.len < 2:
      quit "'module' takes <input.s.nif> <output.html>"
    renderModule(args[0], args[1])
  else:
    quit "Invalid action: " & action

when isMainModule:
  handleCmdLine()
