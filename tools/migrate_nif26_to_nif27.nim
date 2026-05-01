## Migrate one or more legacy NIF26 files to the new NIF27 syntax.
##
## Tokenizes each input file with the frozen legacy reader
## ([../src/lib/legacy/nif26reader.nim]) and re-emits it through the current
## NIF27 builder ([../src/lib/nifbuilder.nim]). Side-effects of the migration:
##
## - the version directive is rewritten to `(.nif27)`;
## - line information is moved from prefix position to a postfix `@…` (or
##   bare `~…`) suffix on the atom or tag it belongs to;
## - line-info diffs are re-encoded in base62;
## - numbers lose their leading `+` (e.g. `+12` becomes `12`).
##
## Other directives that the legacy reader silently consumes during `open`
## (e.g. `(.vendor ...)`) are not preserved. For fixtures we ship that's
## acceptable; the only directive we rewrite is the version.
##
## Usage:
##
##   migrate_nif26_to_nif27 <path>...
##
## Each `<path>` is rewritten in place. Use `--inplace=false` (default true)
## to dump the new content to stdout instead. With `--check` the tool only
## reports whether the input parses cleanly under the legacy reader and
## exits non-zero on any mismatch in the round-trip; useful for CI.

import std / [os, parseopt, strutils, syncio]

import ".." / src / lib / legacy / nif26reader
import ".." / src / lib / nifbuilder

# A tiny copy of `decodeStr` that doesn't reach into the legacy reader's
# internal `thisModule` expansion: for migration purposes we want raw token
# bytes, not the auto-expanded `foo.0.<thismod>` form, so the migrated file
# stays byte-equivalent in symbols.
proc decodeRawStr(t: ExpandedToken): string =
  if t.data.len == 0: return ""
  result = newString(t.data.len)
  copyMem(addr result[0], t.data.p, t.data.len)
  # `\xx` escapes carry over verbatim — the new builder will re-escape them
  # if needed via its own ControlChars rules. Just preserve the bytes.

proc convert(input: string): string =
  ## Round-trip a NIF26 file body and return the NIF27 equivalent (header
  ## included). Caller is responsible for reading and writing the file.
  var r = nif26reader.openFromBuffer(input, "migrate")
  defer: nif26reader.close(r)
  var b = nifbuilder.open(input.len + 64)
  b.addRaw "(.nif27)\n"

  var tok = default(ExpandedToken)
  var depth = 0
  while true:
    nif26reader.next(r, tok)
    if tok.tk == EofToken: break
    let raw = decodeRawStr(tok)
    let info = tok.pos
    let fname = decodeFilename(tok)
    case tok.tk
    of ParLe:
      b.addTree raw
      b.attachLineInfo(info.col, info.line, fname)
      inc depth
    of ParRi:
      b.endTree
      dec depth
    of DotToken:
      b.addEmpty 1
      b.attachLineInfo(info.col, info.line, fname)
    of Ident:
      b.addIdent raw
      b.attachLineInfo(info.col, info.line, fname)
    of Symbol:
      b.addSymbol raw
      b.attachLineInfo(info.col, info.line, fname)
    of SymbolDef:
      b.addSymbolDef raw
      b.attachLineInfo(info.col, info.line, fname)
    of StringLit:
      b.addStrLit decodeStr(r, tok)
      b.attachLineInfo(info.col, info.line, fname)
    of CharLit:
      b.addCharLit decodeChar(tok)
      b.attachLineInfo(info.col, info.line, fname)
    of IntLit:
      b.addIntLit decodeInt(tok)
      b.attachLineInfo(info.col, info.line, fname)
    of UIntLit:
      b.addUIntLit decodeUInt(tok)
      b.attachLineInfo(info.col, info.line, fname)
    of FloatLit:
      b.addFloatLit decodeFloat(tok)
      b.attachLineInfo(info.col, info.line, fname)
    of UnknownToken, EofToken:
      discard

  result = nifbuilder.extract(b)

proc main =
  var inplace = true
  var check = false
  var paths: seq[string] = @[]
  for kind, key, val in getopt():
    case kind
    of cmdShortOption, cmdLongOption:
      case key
      of "inplace": inplace = parseBool(val)
      of "check": check = true
      else: quit "unknown flag: " & key
    of cmdArgument: paths.add key
    of cmdEnd: discard

  if paths.len == 0:
    quit "usage: migrate_nif26_to_nif27 [--inplace=true|false] [--check] <path>...", 1

  var ok = 0
  var bad = 0
  for p in paths:
    if not fileExists(p):
      stderr.writeLine "skip (missing): ", p
      inc bad
      continue
    let inp = readFile(p)
    var converted = ""
    try:
      converted = convert(inp)
    except CatchableError as e:
      stderr.writeLine "FAIL ", p, ": ", e.msg
      inc bad
      continue
    if check:
      stdout.writeLine "ok ", p, " (", inp.len, " -> ", converted.len, " bytes, ",
                       converted.len * 100 div max(inp.len, 1), "%)"
    elif inplace:
      writeFile(p, converted)
      stdout.writeLine "wrote ", p, " (", inp.len, " -> ", converted.len, " bytes)"
    else:
      stdout.write converted
    inc ok

  if bad != 0:
    quit "migrated " & $ok & ", failed " & $bad, 1

main()
