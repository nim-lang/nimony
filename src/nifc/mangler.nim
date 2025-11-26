#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Name mangling. See nifc-spec.md for details.

from std / strutils import toOctal, replace, endsWith

proc escape(result: var string; c: char) {.inline.} =
  const HexChars = "0123456789ABCDEF"
  var n = int(c)
  result.add 'X'
  result.add HexChars[n shr 4 and 0xF]
  result.add HexChars[n and 0xF]
  result.add 'Q'

proc isImportC*(s: string): bool =
  s.len > 2 and s[s.len-2] == '.' and s[s.len-1] == 'c'

proc mangle*(s: string): string =
  if s.isImportC:
    result = substr(s, 0, s.len-3)
  else:
    var i = 0
    result = newStringOfCap(s.len)
    while i < s.len:
      case s[i]
      of 'A'..pred('Q'), succ('Q')..'Z', 'a'..'z', '0'..'9':
        result.add s[i]
      of 'Q': result.add "QQ"
      of '_': result.add "Q_"
      of '.': result.add '_'
      of '[':
        if i < s.len-1 and s[i+1] == ']':
          if i < s.len-2 and s[i+2] == '=':
            result.add "putQ"
            inc i, 2
          else:
            result.add "getQ"
            inc i
        else:
          result.escape '['
      of '=':
        if i < s.len-1 and s[i+1] == '=':
          result.add "eqQ"
          inc i
        else:
          result.add "eQ"
      of '<':
        if i < s.len-1 and s[i+1] == '=':
          result.add "leQ"
          inc i
        else:
          result.add "ltQ"
      of '>':
        if i < s.len-1 and s[i+1] == '=':
          result.add "geQ"
          inc i
        else:
          result.add "gtQ"
      of '$': result.add "dollarQ"
      of '%': result.add "percentQ"
      of '&': result.add "ampQ"
      of '^': result.add "roofQ"
      of '!': result.add "emarkQ"
      of '?': result.add "qmarkQ"
      of '*': result.add "starQ"
      of '+': result.add "plusQ"
      of '-': result.add "minusQ"
      of '/': result.add "slashQ"
      of '\\': result.add "bslashQ"
      of '~': result.add "tildeQ"
      of ':': result.add "colonQ"
      of '@': result.add "atQ"
      of '|': result.add "barQ"
      else:
        result.escape s[i]
      inc i

proc toCChar*(c: char; result: var string) {.inline.} =
  case c
  of '\0'..'\x1F', '\x7F'..'\xFF':
    result.add '\\'
    result.add toOctal(c)
  of '\'', '\"', '\\', '?':
    result.add '\\'
    result.add c
  else:
    result.add c

proc makeCString*(s: string): string =
  result = newStringOfCap(s.len + 10)
  result.add('"')
  for c in s: toCChar(c, result)
  result.add('"')

when isMainModule:
  import std/assertions

  assert mangle"foo.3.baz" == "foo_3_baz"
  assert mangle"Query?" == "QQueryqmarkQ"
  assert mangle"abc_def_[]=" == "abcQ_defQ_putQ"
  assert mangle"[]" == "getQ"
