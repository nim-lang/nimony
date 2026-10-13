#
#
#            Nimony's Standard Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The `headers"..."` plugin of `std/httpclient`: a header block literal,
## checked while the program is compiled. A malformed block — a line without
## `\r\n`, a name outside the token characters, a line break inside a value —
## is a compile-time error at the literal. A well-formed one becomes
## `HttpHeaders("...")` with its spelled-out `\r\n` turned into line ends.

import plugins
import std / http / httpheaders

proc literalText(n: NifCursor; ok: var bool): string =
  ## A generalized raw literal `headers"..."` reaches the plugin as
  ## `(suf "..." "R")`; an ordinary call `headers("...")` as the string itself.
  ok = false
  result = ""
  if n.kind == StrLit:
    ok = true
    result = stringValue(n)
  elif n.kind == TagLit and n.tagText == "suf":
    let f = firstChild(n)
    if f.kind == StrLit:
      ok = true
      result = stringValue(f)

proc transform(n: NifCursor): NifBuilder =
  let info = n.info
  var arg = callArgs(n)
  var ok = false
  let lit = if arg.hasMore: literalText(arg, ok) else: ""
  if not ok:
    return errorTree("`headers` needs a string literal; build a computed " &
                     "header block with `add`", n)
  let text = spelledLineEnds(lit)
  # what is left spelled out was meant as a line break, and one without its
  # partner is exactly the bare-LF line split that header injection relies on
  for i in 0 ..< text.len - 1:
    if text[i] == '\\' and text[i+1] in {'r', 'n'}:
      return errorTree("malformed header block: a line ends in `\\r\\n`, " &
                       "not in a lone `\\" & text.substr(i+1, i+1) & "`", arg)
  let err = headerBlockError(text)
  if err.len > 0:
    return errorTree("malformed header block: " & err, arg)
  result = createTree()
  result.withTree CallX, info:
    result.bindSym "HttpHeaders"
    result.addStrLit text

var input = loadPluginInput()
saveTree transform(input)
