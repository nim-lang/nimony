#       Dagon
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Code-block syntax highlighting via Nim's `std/packages/docutils/highlite`.
## Tokenises a code string and emits classified spans through `htmlbuilder`,
## so the same caller produces either `<span class="kw">…</span>` HTML or
## `(kw "…")` NIF.

import packages / docutils / highlite
import ".." / lib / htmlbuilder

proc tokenClassName(k: TokenClass): string =
  ## Map highlite's fine-grained classes onto the small set of CSS classes
  ## the dagon stylesheet knows about. Returns "" to mean "emit as plain text".
  case k
  of gtKeyword: "kw"
  of gtStringLit, gtLongStringLit, gtCharLit, gtRawData: "str"
  of gtDecNumber, gtBinNumber, gtHexNumber, gtOctNumber, gtFloatNumber: "num"
  of gtComment, gtLongComment: "com"
  of gtEscapeSequence: "esc"
  of gtPreprocessor, gtDirective: "pre"
  else: ""

proc renderCode*(b: var HtmlBuilder; lang: string; code: string) =
  ## Emit a `<pre>` (HTML) or `(pre …)` (NIF) block with each token wrapped
  ## in a classified span. `lang` is a highlite language name (`Nim`, `C`,
  ## `Python`, …); empty defaults to Nim.
  let sl =
    if lang.len == 0: langNim
    else:
      let g = getSourceLanguage(lang)
      if g == langNone: langNim else: g
  case b.format
  of hofHtml:
    emitOpenAttr(b, "pre", [("class", "code")])
  of hofNif:
    emitOpen(b, "pre")
    if lang.len > 0:
      emitTag(b, "lang"):
        emitText(b, lang)
  var g = default(GeneralTokenizer)
  initGeneralTokenizer(g, code)
  while true:
    getNextToken(g, sl)
    if g.kind == gtEof: break
    let tok = substr(code, g.start, g.start + g.length - 1)
    let cls = tokenClassName(g.kind)
    if cls.len > 0:
      emitClass(b, cls):
        emitText(b, tok)
    else:
      emitText(b, tok)
  deinitGeneralTokenizer(g)
  emitClose(b, "pre")
