#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Tag-stream builder that targets either HTML (`<tag>body</tag>`) or NIF
## (`(tag body)`). Dagon emits its rendered output through this so the same
## producer can drive a polished HTML build *or* a structural NIF dump that
## downstream tools can re-process. NIF and HTML happen to share the same
## tree-of-named-nodes shape — this module is the thin adapter that exploits
## that overlap.

import nifbuilder

type
  HtmlOutFormat* = enum
    hofHtml   ## `<tag>body</tag>`
    hofNif    ## `(tag body)`
  HtmlBuilder* = object
    case format*: HtmlOutFormat
    of hofHtml:
      buf: string
    of hofNif:
      nif: nifbuilder.Builder

proc initHtmlBuilder*(format: HtmlOutFormat; sizeHint = 1024): HtmlBuilder =
  case format
  of hofHtml:
    result = HtmlBuilder(format: hofHtml, buf: newStringOfCap(sizeHint))
    result.buf.add "<!DOCTYPE html>\n"
  of hofNif:
    var nif = nifbuilder.open(sizeHint)
    nif.addHeader "Dagon", "doc"
    result = HtmlBuilder(format: hofNif, nif: nif)

proc finalize*(b: var HtmlBuilder): string =
  ## Consume the builder and return the produced text.
  case b.format
  of hofHtml: result = move(b.buf)
  of hofNif:  result = extract(move(b.nif))

proc emitOpen*(b: var HtmlBuilder; tag: string) =
  case b.format
  of hofHtml:
    b.buf.add '<'
    b.buf.add tag
    b.buf.add '>'
  of hofNif:
    b.nif.addTree tag

proc emitClose*(b: var HtmlBuilder; tag: string) =
  case b.format
  of hofHtml:
    b.buf.add "</"
    b.buf.add tag
    b.buf.add '>'
  of hofNif:
    b.nif.endTree()

template emitTag*(b: var HtmlBuilder; tag: string; body: untyped) =
  ## Open `tag`, run `body` (which emits children), close `tag`.
  emitOpen(b, tag)
  body
  emitClose(b, tag)

proc emitText*(b: var HtmlBuilder; s: string) =
  ## Emit user text. HTML-escapes for hofHtml; intern as a NIF string literal
  ## for hofNif.
  case b.format
  of hofHtml:
    for c in s:
      case c
      of '<': b.buf.add "&lt;"
      of '>': b.buf.add "&gt;"
      of '&': b.buf.add "&amp;"
      else:   b.buf.add c
  of hofNif:
    b.nif.addStrLit s

proc emitVoid*(b: var HtmlBuilder; tag: string) =
  ## Self-closing tag (`<br>` / `(br)`).
  case b.format
  of hofHtml:
    b.buf.add '<'
    b.buf.add tag
    b.buf.add '>'
  of hofNif:
    b.nif.addKeyw tag

proc emitAttrValueHtml(b: var HtmlBuilder; v: string) =
  for c in v:
    case c
    of '<': b.buf.add "&lt;"
    of '>': b.buf.add "&gt;"
    of '&': b.buf.add "&amp;"
    of '"': b.buf.add "&quot;"
    else:   b.buf.add c

proc emitOpenAttr*(b: var HtmlBuilder; tag: string;
                   attrs: openArray[(string, string)]) =
  ## Open `tag` with key/value attributes. HTML: `<tag k="v" …>`. NIF:
  ## `(tag (k "v") …)` — each attr becomes a child node whose tag is the
  ## attribute name and whose only child is the value as a string literal.
  case b.format
  of hofHtml:
    b.buf.add '<'
    b.buf.add tag
    for (k, v) in attrs:
      b.buf.add ' '
      b.buf.add k
      b.buf.add "=\""
      emitAttrValueHtml(b, v)
      b.buf.add '"'
    b.buf.add '>'
  of hofNif:
    b.nif.addTree tag
    for (k, v) in attrs:
      b.nif.addTree k
      b.nif.addStrLit v
      b.nif.endTree()

template emitTagAttr*(b: var HtmlBuilder; tag: string;
                      attrs: openArray[(string, string)]; body: untyped) =
  ## Open `tag` with attributes, run body, close tag.
  emitOpenAttr(b, tag, attrs)
  body
  emitClose(b, tag)

proc emitClassOpen*(b: var HtmlBuilder; cls: string) =
  if b.format == hofHtml:
    emitOpenAttr(b, "span", [("class", cls)])
  else:
    emitOpen(b, cls)

proc emitClassClose*(b: var HtmlBuilder; cls: string) =
  if b.format == hofHtml:
    emitClose(b, "span")
  else:
    emitClose(b, cls)

template emitClass*(b: var HtmlBuilder; cls: string; body: untyped) =
  ## Inline classified span. HTML: `<span class="cls">body</span>`.
  ## NIF: `(cls body)` — the class name *is* the tag, since NIF has no
  ## attribute concept and a tagged subtree carries the same semantic.
  emitClassOpen(b, cls)
  body
  emitClassClose(b, cls)
