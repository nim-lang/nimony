#       Dagon
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Minimal streaming Markdown renderer for Nimony doc comments. Produces
## htmlbuilder events directly — no AST, no intermediate string buffer —
## so the same parse drives either HTML or NIF output.
##
## Subset:
##   - headers `# … `, `## … `, `### … `
##   - paragraphs (blank-line separated)
##   - bullet lists (`- ` or `* `)
##   - fenced code blocks (` ```lang ` … ` ``` `) — highlighted via highlite
##   - pipe tables
##   - inline `**bold**`, `*italic*`, `` `code` ``, `[text](url)`

import std / strutils
import ".." / lib / htmlbuilder
import codehl

# ---- inline ---------------------------------------------------------------

proc renderInline*(b: var HtmlBuilder; s: string)

proc findClose(s: string; start: int; ch: char): int =
  ## Returns index of the closing `ch` at or after `start`, or -1.
  var i = start
  while i < s.len:
    if s[i] == ch: return i
    inc i
  -1

proc findCloseTwo(s: string; start: int; ch: char): int =
  ## Returns index of the first `ch` followed by another `ch`, at or after
  ## `start`. Used for `**bold**` close detection. -1 if not found.
  var i = start
  while i + 1 < s.len:
    if s[i] == ch and s[i+1] == ch: return i
    inc i
  -1

proc flushPending(b: var HtmlBuilder; pending: var string) =
  if pending.len > 0:
    emitText(b, pending)
    pending.setLen 0

proc renderInline*(b: var HtmlBuilder; s: string) =
  var i = 0
  var pending = ""
  while i < s.len:
    let c = s[i]
    case c
    of '`':
      flushPending(b, pending)
      let close = findClose(s, i + 1, '`')
      if close >= 0:
        emitTag(b, "code"):
          emitText(b, s[i+1 ..< close])
        i = close + 1
      else:
        pending.add c
        inc i
    of '*', '_':
      flushPending(b, pending)
      if i + 1 < s.len and s[i+1] == c:
        # **bold** / __bold__
        let close = findCloseTwo(s, i + 2, c)
        if close >= 0:
          emitTag(b, "strong"):
            renderInline(b, s[i+2 ..< close])
          i = close + 2
        else:
          pending.add c
          inc i
      else:
        let close = findClose(s, i + 1, c)
        if close >= 0:
          emitTag(b, "em"):
            renderInline(b, s[i+1 ..< close])
          i = close + 1
        else:
          pending.add c
          inc i
    of '[':
      flushPending(b, pending)
      let textEnd = findClose(s, i + 1, ']')
      if textEnd >= 0 and textEnd + 1 < s.len and s[textEnd+1] == '(':
        let urlEnd = findClose(s, textEnd + 2, ')')
        if urlEnd >= 0:
          let text = s[i+1 ..< textEnd]
          let url = s[textEnd+2 ..< urlEnd]
          emitTagAttr(b, "a", [("href", url)]):
            renderInline(b, text)
          i = urlEnd + 1
        else:
          pending.add c
          inc i
      else:
        pending.add c
        inc i
    else:
      pending.add c
      inc i
  flushPending(b, pending)

# ---- block ----------------------------------------------------------------

proc isFence(s: string): bool {.inline.} = s.startsWith("```")
proc isBullet(s: string): bool {.inline.} = s.startsWith("- ") or s.startsWith("* ")
proc isHeader(s: string): bool {.inline.} = s.startsWith("#")

proc isTableSeparator(s: string): bool =
  ## A markdown table separator looks like `|---|---|` or `|:--|--:|`. We
  ## demand at least one `-` and only `|`, `-`, `:`, ` `.
  if s.len < 3: return false
  var sawDash = false
  for c in s:
    case c
    of '|', '-', ':', ' ': discard
    else: return false
    if c == '-': sawDash = true
  result = sawDash

proc splitRow(s: string): seq[string] =
  ## Split a `| a | b | c |` row into trimmed cells. Leading/trailing `|`
  ## are tolerated (and stripped).
  result = @[]
  var line = s
  if line.startsWith("|"): line = line[1 .. ^1]
  if line.endsWith("|"):   line = line[0 ..< line.len - 1]
  for part in line.split('|'):
    result.add part.strip()

proc renderHeader(b: var HtmlBuilder; line: string) =
  var level = 0
  while level < line.len and line[level] == '#': inc level
  if level > 6: level = 6
  let title =
    if level < line.len and line[level] == ' ': line[level+1 .. ^1].strip()
    else: line[level .. ^1].strip()
  emitTag(b, "h" & $level):
    renderInline(b, title)

proc renderMarkdown*(b: var HtmlBuilder; src: string) =
  ## Parse `src` as Markdown and stream the result into `b`. Stateless across
  ## calls — re-entrant.
  if src.len == 0: return
  let lines = src.splitLines()
  var i = 0
  while i < lines.len:
    let ln = lines[i]
    if ln.len == 0:
      inc i
      continue
    if isFence(ln):
      let lang = ln[3 .. ^1].strip()
      inc i
      let codeStart = i
      while i < lines.len and not isFence(lines[i]):
        inc i
      let code = lines[codeStart ..< i].join("\n")
      if i < lines.len: inc i  # consume closing fence
      renderCode(b, lang, code)
    elif isHeader(ln):
      renderHeader(b, ln)
      inc i
    elif isBullet(ln):
      emitTag(b, "ul"):
        while i < lines.len and isBullet(lines[i]):
          emitTag(b, "li"):
            renderInline(b, lines[i][2 .. ^1])
          inc i
    elif ln.startsWith("|") and i + 1 < lines.len and isTableSeparator(lines[i+1]):
      let header = splitRow(ln)
      inc i  # past header
      inc i  # past separator
      emitTag(b, "table"):
        emitTag(b, "thead"):
          emitTag(b, "tr"):
            for cell in header:
              emitTag(b, "th"):
                renderInline(b, cell)
        emitTag(b, "tbody"):
          while i < lines.len and lines[i].startsWith("|"):
            emitTag(b, "tr"):
              for cell in splitRow(lines[i]):
                emitTag(b, "td"):
                  renderInline(b, cell)
            inc i
    else:
      # paragraph: gather contiguous non-blank, non-block-start lines
      var para = ln
      inc i
      while i < lines.len and lines[i].len > 0 and
            not isFence(lines[i]) and not isHeader(lines[i]) and
            not isBullet(lines[i]) and not lines[i].startsWith("|"):
        para.add ' '
        para.add lines[i]
        inc i
      emitTag(b, "p"):
        renderInline(b, para)
