## Finds UPPERCASED words in the comments of Nim sources: the shouted
## emphasis ("is NOT", "ONLY then", "EVERY module") that generated prose is
## fond of. A word counts when the same comments also use it in lowercase, as
## ordinary English; acronyms (`Acronyms`, or simply never written in
## lowercase) and code in backticks are left alone.
##
##   capswords [--stats] file.nim|dir ...
##
## Prints `file(line, col): WORD  comment` for every hit; `--stats` prints how
## often each word occurs instead, most frequent first.

import std / [syncio, strutils, tables, cmdline, paths, dirs, rex, algorithm]

const
  Acronyms = [
    "ABI", "API", "ARC", "ASCII", "AST", "BE", "BSD", "CI", "CPS", "CPU",
    "CSE", "CTFE", "DAG", "DCE", "DFA", "DLL", "DSL", "DWARF", "ELF", "EOF",
    "FFI", "FIFO", "GC", "GCC", "GNU", "HTML", "HTTP", "ID", "IC", "IDE", "IO",
    "IR", "INF", "IND", "IOCP", "ISA", "IV", "JIT", "JS", "JSON", "LE", "LHS", "LLVM", "LRU", "LTO", "MSVC",
    "NAN", "NFA", "NIF", "NIFC", "NUL", "OK", "ORC", "OS", "PE", "PID", "POSIX", "RAII",
    "RAM", "RC", "RFC", "RHS", "RTTI", "SCC", "SIMD", "SSA", "SSE", "TCP", "TLS",
    "TODO", "UB", "UDP", "URL", "UTF", "VM", "XXX", "YRC"]

const
  MinProse = 5
    ## how often the lowercase form must occur in the scanned comments for an
    ## uppercase word to count as shouted rather than an acronym

proc commentOf(line: string; inBlock, inTriple: var bool): string =
  ## The part of `line` that is comment. `inBlock` and `inTriple` carry a
  ## `#[ ]#` comment and a `"""` string over to the next line.
  result = ""
  var i = 0
  while i < line.len:
    {.assume: i >= 0.}  # starts at 0 and only grows
    if inBlock:
      let e = line.find("]#", i)
      if e < 0:
        result.add line.substr(i)
        return
      result.add line.substr(i, e - 1)
      result.add ' '
      inBlock = false
      i = e + 2
    elif inTriple:
      let e = line.find("\"\"\"", i)
      if e < 0: return
      inTriple = false
      i = e + 3
    elif line.continuesWith("\"\"\"", i):
      inTriple = true
      i += 3
    elif line[i] == '"':
      inc i
      while i < line.len and line[i] != '"':
        if line[i] == '\\': inc i
        inc i
      inc i
    elif line[i] == '\'' and i + 2 < line.len and line[i+2] == '\'':
      i += 3
    elif line[i] == '\'' and i + 3 < line.len and line[i+1] == '\\' and line[i+3] == '\'':
      i += 4
    elif line[i] == '#':
      if i + 1 < line.len and line[i+1] == '[':
        inBlock = true
        i += 2
      elif i + 2 < line.len and line[i+1] == '#' and line[i+2] == '[':
        inBlock = true
        i += 3
      else:
        result.add line.substr(i)
        return
    else:
      inc i

proc maskCode(s: var string) =
  ## Blanks out what is between backticks: code, which may be uppercase.
  var inCode = false
  for i in 0 ..< s.len:
    if s[i] == '`':
      inCode = not inCode
    elif inCode:
      s[i] = ' '

type
  Hit = object
    file, word, text: string
    line, col: int
  Scan = object
    caps, lower: Regex
    hits: seq[Hit]
    prose: Table[string, int] ## how often each lowercase word occurs in comments

proc scan(s: var Scan; file: string) =
  var inBlock = false
  var inTriple = false
  var lineNo = 0
  for line in lines(file):
    inc lineNo
    var c = commentOf(line, inBlock, inTriple)
    if c.len == 0: continue
    let offset = line.len - c.len
    maskCode c
    for b in findAll(c, s.caps):
      let w = c.substr(b[0], b[1])
      # `GRAMMAR.TXT`, `FOO.nim`: a file name
      let inFileName = (b[0] > 0 and c[b[0]-1] == '.') or
        (b[1] + 2 < c.len and c[b[1]+1] == '.' and c[b[1]+2] in {'A'..'Z', 'a'..'z'})
      if w notin Acronyms and not inFileName:
        s.hits.add Hit(file: file, word: w, text: line.strip(),
                       line: lineNo, col: offset + b[0] + 1)
    for b in findAll(c, s.lower):
      let w = c.substr(b[0], b[1])
      s.prose[w] = s.prose.getOrDefault(w, 0) + 1

proc isShouted(s: Scan; w: string): bool =
  ## An uppercase word is shouted English, not an acronym or a placeholder
  ## of a grammar, when the comments also use it as an ordinary word.
  s.prose.getOrDefault(toLowerAscii(w), 0) >= MinProse

proc main() =
  var stats = false
  var inputs: seq[string] = @[]
  for a in commandLineParams():
    if a == "--stats": stats = true
    else: inputs.add a
  if inputs.len == 0:
    quit "usage: capswords [--stats] file.nim|dir ..."
  var s = Scan(caps: re"\b[A-Z][A-Z]+\b", lower: re"\b[a-z]+\b",
               prose: initTable[string, int]())
  for x in inputs:
    if x.endsWith(".nim"):
      scan s, x
    else:
      try:
        for p in walkDirRec(path(x), checkDir = true):
          if ($p).endsWith(".nim"): scan s, $p
      except:
        quit "cannot read the directory " & x
  if stats:
    var counts = initTable[string, int]()
    for h in s.hits:
      if s.isShouted(h.word): counts[h.word] = counts.getOrDefault(h.word, 0) + 1
    var all: seq[(int, string)] = @[]
    for w, n in pairs(counts): all.add (n, w)
    sort all, proc (a, b: (int, string)): int =
      result = cmp(b[0], a[0])
      if result == 0: result = cmp(a[1], b[1])
    for x in all: echo x[0], "\t", x[1]
  else:
    for h in s.hits:
      if s.isShouted(h.word):
        echo h.file, "(", h.line, ", ", h.col, "): ", h.word, "  ", h.text

main()
