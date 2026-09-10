## `std/regex`'s compile-time constructs: `lex` (scan and advance) and
## `rematch` (classify a whole string). Both are the `deps/regex` plugin
## turning a set of patterns into ONE automaton and emitting it as a `case`
## statement, so what runs here contains no regex engine at all.

import std / [syncio, assertions, regex]

proc tokenize(input: string): seq[string] =
  ## The lexim README's example, as a lexer that returns what it saw.
  result = @[]
  var pos = 0
  while pos < input.len:
    let start = pos
    lex input, pos:
    of r"\d+": result.add "int(" & substr(input, start, pos-1) & ")"
    of "else": result.add "ELSE"
    of "elif": result.add "ELIF"
    of "end": result.add "END"
    of r"[a-zA-Z_]\w+": result.add "ident(" & substr(input, start, pos-1) & ")"
    of r"\s+": discard
    of r".": result.add "other(" & substr(input, start, pos-1) & ")"
    else:
      result.add "STUCK"
      break

proc scanning =
  let t = tokenize("the 0909 else input elif elseo end")
  assert t.len == 7, $t.len
  assert t[0] == "ident(the)"
  assert t[1] == "int(0909)"
  assert t[2] == "ELSE"
  assert t[3] == "ident(input)"
  assert t[4] == "ELIF"
  # maximal munch: `elseo` is longer than the `else` keyword, so it is one
  # identifier rather than a keyword followed by `o`
  assert t[5] == "ident(elseo)"
  assert t[6] == "END"
  echo "scanning ok"

proc rewinding =
  ## The automaton walks into `elsewhere`'s prefix, fails, and rewinds to the
  ## last accepting position instead of leaving `pos` stranded.
  var pos = 0
  let input = "el+"
  var seen = ""
  while pos < input.len:
    let start = pos
    lex input, pos:
    of "else": seen.add "K"
    of r"[a-z]+": seen.add "W" & substr(input, start, pos-1)
    of r".": seen.add "."
  assert seen == "Wel.", seen
  echo "rewinding ok"

proc noMatch =
  ## Nothing matches: `pos` must not move, so a caller can tell the difference
  ## between "consumed nothing" and "consumed something I ignored".
  var pos = 1
  let input = "a1"
  var ran = false
  lex input, pos:
  of r"[a-z]+": ran = true
  assert not ran
  assert pos == 1
  echo "no match ok"

proc classify(token: string): string =
  result = ""
  rematch token:
  of "if", "else", "while": result = "keyword"
  of r"\d+": result = "number"
  of r"[a-z]+": result = "word"
  of r"[A-Z][a-z]*": result = "capitalized"
  else: result = "unknown"

proc whole =
  # a branch wins only when its pattern matches ALL of the string
  assert classify("while") == "keyword"
  assert classify("whilex") == "word"
  assert classify("123") == "number"
  assert classify("Hello") == "capitalized"
  assert classify("12ab") == "unknown"
  assert classify("") == "unknown"
  echo "whole-string ok"

proc agreesWithRuntime =
  ## The two halves of the module share one implementation of what a pattern
  ## means, and this is the assertion that says so.
  let words = ["", "a", "abc", "123", "a1", "Zz", "  ", "else", "elseo"]
  for w in words:
    let byLex = classify(w)
    let byRuntime =
      if fullMatch(w, re"if|else|while"): "keyword"
      elif fullMatch(w, re"\d+"): "number"
      elif fullMatch(w, re"[a-z]+"): "word"
      elif fullMatch(w, re"[A-Z][a-z]*"): "capitalized"
      else: "unknown"
    assert byLex == byRuntime, w & ": " & byLex & " vs " & byRuntime
  echo "agreement ok"

proc lexNimish(input: string): string =
  ## A lexer of realistic size: 18 keywords plus numbers, identifiers,
  ## comments, operators and whitespace, all in ONE automaton. It is here to
  ## keep an eye on the 255-state ceiling as much as on the tokens.
  result = ""
  var pos = 0
  while pos < input.len:
    let start = pos
    lex input, pos:
    of "if", "else", "elif", "while", "for", "proc", "func", "var", "let",
       "const", "type", "return", "discard", "and", "or", "not", "in", "is":
      result.add "K"
    of r"\d+ \. \d+": result.add "F"
    of r"\d+": result.add "N"
    of r"[a-zA-Z_]\w*": result.add "I"
    of r"\# [^\n]*": result.add "C"
    of r"== | != | <= | >= | = | < | > | \+ | -": result.add "O"
    of r"\s+": discard
    of r".": result.add "."
    else:
      result.add "!"
      break

proc realisticLexer =
  # `if x == 12 and y != 3.5: proc foo() = discard # done`
  assert lexNimish("if x == 12 and y != 3.5: proc foo() = discard # done") ==
         "KIONKIOF.KI..OKC"
  # a keyword prefix is an identifier, not a keyword
  assert lexNimish("iffy if") == "IK"
  # the longest number wins
  assert lexNimish("3.5 3 . 5") == "FN.N"
  echo "realistic lexer ok"

proc hygiene =
  ## The generated machine calls `inc` and `len`. They are bound in the
  ## plugin's own scope, so shadowing them at the call site must not reach into
  ## the expansion.
  proc len(s: string): int = 99
  proc inc(x: var int) = x = x + 100

  var pos = 0
  let input = "abc"
  var got = ""
  lex input, pos:
  of r"[a-z]+": got = substr(input, 0, pos-1)
  assert got == "abc", got
  assert pos == 3
  assert len(input) == 99   # the shadowing really is in scope here
  echo "hygiene ok"

scanning()
rewinding()
hygiene()
realisticLexer()
noMatch()
whole()
agreesWithRuntime()
