## Measure the runtime matcher, searching, or building automata:
##
##   bin/nimony c -d:release --out:/tmp/rex-capture bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchLongCapture --out:/tmp/rex-long bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchSearch --out:/tmp/rex-search bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchUnicodeSearch --out:/tmp/rex-utf8 bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchUnicodeWide --out:/tmp/rex-wide bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchParse --out:/tmp/rex-parse bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchGenerate --out:/tmp/rex-gen bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchCompile --out:/tmp/rex-compile bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchLexer --out:/tmp/rex-lexer bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchCaptures --out:/tmp/rex-caps bench/rex_perf.nim
##   valgrind --tool=cachegrind --vgdb=no /tmp/rex-capture

import std / [rex, syncio, assertions]
import std / private / regexcore

const smoke = defined(benchSmoke)

when defined(benchFailedSearch) or defined(benchShortSearch):
  # Failed tries and successful short matches both used to rescan each suffix.
  let r = when defined(benchFailedSearch): re"\w+@\w+" else: re"a|a*b"
  var s = ""
  for i in 0 ..< (when smoke: 40 else: 8000): s.add 'a'
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 100):
    for (first, last) in findAll(s, r): total += last - first + 1
  echo total
elif defined(benchCompile):
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 300):
    var r = default(Regex)
    var err = ""
    assert tryRe("[a-z0-9]+=(?:[a-z]+|[0-9]+)*", r, err)
    total += r.code.len
  echo total
elif defined(benchLexer):
  # The rule sets of nifler2's three `lex` blocks (src/nifler2/nimlexer.nim),
  # built the way the `lex` plugin builds them.
  const suffixRules = [
    r"'? [fF] (32)?",
    r"'? ([dD] | [fF] 64)",
    r"'? [fF] 128",
    r"'? [iI] 8",
    r"'? [iI] 16",
    r"'? [iI] 32",
    r"'? [iI] 64",
    r"'? [uU]",
    r"'? [uU] 8",
    r"'? [uU] 16",
    r"'? [uU] 32",
    r"'? [uU] 64",
    r"' [a-zA-Z0-9\0128-\0255] [a-zA-Z0-9_\0128-\0255]*",
    r"[fFdDiIuU] [a-zA-Z0-9_\0128-\0255]*",
  ]
  const numberRules = [
    r"-? 0 [xX] ([0-9a-fA-F](_?[0-9a-fA-F])*)?",
    r"-? 0 [ocC] ([0-7](_?[0-7])*)?",
    r"-? 0 [bB] ([01](_?[01])*)?",
    r"-? [0-9](_?[0-9])* (\. [0-9](_?[0-9])*)? [eE][+-]? [0-9](_?[0-9])*",
    r"-? [0-9](_?[0-9])* \. [0-9](_?[0-9])*",
    r"-? [0-9](_?[0-9])*",
  ]
  const symbolRules = [
    r"addr",
    r"and",
    r"as",
    r"asm",
    r"bind",
    r"block",
    r"break",
    r"case",
    r"cast",
    r"concept",
    r"const",
    r"continue",
    r"converter",
    r"defer",
    r"discard",
    r"distinct",
    r"div",
    r"do",
    r"elif",
    r"else",
    r"end",
    r"enum",
    r"except",
    r"export",
    r"finally",
    r"for",
    r"from",
    r"func",
    r"if",
    r"import",
    r"in",
    r"include",
    r"interface",
    r"is",
    r"isnot",
    r"iterator",
    r"let",
    r"macro",
    r"method",
    r"mixin",
    r"mod",
    r"nil",
    r"not",
    r"notin",
    r"object",
    r"of",
    r"or",
    r"out",
    r"proc",
    r"ptr",
    r"raise",
    r"ref",
    r"return",
    r"shl",
    r"shr",
    r"static",
    r"template",
    r"try",
    r"tuple",
    r"type",
    r"using",
    r"var",
    r"when",
    r"while",
    r"xor",
    r"yield",
    r"[a-zA-Z\0128-\0255](_?[a-zA-Z0-9\0128-\0255])*",
  ]
  var total = 0
  for i in 0 ..< (when smoke: 1 else: 20):
    for rules in [@suffixRules, @numberRules, @symbolRules]:
      var dfa = default(Dfa)
      var err = ""
      rulesToDfa(rules, {reExtended, reNoCaptures, reNoBackrefs}, dfa, err)
      assert err.len == 0
      total += dfa.stateCount
  echo total
elif defined(benchGenerate):
  var dfa = default(Dfa)
  var err = ""
  regexToDfa("[a-z0-9]+=(?:[a-z]+|[0-9]+)*", {reExtended, reNoCaptures}, dfa, err)
  assert err.len == 0
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 20000):
    var r = emptyRegex()
    genBytecode(dfa, r)
    total += r.code.len
  echo total
elif defined(benchCaptures):
  var r = default(Regex)
  var err = ""
  assert tryRe("(a+)", r, err)
  assert r.capCode.len > 0
  var s = ""
  for i in 0 ..< (when smoke: 40 else: 4000): s.add 'a'
  var caps: seq[Capture] = @[]
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 1500):
    total += matchLen(s, r, caps)
  echo total, " ", caps.len
elif defined(benchParse):
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 30000):
    var err = ""
    let node = parseRegExpr("(?:abcdefghij)+=(abc|def)*[0-9]", {reExtended}, err)
    assert err.len == 0
    total += ord(node.kind)
  echo total
elif defined(benchSearch) or defined(benchUnicodeSearch) or defined(benchUnicodeWide):
  var r = default(Regex)
  var err = ""
  assert tryRe("z", r, err)
  var s = ""
  when defined(benchUnicodeWide):
    for i in 0 ..< (when smoke: 10 else: 1000): s.add "🙂"
  elif defined(benchUnicodeSearch):
    for i in 0 ..< (when smoke: 20 else: 2000): s.add "é"
  else:
    for i in 0 ..< (when smoke: 40 else: 4000): s.add 'a'
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 2500): total += find(s, r)
  echo total
else:
  var r = default(Regex)
  var err = ""
  var s = ""
  when defined(benchLongCapture):
    assert tryRe("(abcdefghijklmnop)\\1+(?:b\\1c)?", r, err)
    for i in 0 ..< (when smoke: 2 else: 250): s.add "abcdefghijklmnop"
  else:
    assert tryRe("(a)\\1+(?:b\\1c)?", r, err)
    for i in 0 ..< (when smoke: 40 else: 4000): s.add 'a'
  var caps: seq[Capture] = @[]
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 10000):
    total += matchLen(s, r, caps)
  echo total, " ", caps.len
