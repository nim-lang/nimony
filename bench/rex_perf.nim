## Measure the runtime matcher, ASCII search, or pattern parser:
##
##   bin/nimony c -d:release --out:/tmp/rex-capture bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchLongCapture --out:/tmp/rex-long bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchSearch --out:/tmp/rex-search bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchUnicodeSearch --out:/tmp/rex-utf8 bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchUnicodeWide --out:/tmp/rex-wide bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchParse --out:/tmp/rex-parse bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchGenerate --out:/tmp/rex-gen bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchCompile --out:/tmp/rex-compile bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchCaptures --out:/tmp/rex-caps bench/rex_perf.nim
##   valgrind --tool=cachegrind --vgdb=no /tmp/rex-capture

import std / [rex, syncio, assertions]
import std / private / regexcore

const smoke = defined(benchSmoke)

when defined(benchCompile):
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 300):
    var r = default(Regex)
    var err = ""
    assert tryRe("[a-z0-9]+=(?:[a-z]+|[0-9]+)*", r, err)
    total += r.code.len
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
