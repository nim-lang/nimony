## Measure the runtime matcher, ASCII search, or pattern parser:
##
##   bin/nimony c -d:release --out:/tmp/rex-capture bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchSearch --out:/tmp/rex-search bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchUnicodeSearch --out:/tmp/rex-utf8 bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchUnicodeWide --out:/tmp/rex-wide bench/rex_perf.nim
##   bin/nimony c -d:release -d:benchParse --out:/tmp/rex-parse bench/rex_perf.nim
##   valgrind --tool=cachegrind --vgdb=no /tmp/rex-capture

import std / [rex, syncio, assertions]
import std / private / regexcore

const smoke = defined(benchSmoke)

when defined(benchParse):
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
  assert tryRe("(a)\\1+(?:b\\1c)?", r, err)
  var s = ""
  for i in 0 ..< (when smoke: 40 else: 4000): s.add 'a'
  var caps: seq[Capture] = @[]
  var total = 0
  for i in 0 ..< (when smoke: 3 else: 500):
    total += matchLen(s, r, caps)
  echo total, " ", caps.len
