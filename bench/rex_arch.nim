## Architecture benchmark: build with -d:release and optionally -d:benchSmoke.
## Variants: archMixed, archSearch, archPrefix, archAbsent, archLate,
## archNoLiteral, archCompile. Use a distinct --nimcache per variant.
## Measurements and architectural trade-offs: doc/internals/regex_tables.md.
import std/[rex, syncio, assertions]
const smoke = defined(benchSmoke)
var total = 0
when defined(archCompile):
  for i in 0 ..< (when smoke: 1 else: 200):
    for p in [r"[a-z0-9]+=(?:[a-z]+|[0-9]+)*", r"[α-ω]+", r".+", r"^\d+$"]:
      var r = default(Regex)
      var err = ""
      assert tryRe(p, r, err, {reExtended, reUnanchored})
      total += matchLen("abc=def", r)
else:
  var s = ""
  when defined(archLate):
    for i in 0 ..< (when smoke: 20 else: 2000): s.add "ab"
    s.add "bc"
    let p = r"(?:a|ab)*c"
  elif defined(archNoLiteral):
    for i in 0 ..< (when smoke: 20 else: 4000): s.add '1'
    s.add '!'
    let p = r"^\d+$"
  elif defined(archMixed):
    for i in 0 ..< (when smoke: 20 else: 4000): s.add "ä\xE4🙂\xFF"
    let p = r"[ä🙂\0255]+"
  elif defined(archPrefix):
    for i in 0 ..< (when smoke: 20 else: 4000): s.add "abc"
    s.add "z"
    let p = "z"
  elif defined(archAbsent):
    for i in 0 ..< (when smoke: 20 else: 4000): s.add "abc"
    let p = r"\w+@\w+"
  else:
    for i in 0 ..< (when smoke: 20 else: 4000): s.add "abc123"
    let p = r"[a-z0-9]+"
  var r = default(Regex)
  var err = ""
  assert tryRe(p, r, err, {reExtended, reUnanchored})
  for i in 0 ..< (when smoke: 2 elif defined(archLate) or defined(archNoLiteral): 5 else: 200):
    when defined(archLate) or defined(archNoLiteral) or defined(archAbsent) or defined(archPrefix) or defined(archSearch):
      total += find(s, r)
    else:
      total += matchLen(s, r)
echo total
