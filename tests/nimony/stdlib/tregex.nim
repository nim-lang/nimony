## `std/regex`'s run-time surface: `re` (compiled while this test is compiled),
## `tryRe` (compiled while it runs) and everything that matches against them.
## The assertions marked "lexim" are that project's own test suite, kept
## verbatim so the port stays honest about what it does and does not match.

import std / [syncio, assertions, regex]

proc basics =
  # lexim's tests.nim
  assert match("(a b c)", re"\( .* \)")
  assert match("while", re"while")
  assert match("0158787", re"\d+")
  assert match("ABC 0232", re"\w+\s+\d+")
  assert match("ABC", re"\d+ | \w+")
  assert matchLen("key", re"\w+") == 3
  assert matchLen("key1=  cal9", re"[a-z0-9]+\s*=\s*[a-z0-9]+") == 11
  assert match("abc", re"\Aabc\Z")
  assert not match("abcdef", re"^abc$")
  assert not match("aef", re"\A(?:abc|def)\Z")
  assert match("def", re"\A(?:abc|def)\Z")
  assert not match("deffoo", re"\A(?:abc|def)\Z")
  assert not match("deffoo", re"\b(?:abc|def)\b")
  assert match("def foo", re"\b(?:abc|def)\b")
  assert matchLen("def foo", re"\b(?:abc|def)\b") == 3
  assert matchLen("def foo\C\L", re"\bdef\sfoo\n") == 9
  echo "basics ok"

proc anchoring =
  # `match` is a PREFIX match; `fullMatch` is the whole-string one.
  assert match("abcdef", re"[a-z]+")
  assert fullMatch("abcdef", re"[a-z]+")
  assert match("abc123", re"[a-z]+")
  assert not fullMatch("abc123", re"[a-z]+")
  assert matchLen("abc123", re"[a-z]+") == 3
  # matching from an offset
  assert matchLen("abc123", re"\d+", 3) == 3
  assert matchLen("abc123", re"\d+", 0) == -1
  echo "anchoring ok"

proc searching =
  assert find("hello world 42", re"\d+") == 12
  assert find("hello", re"\d+") == -1
  assert re"\d+" in "abc9"
  assert not (re"\d+" in "abc")
  let (a, b) = findBounds("a1b22c333", re"\d\d+")
  assert a == 3 and b == 4
  var found: seq[string] = @[]
  for (f, l) in findAll("a1b22c333d", re"\d+"):
    found.add substr("a1b22c333d", f, l)
  assert found.len == 3
  assert found[0] == "1" and found[1] == "22" and found[2] == "333"
  echo "searching ok"

proc rewriting =
  assert replace("a1b22c333d", re"\d+", "#") == "a#b#c#d"
  let parts = split("a1b22c333d", re"\d+")
  assert parts.len == 4
  assert parts[0] == "a" and parts[3] == "d"
  echo "rewriting ok"

proc capturing =
  var caps: seq[Capture] = @[]
  assert matchLen("key=value", re"(\w+)=(\w+)", caps) == 9
  assert caps.len == 2
  assert capture("key=value", caps, 0) == "key"
  assert capture("key=value", caps, 1) == "value"
  assert captureTexts("key=value", caps).len == 2
  # a back reference has to repeat what the group captured
  assert matchLen("'haha'", re"(\`|\')[^`']*\1") == 6
  assert matchLen("aaaaabcc", re"((a+)b(c)\2)") == 8
  echo "capturing ok"

proc captureDegradation =
  ## The match is decided by the capture-free automaton, so a group next to
  ## something of variable length still MATCHES — upstream lexim returns "no
  ## match" for exactly this pattern. The bounds are what is missing, and
  ## `capture` says so by returning "".
  var caps: seq[Capture] = @[]
  assert matchLen("key = value", re"(\w+) \s* = \s* (\w+)", caps) == 11
  assert caps.len == 0
  assert capture("key = value", caps, 0) == ""
  # a fixed-shape neighbourhood keeps the bounds
  assert matchLen("key = value", re"(\w+) \  = \  (\w+)", caps) == 11
  assert caps.len == 2
  assert capture("key = value", caps, 0) == "key"
  assert capture("key = value", caps, 1) == "value"
  # `caps` is cleared per match, never appended to
  assert matchLen("ab", re"[a-z]+", caps) == 2
  assert caps.len == 0
  echo "capture degradation ok"

proc characterClasses =
  # `\n` inside a class is the line feed alone; outside it, it is a newline in
  # any of its three spellings
  assert fullMatch("abc", re"[^\n]+")
  assert not fullMatch("a\Lc", re"[^\n]+")
  assert matchLen("\C\L", re"\n") == 2
  assert matchLen("\L", re"\n") == 1
  # a negated class never contains '\0', so it cannot run off the end
  assert matchLen("ab", re"[^x]+") == 2
  echo "character classes ok"

proc runtimePatterns =
  # A pattern that is only known at run time goes through `tryRe`, which hands
  # back the diagnostic rather than failing the build.
  var r = default(Regex)
  var err = ""
  assert not tryRe("[a-z", r, err)
  assert err.len > 0
  assert isEmpty(r)
  assert not match("a", r)

  assert tryRe("[a-z]+", r, err)
  assert err == ""
  assert not isEmpty(r)
  assert fullMatch("abc", r)
  assert captureCount(r) == 0

  # `reExtended` is the default; without it a space is a literal space
  assert tryRe("a b", r, err, {})
  assert fullMatch("a b", r)
  assert tryRe("a b", r, err)
  assert fullMatch("ab", r)
  echo "runtime patterns ok"

basics()
anchoring()
searching()
rewriting()
capturing()
captureDegradation()
characterClasses()
runtimePatterns()
