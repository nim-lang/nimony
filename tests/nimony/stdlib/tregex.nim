## `std/rex`'s run-time surface: `re` (compiled while this test is compiled),
## `tryRe` (compiled while it runs) and everything that matches against them.
## The assertions marked "lexim" are that project's own test suite, kept
## verbatim so the port stays honest about what it does and does not match.

import std / [syncio, assertions, rex]

proc basics =
  # lexim's tests.nim; two of its patterns are ambiguous
  assert match("(a b c)", re(r"\( .* \)", {reExtended, reAllowEvil}))
  assert match("while", re"while")
  assert match("0158787", re"\d+")
  assert match("ABC 0232", re"\w+\s+\d+")
  assert match("ABC", re(r"\d+ | \w+", {reExtended, reAllowEvil}))
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
  # `find` skips to bytes that can start a match; it must agree with trying
  # every position, including for patterns that can match without a byte.
  for pattern in ["\\d+", "é|z", "x*", "\\bab", "^a", "a$", "(a)\\1", "[^a]",
                  "🙂+", "\\s", "0x\\d", "é", "\\0255"]:
    var r = default(Regex)
    var err = ""
    assert tryRe(pattern, r, err)
    for subject in ["", "a", "xab ab", "é🙂aa z", "aa1", "\xC0\x80a",
                    "0123456789abcdef0x7", "aaaaaaaaaaaaaaaaaaaaaaé",
                    "x0x0x0x0x0x0x0x0x9a", "\xFF0xxxxxxxxxxxxxx\xFF"]:
      var slow = -1
      for i in 0 .. subject.len:
        if matchLen(subject, r, i) >= 0:
          slow = i
          break
      assert find(subject, r) == slow
  echo "searching ok"

proc rewriting =
  assert replace("a1b22c333d", re"\d+", "#") == "a#b#c#d"
  let parts = split("a1b22c333d", re"\d+")
  assert parts.len == 4
  assert parts[0] == "a" and parts[3] == "d"
  assert replace("é1é22é", re"\d+", "#") == "é#é#é"
  assert replace("abc", re"x*", "-") == "abc"
  assert split("é1é22", re"\d+") == @["é", "é", ""]
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

proc captureBounds =
  ## The match is decided by the capture-free automaton and the groups are
  ## located afterwards, following every way the automaton could read the
  ## match. A group next to something of variable length is where a DFA that
  ## takes the capture marker eagerly loses the match (upstream lexim does).
  var caps: seq[Capture] = @[]
  assert matchLen("key = value", re"(\w+) \s* = \s* (\w+)", caps) == 11
  assert caps.len == 2
  assert capture("key = value", caps, 0) == "key"
  assert capture("key = value", caps, 1) == "value"
  assert matchLen("key = value", re"(\w+) \  = \  (\w+)", caps) == 11
  assert capture("key = value", caps, 1) == "value"
  # a group that took no part in the match is there, and empty
  assert matchLen("xyz!", re"(abc)|(xyz)", caps) == 3
  assert caps.len == 2
  assert capture("xyz!", caps, 0) == ""
  assert capture("xyz!", caps, 1) == "xyz"
  assert matchLen("aab", re"(a*)(b)?", caps) == 3
  assert capture("aab", caps, 0) == "aa" and capture("aab", caps, 1) == "b"
  assert matchLen("aa", re"(a*)(b)?", caps) == 2
  assert capture("aa", caps, 0) == "aa" and capture("aa", caps, 1) == ""
  # `findBounds` reports the groups of the match it found
  assert findBounds("-- k=v --", re"(\w+)=(\w+)", caps) == (3, 5)
  assert capture("-- k=v --", caps, 0) == "k"
  # `caps` is cleared per match, never appended to
  assert matchLen("ab", re"[a-z]+", caps) == 2
  assert caps.len == 0
  echo "capture bounds ok"

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

  # "the 9th character from the end is an `a`" has no automaton smaller than
  # 512 states. It is not deterministic either, and that is no coincidence.
  let ninth = "(?:a|b)* a (?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)"
  assert not tryRe(ninth, r, err)
  assert isEmpty(r)
  assert tryRe(ninth, r, err, {reExtended, reAllowEvil})
  assert fullMatch("abbbbbbbb", r)
  assert fullMatch("babababbbb", r)
  assert not fullMatch("bbbbbbbbb", r)
  assert not fullMatch("abbbbbbbbb", r)
  echo "runtime patterns ok"

proc literalSearch() =
  # adjacent loops are separate loops: `a*b*` does not read an `a` after a `b`
  assert matchLen("ababc", re"a*b*c") == -1
  assert matchLen("aabbc", re"a*b*c") == 5
  assert findBounds("ababc abc", re"a*b*c") == (2, 4)

  # a search finds the literal first and only then runs the automaton; the
  # result must be the one the per-position search gives
  proc perPosition(s: string; r: Regex): (int, int) =
    var i = 0
    while i <= s.len:
      let L = matchLen(s, r, i)
      if L >= 0: return (i, i + L - 1)
      inc i
    result = (-1, 0)
  let pats = @[re"\w+@\w+", re"a+b", re"[a-z]{1,3}@", re"(\w+)\s*=\s*(\w+)",
               re(r"x\w*y", {reExtended, reAllowEvil}), re"\d\d-\d+", re"(?:ab)+c", re"\bfoo\b", re"a*b*c",
               re"[^,]*,x", re"ab|abc", re"\w+(@)x", re"\s*=", re"é+x", re"é\w+",
               re"\w+\s+0", re(r"(?:a|ab)*c", {reExtended, reAllowEvil}),
               re(r"\w+@.*c$", {reExtended, reAllowEvil}),
               re(r"a|a*b", {reExtended, reAllowEvil}),
               re(r".*x", {reExtended, reAllowEvil}), re"if|in|int|i",
               re(r"(?:\bab|a)c", {reExtended, reAllowEvil}), re"^a\w*"]
  let subjects = @["", "a", "abc", "aaaab", "x@y@z", "foo bar foo",
    "aa ab@ abcd@e", "k = v", "xyy xzy", "12-345 6-7", "ababc abc", "aabbcc c",
    ",x a,,x", "abcabc", "a@x b@@x", "  =", "ééx éx", "aaaa!@x", "foofoo foo.",
    "éa é", "ab 0 cd  0", "ababbc", "a@a@c a@c", "aab", "intx in if",
    "xabc abc", "abac"]
  for p in pats:
    for s in subjects:
      assert findBounds(s, p) == perPosition(s, p)
  assert find("aaaa!@x", re"\w+@x") == -1
  var n = 0
  for (a, b) in findAll("a@b c@d", re"\w@\w"): inc n
  assert n == 2
  assert replace("k=v, k2 = v2", re"\w+\s*=\s*\w+", "X") == "X, X"
  assert split("a;b;;c", re";") == @["a", "b", "", "c"]
  echo "literal search ok"

proc determinism() =
  # a pattern from outside the program is checked like a literal one
  var r = default(Regex)
  var err = ""
  for bad in [r".*x", r"\d+|\w+", r"a|a*b", r"x\w*y", r"\d+\.?\d*",
              "\\\".*\\\""]:
    assert not tryRe(bad, r, err)
    assert err.len > 0
    assert isEmpty(r)
    assert tryRe(bad, r, err, {reExtended, reAllowEvil})
  for good in [r"[^x]*x", r"\w+", r"a(?:a*b)?|b", r"\d+(?:\.\d*)?",
               "(\\\"[^\"]*\\\")", r"(?:a|ab)*c",
               r"\w+\s*=\s*\w+", r"^\d+$", r"if|in|int|i", r"ab|abc|abd",
               r"(?:ab)*c", r"\d{1,3}(?:\.\d{1,3}){3}"]:
    assert tryRe(good, r, err)
  assert not tryRe(".*x", r, err)
  assert err == "ambiguous regular expression: at the start, the character " &
    "`x` could belong to either `.` or `x`. Rewrite the pattern so that the " &
    "next character always decides (`[^x]*x` rather than `.*x`), or add " &
    "`reAllowEvil` to the flags (keeping `reExtended`)"
  # left factoring keeps the language
  assert fullMatch("int", re"if|in|int") and fullMatch("in", re"if|in|int")
  assert not fullMatch("i", re"if|in|int")
  assert matchLen("abcd", re"ab|abc") == 3
  # `^` is the start of the subject, not of the search
  assert replace("aaa", re"^a", "-") == "-aa"
  assert matchLen("xab", re"^ab", 1) == -1
  # `\b` looks at both sides
  assert find("xfoo foo", re"\bfoo\b") == 5
  assert matchLen("xfoo", re"\bfoo", 1) == -1
  assert matchLen("x foo", re"\bfoo", 2) == 3
  echo "determinism ok"

basics()
anchoring()
searching()
rewriting()
capturing()
captureBounds()
characterClasses()
runtimePatterns()
literalSearch()
determinism()
