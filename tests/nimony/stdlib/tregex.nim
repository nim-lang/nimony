## `std/rex`'s run-time surface: `re` (compiled while this test is compiled),
## `tryRe` (compiled while it runs) and everything that matches against them.
## The assertions marked "lexim" are that project's own test suite, kept
## verbatim so the port stays honest about what it does and does not match.

import std / [syncio, assertions, rex]

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

  # "the 9th character from the end is an `a`" has no automaton smaller than
  # 512 states
  assert tryRe("(?:a|b)* a (?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)(?:a|b)",
               r, err)
  assert fullMatch("abbbbbbbb", r)
  assert fullMatch("babababbbb", r)
  assert not fullMatch("bbbbbbbbb", r)
  assert not fullMatch("abbbbbbbbb", r)
  echo "runtime patterns ok"

basics()
anchoring()
searching()
rewriting()
capturing()
captureDegradation()
characterClasses()
runtimePatterns()

proc slowFindAll(s: string; r: Regex; start: int): seq[(int, int)] =
  ## `findAll` as it tries a match at every position.
  result = @[]
  var i = start
  while i <= s.len:
    let L = matchLen(s, r, i)
    if L > 0:
      result.add (i, i + L - 1)
      inc i, L
    elif L == 0:
      result.add (i, i - 1)
      inc i
    else:
      inc i

proc slowReplace(s: string; r: Regex; by: string): string =
  result = ""
  var i = 0
  while i < s.len:
    let L = matchLen(s, r, i)
    if L > 0:
      result.add by
      inc i, L
    else:
      result.add s[i]
      inc i

proc slowSplit(s: string; r: Regex): seq[string] =
  result = @[]
  var piece = ""
  var i = 0
  while i < s.len:
    let L = matchLen(s, r, i)
    if L > 0:
      result.add piece
      piece = ""
      inc i, L
    else:
      piece.add s[i]
      inc i
  result.add piece

proc memoSearch =
  # Compare byte-by-byte anchored matching with search-local memoization.
  var long1 = ""
  for i in 0 ..< 300: long1.add char(ord('a') + i mod 26)
  var long2 = long1 & " x@y " & long1 & "@" & long1 & " é@🙂 "
  var long3 = ""
  for i in 0 ..< 40: long3.add "abcabcd ééé\xC3 12 x@ @y "
  var long4 = ""
  for i in 0 ..< 200: long4.add "é"
  long4.add "\xFF\x80aé@é"
  var long5 = ""
  for i in 0 ..< 300: long5.add 'a'
  for pattern in ["abcd|c", "(?:abcd|c)d*", "\\w+@\\w+", "x*", "\\bab\\w*",
                  "\\w+$", "a*$", "(a)\\1+", "é+@?", "[^@]+@", "(?:\\w+@\\w+)?",
                  "a.*z", "(?:a|b)*c", "\\d+|\\w+@", "$", "(\\w+)@(\\w+)",
                  "\\s*$", "c?d*$|x", "[a-z]+ \\ x", "🙂+|é+", "(?:abc)+d", "\\w*",
                  "^\\w+", "[^\\0]+\\Z", "\\w+\\B", "a|a*b", "(?:a|ab)*c", ".*x", "(a|a*b)",
                  "a+b|a{20}z", "(?:ab)+c", "a+b*c", "a+b*c+d",
                  "a+X|b+Y", "a{64}b", "(?:ab|a)+c", "(?:a+b?)+X"]:
    var r = default(Regex)
    var err = ""
    assert tryRe(pattern, r, err)
    for subject in ["", "abcd", "abcabcd", "xab ab", "é🙂aa z", "\xC0\x80a",
                    "a@b", long1, long2, long3, long4, long4 & long1, long5]:
      for start in [0, 1, 2, 7, 150, 301]:
        if start > subject.len: continue
        let slow = slowFindAll(subject, r, start)
        var fast: seq[(int, int)] = @[]
        for m in findAll(subject, r, start): fast.add m
        assert fast == slow
        let (a, b) = findBounds(subject, r, start)
        if slow.len == 0: assert a == -1 and b == 0
        else: assert (a, b) == slow[0]
      var caps: seq[Capture] = @[]
      let bounds = findBounds(subject, r, caps)
      if bounds[0] >= 0:
        var expected: seq[Capture] = @[]
        assert matchLen(subject, r, expected, bounds[0]) == bounds[1] - bounds[0] + 1
        assert caps.len == expected.len
        for i in 0 ..< caps.len:
          assert caps[i].first == expected[i].first and caps[i].last == expected[i].last
      assert replace(subject, r, "<>") == slowReplace(subject, r, "<>")
      assert split(subject, r) == slowSplit(subject, r)
  # Compile-time and runtime regex construction share search behavior.
  let lit = re"\w+@\w+"
  assert findBounds(long2, lit) == (301, 303)
  # captures come from the forward run at the start that was found
  var caps: seq[Capture] = @[]
  let (a, b) = findBounds(long2, re"(\w+)@(\w+)", caps)
  assert (a, b) == (301, 303)
  assert capture(long2, caps, 0) == "x" and capture(long2, caps, 1) == "y"
  # More than 64 nonaccepting cyclic states times one million positions
  # exceeds the 8 MiB bitset budget. Acyclic states no longer consume bits.
  # The first attempt overscans, so the next attempt reaches the cap fallback.
  var pattern = ""
  for i in 0 ..< 100: pattern.add "a+b"
  pattern.add "z|a"
  var large = "aaa!"
  for i in 0 ..< 1048576: large.add 'x'
  var capped = default(Regex)
  var error = ""
  assert tryRe(pattern, capped, error)
  var count = 0
  for (first, last) in findAll(large, capped):
    assert first == count and last == count
    inc count
  assert count == 3
  assert find("xxab", re"^ab") == 2
  assert findBounds("ab", re"a|ab") == (0, 1)
  assert findBounds("abcd", re"abcd|c") == (0, 3)
  echo "memo search ok"

memoSearch()
