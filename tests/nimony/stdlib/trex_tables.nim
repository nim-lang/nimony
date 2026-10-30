## Differential test: the instruction VM is the reference for the table
## executor, including its historical attempt-relative assertion semantics.
import std/[assertions, rex, syncio]
import std/private/regexcore

proc reference(s: string; r: Regex; start: int): (int, int) =
  for i in start .. s.len:
    let n = matchLen(s, r, i)
    if n >= 0: return (i, i + n - 1)
  result = (-1, 0)

proc compare(pattern, s: string; compiled, vm: Regex) =
  for start in 0 .. s.len + 1:
    let actualLength = matchLen(s, compiled, start)
    let expectedLength = matchLen(s, vm, start)
    let actual = findBounds(s, compiled, start)
    let expected = reference(s, vm, start)
    if actualLength != expectedLength or actual != expected:
      echo "pattern: ", pattern, " start: ", start, " input: ", s
      echo "match: ", actualLength, " vs ", expectedLength
      echo "search: ", actual[0], ":", actual[1], " vs ", expected[0], ":", expected[1]
    assert actualLength == expectedLength
    assert actual == expected

for pattern in ["", "a", "ab|a", "a|ab", "a*b", "a|a*b", "(?:a|ab)*c",
                "[ab]*[12]", "[ab]+[12]+", "[^a]+", ".", "..", ".+",
                "ä+", "[ä🙂]+", "[α-ω]", r"\w+", r"\W", r"\d+\w",
                r"^\d+$", r"a$|ab", r"a^|ab", r"\b[ab]+\b", r"a\B",
                r"\Ba", r"a\b|ab", r"(?:a$|b)+", r"a?^b", r"a?\bb",
                r"^a|b$", r"(?:ab)?", r"[\0128-\0255]+"]:
  var compiled = emptyRegex()
  var vm = emptyRegex()
  var dfa = default(Dfa)
  var err = ""
  assert tryRe(pattern, compiled, err)
  assert compiled.transitions.len > 0
  regexToDfa(pattern, {reExtended, reNoCaptures}, dfa, err)
  assert err.len == 0
  genBytecode(dfa, vm)
  # Enumerate every string of up to four bytes over this mixed alphabet.
  let alphabet = "ab1\0\xC3\xA4\xE4"
  var count = 1
  for length in 0 .. 4:
    for value in 0 ..< count:
      var n = value
      var s = ""
      for i in 0 ..< length:
        s.add alphabet[n mod alphabet.len]
        n = n div alphabet.len
      compare(pattern, s, compiled, vm)
    count *= alphabet.len
  for s in ["ä\xE4🙂\xFF", "\xC0\x80", "\xED\xA0\x80", "\xE2\x82",
            "\xF4\x90\x80\x80", "abcabc", "aab12", "aaaa!", "1111!", "!111"]:
    compare(pattern, s, compiled, vm)

block captures:
  let r = re"(ä🙂)"
  var caps: seq[Capture] = @[]
  assert findBounds("!\xE4🙂?", r, caps) == (1, 5)
  assert capture("!\xE4🙂?", caps, 0) == "\xE4🙂"

block literalPlugin:
  assert findBounds("1111!22", re"^\d+$") == (5, 6)
  assert findBounds("abababbc", re"(?:a|ab)*c") == (7, 7)
  assert findBounds("!ä\xE4🙂", re"[ä🙂]+") == (1, 7)
  assert findBounds("ab", re"a|ab") == (0, 1)
  assert findBounds("ab", re"a*|b") == (0, 0)
