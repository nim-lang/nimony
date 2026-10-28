import std / [assertions, rex]

block literals:
  assert fullMatch("éé", re"é+"), "literal"
  assert fullMatch("🙂🙂", re"🙂{2}"), "repeat"
  assert matchLen("é🙂!", re"..") == 6, "dot"
  assert not fullMatch("\xC3", re"."), "truncated"
  assert not fullMatch("\xED\xA0\x80", re"."), "surrogate"
  assert matchLen("ababx", re"(?:ab)+") == 4, "longest"
  assert matchLen("abax", re"(?:ab)+") == 2, "rewind"
  assert fullMatch("\x7F\xC2\x80\xDF\xBF\xE0\xA0\x80" &
                   "\xED\x9F\xBF\xEE\x80\x80\xF4\x8F\xBF\xBF", re".+")
  assert not fullMatch("\xF4\x90\x80\x80", re".")
  assert not fullMatch("\xC0\x80", re".")

block classes:
  assert fullMatch("αβω", re"[α-ω]+"), "range"
  assert not fullMatch("αЖ", re"[α-ω]+"), "range exclusion"
  assert fullMatch("é🙂", re"[^a]+"), "negated"
  assert not fullMatch("éa", re"[^a]+"), "negated exclusion"
  assert fullMatch("é🙂", re"\w+"), "word class"
  assert fullMatch("🙂", re"\D"), "complement class"
  assert not fullMatch("\x80", re"\D"), "invalid class input"
  assert fullMatch("zéα🙂", re"[a-🙂]+"), "range across UTF-8 widths"
  assert not fullMatch("😀", re"[a-ω]"), "range upper bound"
  assert not fullMatch("é", re"[ê-ω]"), "range lower bound"

block positions:
  var caps: seq[Capture] = @[]
  assert matchLen("é🙂!", re"(é🙂)", caps) == 6
  assert capture("é🙂!", caps, 0) == "é🙂"
  assert find("é🙂x", re"x") == 6
  var offsets: seq[int] = @[]
  for (a, _) in findAll("é🙂", re""):
    offsets.add a
  assert offsets == @[0, 2, 6]

block runtime:
  var r = default(Regex)
  var err = ""
  assert tryRe("é+", r, err), "runtime compile"
  assert matchLen("éé!", r) == 4, "runtime match"
  assert not tryRe("\xC3", r, err), "invalid pattern"
  assert err.len > 0

block generated:
  let s = "é🙂!"
  var pos = 0
  lex s, pos:
  of "é.": discard
  assert pos == 6, "lex"
  rematch "αβ":
  of "[α-ω]+": discard
  else: assert false
