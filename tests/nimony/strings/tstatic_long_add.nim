## Minimal repro of the static-long-string append bug that breaks bootstrap on Windows.
##
## A string literal whose length > PayloadSize (14) is represented as a "static long"
## string: slen = 254, `more` points to a const LongString. When such a string is
## first mutated via `add`, `ensureUniqueLong`'s else branch (sl != HeapSlen) copies
## from `inlinePtr(s)` (the 7-byte inline cache) instead of `addr s.more.data[0]`.
##
## Result: bytes 0..6 of the new heap block are correct (the inline cache);
## bytes 7..oldLen-1 are read past the inline cache into the `more` pointer's bytes
## and beyond — garbage that happens to look like a pointer on Windows.

import std / syncio

proc toHex2(b: byte): string =
  const Hex = "0123456789abcdef"
  result = ""
  result.add Hex[int(b shr 4)]
  result.add Hex[int(b and 0xF)]

proc dumpHex(s: string) =
  for i in 0 ..< s.len:
    write(stdout, toHex2(byte(s[i])))
    write(stdout, " ")
  write(stdout, "\n")

proc check() =
  # Literal length 16 → static long string (> PayloadSize=14).
  var content = "(.nif27)\n(index\n"
  let expected = "(.nif27)\n(index\n"

  echo "before add, len=", content.len
  echo "first 16 bytes (should be \"(.nif27)\\n(index\\n\"):"
  dumpHex(content)

  # Trigger the static-long → heap promotion. Any add path works.
  content.add "X"

  echo "after add, len=", content.len
  echo "first 16 bytes:"
  for i in 0 ..< 16:
    write(stdout, toHex2(byte(content[i])))
    write(stdout, " ")
  write(stdout, "\n")

  # The first 16 chars should still equal the original literal.
  var bad = 0
  for i in 0 ..< expected.len:
    if content[i] != expected[i]:
      echo "MISMATCH at byte ", i,
           ": expected ", toHex2(byte(expected[i])),
           " got ", toHex2(byte(content[i]))
      inc bad
  if bad == 0:
    echo "OK"
  else:
    echo bad, " mismatched bytes — bug reproduced"
    quit 1

check()
