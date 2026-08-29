import std/[syncio, strutils]

# case mapping + predicates
echo toUpperAscii("hello, Ward!")     # HELLO, WARD!
echo toLowerAscii("MIXED case 42")    # mixed case 42
echo capitalizeAscii("nimony")        # Nimony
echo isDigit('7'), " ", isDigit('x')  # true false
echo isAlphaAscii('q'), " ", isAlphaAscii('9')

# search + replace
let s = "the quick brown fox jumps over the lazy dog"
echo s.find("brown")                  # 10
echo s.find('z')                      # 37
echo s.find("missing")                # -1
echo s.replace("quick", "slow")
echo s.replace('o', '0')
echo s.startsWith("the q"), " ", s.endsWith("dog")
echo s.continuesWith("brown", 10)

# split / repeat / spaces
for part in "a,b,,c".split(','):
  echo "[", part, "]"
echo "ab".repeat(3)                   # ababab
echo repeat('-', 5)                   # -----
echo "|", spaces(3), "|"

# comparisons (cmpIgnoreCase breaks the NATIVE backend's register allocator —
# see nativebugs/cmp_ignore_case.nim)
echo normalize("Foo_Bar")             # foobar

# escape, which is `.raises`-free, still round-trips through this file. The
# `try`/`except` that `parseInt`/`unescape` need does not compile on the WASM
# leg — see wasmgaps/try_except.nim — so those two calls live there until
# ithaqua's landing pads can be jumped to from where this jumps to them.
let raw = "tab\there \"quoted\""
echo escape(raw)
