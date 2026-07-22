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

# parsing round trips (small values only: the oracle is 64-bit, wasm is 32).
# parseInt/unescape are `.raises` routines — nimony requires the try block.
try:
  echo parseInt("-0042")              # -42
  echo parseInt("123456")             # 123456
except:
  echo "parse failed"

# escape / unescape round trip
let raw = "tab\there \"quoted\""
let esc = escape(raw)
echo esc
try:
  echo unescape(esc) == raw           # true
except:
  echo "unescape failed"
