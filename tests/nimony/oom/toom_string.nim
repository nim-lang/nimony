# Strings and seqs have a defined continuation on allocation failure, which is
# what puts them in the recoverable class of `doc/internals/failure_modes.md`:
# unlike `new`, a failed string or seq operation yields a value the program can
# inspect and carry on with.
#
# For a string that value is the `"\nD^OOM\0"` cookie packed inline (length 7),
# which `isOom` detects. `-d:maxMem=1` (see `nimony.args`) is what makes the
# failure reachable: a single request far above the cap fails no matter how the
# allocator rounds sizes, which is what keeps this test deterministic.
import std / [syncio]

proc main() =
  # Short strings live inline. No allocation, so never the cookie.
  var small = "hi"
  small.add " there"
  echo "small: ", small, " oom=", isOom(small)

  # 2 MB against a 1 MB budget: must fail.
  let big = newString(2_000_000)
  echo "big oom=", isOom(big)
  echo "big len=", big.len
  echo "cookie: ", big[1], big[2], big[3], big[4], big[5]
  echo "thread saw oom=", threadOutOfMem()

  # The cookie is an ordinary short string, so the program keeps working with
  # it -- including appending, which grows a fresh string out of it.
  var revived = big
  revived.add "!"
  echo "revived len=", revived.len, " oom=", isOom(revived)

  # A growth that cannot be satisfied must leave the seq exactly as it was.
  # Before this was hardened a failed `realloc` nilled `data` and set `len = 0`,
  # which leaked the old block and silently truncated the caller's elements.
  var s = @[10, 20, 30]
  s.grow(1_000_000, 0)   # 8 MB: refused
  echo "seq len=", s.len, " ", s[0], " ", s[1], " ", s[2]

  # ... and the seq is still usable afterwards.
  s.add 40
  echo "seq after add: ", s.len, " ", s[3]

  echo "still running"

main()
