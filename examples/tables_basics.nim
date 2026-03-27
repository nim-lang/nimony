# Hash tables
# ===========
#
# Tables store key-value pairs. Keys must be `Keyable`:
# they need `==` and `hash`. All built-in types qualify.
# You must import `hashes` alongside `tables`.

import std/[syncio, assertions, hashes, tables]

# --- Creating and populating ---

var counts = initTable[string, int]()
counts["apples"] = 3
counts["oranges"] = 5
assert counts.len == 2

# `[]=` overwrites existing keys:
counts["apples"] = 10
assert counts.getOrDefault("apples") == 10

# --- Lookup ---

# `getOrDefault` returns the type's default (0 for int) on missing keys:
assert counts.getOrDefault("bananas") == 0

# `contains` / `hasKey` test for presence:
assert "apples" in counts
assert not counts.hasKey("bananas")

# --- Mutation in place ---

# `mgetOrPut` returns a var reference — ideal for counters:
var words = initTable[string, int]()
for w in ["the", "cat", "sat", "on", "the", "mat"]:
  words.mgetOrPut(w, 0) += 1
assert words.getOrDefault("the") == 2
assert words.getOrDefault("cat") == 1

# --- Iteration ---

# `pairs` yields (key, value):
var total = 0
for fruit, count in pairs(counts):
  total += count
assert total == 15

echo "tables: OK"
