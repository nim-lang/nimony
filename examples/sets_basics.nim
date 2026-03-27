# Sets and hash sets
# ==================
#
# Nim has two kinds of sets:
# - `set[T]` — built-in bitsets for small ordinal types (char, enum, int8..int16)
# - `HashSet[T]` — hash-based sets for any hashable type

import std/[syncio, assertions, strutils, hashes, sets]

# --- Built-in bitsets ---

# `set[char]` is common for character classification:
const vowels = {'a', 'e', 'i', 'o', 'u'}
assert 'a' in vowels
assert 'b' notin vowels

# Set operations:
const consonants = Letters - vowels - UppercaseLetters
assert 'b' in consonants
assert 'a' notin consonants

# `contains` is the proc behind `in`:
assert vowels.contains('e')

# --- HashSet for larger types ---

var seen = initHashSet[string]()
seen.incl "apple"
seen.incl "banana"
seen.incl "apple"    # duplicate, no effect

assert "apple" in seen
assert "cherry" notin seen

# `containsOrIncl` is useful for deduplication loops:
assert seen.containsOrIncl("banana") == true   # was already there
assert seen.containsOrIncl("cherry") == false   # newly added
assert "cherry" in seen

echo "sets: OK"
