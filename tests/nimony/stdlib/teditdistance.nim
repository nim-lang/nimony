import std/assertions
import std/editdistance

# --- empty / identity ---
assert editDistance("", "") == 0
assert editDistance("abc", "") == 3
assert editDistance("", "abc") == 3
assert editDistance("abc", "abc") == 0

# --- single edits ---
assert editDistance("abc", "abd") == 1      # substitution
assert editDistance("abc", "ab") == 1       # deletion
assert editDistance("ab", "abc") == 1       # insertion
assert editDistance("a", "") == 1

# --- classic vectors ---
assert editDistance("kitten", "sitting") == 3
assert editDistance("flaw", "lawn") == 2
assert editDistance("gumbo", "gambol") == 2
assert editDistance("book", "back") == 2
assert editDistance("sunday", "saturday") == 3

# --- symmetry ---
assert editDistance("kitten", "sitting") == editDistance("sitting", "kitten")
assert editDistance("distance", "editing") == editDistance("editing", "distance")

# --- repeated characters ---
assert editDistance("aaa", "aaaa") == 1
assert editDistance("aaaa", "aaa") == 1
assert editDistance("aaaa", "aaaa") == 0
