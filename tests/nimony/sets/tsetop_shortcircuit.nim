## A set operation whose operand is a checked index, inside `and`/`or`. The
## expansion needs statements in front of it (the bound check's temp, a big
## set's loop), and under the Final IR those go in front of the statement, so
## they must never end up in front of an operand the short-circuit skips: an
## index past the end has to read as `false`, not as a panic.
import std/syncio

proc lower(s: string; i: int): bool =
  result = i < s.len and s[i] in {'a'..'z'}

proc upperOrEmpty(s: string; i: int): bool =
  result = i >= s.len or s[i] in {'A'..'Z'}

proc subsetAt(s: string; i: int): bool =
  var letters: set[char] = {'a'..'z'}
  result = i < s.len and {s[i]} < letters

proc arrLower(a: array[3, char]; i: int): bool =
  # an array index is no call, so `xelim` hands a condition's `and` on as it is
  result = false
  if i < 3 and a[i] in {'a'..'z'}:
    result = true

echo lower("aB", 0), " ", lower("aB", 1), " ", lower("aB", 7)
echo upperOrEmpty("aB", 1), " ", upperOrEmpty("aB", 0), " ", upperOrEmpty("aB", 9)
echo subsetAt("x", 0), " ", subsetAt("X", 0), " ", subsetAt("x", 3)
echo arrLower(['a', 'B', 'c'], 0), " ", arrLower(['a', 'B', 'c'], 1), " ", arrLower(['a', 'B', 'c'], 5)
