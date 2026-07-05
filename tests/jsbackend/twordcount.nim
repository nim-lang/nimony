## A small real program end-to-end on the JS backend: count word frequencies in
## a block of text, then print them ordered by count (descending), ties broken
## alphabetically. Exercises std/strutils + std/tables + std/algorithm together,
## compiled with `nimony c --bits:32`, run under Node.
import std/syncio
import std/strutils
import std/tables
import std/algorithm

const text = """
the cat sat on the mat
the dog sat on the log
the cat and the dog
"""

var counts = initTable[string, int]()
for word in text.splitWhitespace():
  let w = word.toLowerAscii()
  counts[w] = counts.getOrDefault(w, 0) + 1

# Collect (word, count) pairs into a seq so we can order them.
var pairsSeq: seq[(string, int)] = @[]
for k, v in counts:
  pairsSeq.add (k, v)

pairsSeq.sort(proc (x, y: (string, int)): int =
  # Higher count first; break ties by word ascending.
  if x[1] != y[1]: return y[1] - x[1]
  if x[0] < y[0]: return -1
  if x[0] > y[0]: return 1
  return 0)

for (word, count) in pairsSeq:
  echo word, ": ", count

echo "distinct words: ", counts.len
