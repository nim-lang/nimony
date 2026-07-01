#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the
## `Levenshtein edit distance <https://en.wikipedia.org/wiki/Levenshtein_distance>`_:
## the minimum number of single-character insertions, deletions or substitutions
## needed to turn one string into another.
##
## This is the Nimony port. `editDistance` compares by `char` (i.e. by byte), so
## it corresponds to Nim 2's `editDistanceAscii`; for pure ASCII the two agree.
## A rune-aware variant over `std/unicode` (matching Nim 2's `editDistance`) is a
## possible follow-up.

func editDistance*(a, b: string): int =
  ## Returns the Levenshtein edit distance between `a` and `b`, comparing by
  ## `char`.
  runnableExamples:
    doAssert editDistance("kitten", "sitting") == 3
    doAssert editDistance("", "abc") == 3
    doAssert editDistance("abc", "abc") == 0
  let la = a.len
  let lb = b.len
  if la == 0: return lb
  if lb == 0: return la
  # Space-optimized DP: a single row of length `lb + 1`, updated in place, with
  # `prev` carrying the diagonal value `dp[i-1][j-1]`.
  var row = newSeq[int](lb + 1)
  for j in 0 .. lb:
    row[j] = j
  for i in 1 .. la:
    var prev = row[0]        # dp[i-1][0]
    row[0] = i               # dp[i][0]
    let ca = a[i-1]
    for j in 1 .. lb:
      let diag = row[j]      # dp[i-1][j], becomes the next diagonal
      let cost = if ca == b[j-1]: 0 else: 1
      var best = row[j] + 1          # deletion:  dp[i-1][j] + 1
      if row[j-1] + 1 < best:        # insertion: dp[i][j-1] + 1
        best = row[j-1] + 1
      if prev + cost < best:         # match/sub: dp[i-1][j-1] + cost
        best = prev + cost
      row[j] = best
      prev = diag
  result = row[lb]
