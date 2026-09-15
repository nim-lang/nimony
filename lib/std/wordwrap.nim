{.feature: "staticContracts".}

#
#
#            Nim's Runtime Library
#        (c) Copyright 2018 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains an algorithm to wordwrap a Unicode string.

import std/[strutils, unicode, assertions]


func olen(s: string; start, lastExclusive: int): int =
  var i = start
  result = 0
  while i < lastExclusive:
    inc result
    let L = graphemeLen(s, i)
    inc i, L

func runEnd(s: string; start: Natural; seps: set[char]; isSep: bool): Natural {.
    ensures: result <= s.len.} =
  ## Where the run of separators (or of non-separators) starting at `start` ends.
  result = s.len
  for j in start ..< s.len:
    if (s[j] in seps) != isSep: return j

func wrapWords*(s: string, maxLineWidth = 80,
               splitLongWords = true,
               seps: set[char] = Whitespace,
               newLine = "\n"): string {.noSideEffect.} =
  ## Word wraps `s`.
  runnableExamples:
    assert "12345678901234567890".wrapWords() == "12345678901234567890"
    assert "123456789012345678901234567890".wrapWords(20) == "12345678901234567890\n1234567890"
    assert "Hello Bob. Hello John.".wrapWords(13, false) == "Hello Bob.\nHello John."
    assert "Hello Bob. Hello John.".wrapWords(13, true, {';'}) == "Hello Bob. He\nllo John."
  result = newStringOfCap(s.len + s.len shr 6)
  var spaceLeft = maxLineWidth
  var lastSep = ""

  var i: Natural = 0
  while true:
    var isSep = false
    if i < s.len: isSep = s[i] in seps
    let j = runEnd(s, i, seps, isSep)
    if j <= i: break
    #yield (substr(s, i, j-1), isSep)
    if isSep:
      lastSep.setLen 0
      for k in i..<j:
        if s[k] notin {'\L', '\C'}: lastSep.add s[k]
      if lastSep.len == 0:
        lastSep.add ' '
        dec spaceLeft
      else:
        spaceLeft = spaceLeft - olen(lastSep, 0, lastSep.len)
    else:
      let wlen = olen(s, i, j)
      if wlen > spaceLeft:
        if splitLongWords and wlen > maxLineWidth:
          var k: Natural = 0
          while k < j - i:
            if spaceLeft <= 0:
              spaceLeft = maxLineWidth
              result.add newLine
            dec spaceLeft
            let L = graphemeLen(s, k+i)
            var p = i + k
            for m in 0 ..< L:
              if p >= s.len: break
              result.add s[p]
              inc p
            inc k, L
        else:
          spaceLeft = maxLineWidth - wlen
          result.add(newLine)
          for k in i..<j: result.add(s[k])
      else:
        spaceLeft = spaceLeft - wlen
        result.add(lastSep)
        for k in i..<j: result.add(s[k])
        #lastSep.setLen(0)
    i = j
