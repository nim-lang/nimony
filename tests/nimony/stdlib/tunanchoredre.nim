# Regular searches need no required literal. `reUnanchored` remains accepted
# for source compatibility, and `^` remains relative to each match attempt.

import std / [assertions, rex]

block anchoredSearch:
  let withFlag = re(r"^\d+$", {reExtended, reUnanchored})
  let withoutFlag = re"^\d+$"
  assert fullMatch("123", withoutFlag)
  assert findBounds("111!22", withoutFlag) == (4, 5)
  assert findBounds("111!", withoutFlag) == (-1, 0)
  for s in ["", "123", "111!22", "111!"]:
    assert findBounds(s, withFlag) == findBounds(s, withoutFlag)

block alternatives:
  let pattern = re"a|a*b"
  assert findBounds("!aaab", pattern) == (1, 4)
  assert findBounds("!aaa!", pattern) == (1, 1)
  assert findBounds("!!!", pattern) == (-1, 0)

block existingPatterns:
  assert findBounds("!abc@def!", re"\w+@\w+") == (1, 7)
  assert findBounds("!123!", re"\d+") == (1, 3)
