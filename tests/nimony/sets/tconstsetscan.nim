import std/assertions

# `desugar.genSetOp` reads a `const` set in place (and hoists an anonymous set
# literal to a `const` to do the same), so a membership test is now an indexed
# load from a GLOBAL rather than from a stack copy of it. Nothing covered that
# shape on the native backend, where it turned out to cost two registers instead
# of none — see `pickStagingScratch`'s `rawHomeRegs` guard and `binFold`'s
# address-register reuse in arkham. Both bugs need the register pressure of
# `-d:danger` to fire, which this suite does not run; this is a plain
# correctness test for the path, not a guard for those two.

const Digits = {'0'..'9'}

func scan(s: openArray[char]; b: var int): int =
  result = 0
  var
    sign = -1
    i = 0
    res = 0
    overflow = false
  if i < s.len:
    if s[i] == '+': inc(i)
    elif s[i] == '-':
      inc(i)
      sign = 1
  if i < s.len and s[i] in Digits:
    while i < s.len and s[i] in Digits:
      let c = ord(s[i]) - ord('0')
      if not overflow:
        if res >= (low(int) + c) div 10:
          res = res * 10 - c
        else:
          overflow = true
      inc(i)
      while i < s.len and s[i] == '_': inc(i)
    if overflow: return -i
    b = res * sign
    result = i

var v = 0
assert scan("-12345".toOpenArray(0, 5), v) == 6
assert v == -12345

assert scan("789".toOpenArray(0, 2), v) == 3
assert v == 789

assert scan("x9".toOpenArray(0, 1), v) == 0
assert v == 789                                  # unchanged: no digit was read

# an anonymous set literal takes the same path, via a hoisted `const`
func countDigits(s: openArray[char]): int =
  result = 0
  for i in 0 ..< s.len:
    if s[i] in {'0'..'9'}: inc result

assert countDigits("a1b22c333".toOpenArray(0, 8)) == 6
assert countDigits("nodigits".toOpenArray(0, 7)) == 0
