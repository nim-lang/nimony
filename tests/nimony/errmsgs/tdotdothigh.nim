# `a..b` up to `high(T)` never ends: `inc` wraps there. Unless `T` has bits
# to spare (an enum, a small `range`, `bool`), that is a compile-time error.

type
  E = enum ea, eb, ec
  R = range[0..10]

proc main() =
  var s = 0
  for e in ea..ec: s += ord(e)
  for r in R(0)..R(10): s += r
  for b in false..true: s += ord(b)
  for i in 0..5: s += i
  for i in 0..high(int): s += i
  for i in 0'u8..255'u8: s += int(i)
  for c in 'a'..'\255': s += ord(c)
  for n in Natural(0)..high(Natural): s += n

main()

proc viaCountup() =
  var s = 0
  for i in countup(0, 10, 2): s += i
  for i in countup(0, high(int)): s += i

viaCountup()
