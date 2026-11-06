# An arithmetic, logical or constructor expression is a value, not a
# location: it cannot be passed to a `var` parameter (nimony#2906).

proc bump(x: var int) =
  x = x + 1

proc flip(x: var bool) =
  x = not x

proc grow(s: var set[char]) =
  s.incl 'a'

proc main(k: int) =
  var a = 1
  bump(a)
  bump(1 + 2)
  bump(a * k)
  flip(a == k)
  flip(not (a < k))
  grow({'b'})

main(3)
