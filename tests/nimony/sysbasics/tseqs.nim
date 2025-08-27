import std/assertions

proc main() =
  let x = newSeq[int](3)
  var s = default(seq[int])
  s.add(123)
  s.add(456)
  #s.add(newSeq[int](3)) # not defined yet
  s.add(0)
  s.add(0)
  s.add(0)
  s[3] = 789
  let s2 = @[123, 456, 0, 789, 0]
  assert s.len == s2.len
  var i = 0
  while i < s.len:
    let a = s[i]
    let b = s2[i]
    assert a == b
    inc i

main()


block:
  var x = @[1, 2, 3]
  assert x == @[1, 2, 3]
  assert x != @[1, 2]
