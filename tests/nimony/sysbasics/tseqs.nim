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

block: # order-preserving `delete`
  var a = @[10, 20, 30, 40, 50]
  a.delete(0)
  assert a == @[20, 30, 40, 50]
  a.delete(2)
  assert a == @[20, 30, 50]
  a.delete(a.len-1)
  assert a == @[20, 30]

  var one = @[99]
  one.delete(0)
  assert one.len == 0

  # managed element type: must not double-free or leak
  var s = @["aa", "bb", "cc", "dd"]
  s.delete(1)
  assert s == @["aa", "cc", "dd"]
  s.delete(0)
  assert s == @["cc", "dd"]
