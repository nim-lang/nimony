import std/[syncio, assertions, deques]

proc main =
  # construction / len
  var a = initDeque[int]()
  assert a.len == 0

  # addLast / addFirst / ordering
  for i in 1 .. 3: a.addLast(i)
  assert a.len == 3
  assert $a == "[1, 2, 3]"
  a.addFirst(0)
  assert $a == "[0, 1, 2, 3]"
  assert a.len == 4

  # indexing (front-relative) / []=
  assert a[0] == 0
  assert a[3] == 3
  a[1] = 10
  assert a[1] == 10
  a[1] = 1

  # peek
  assert a.peekFirst == 0
  assert a.peekLast == 3

  # pop from both ends
  assert a.popFirst == 0
  assert a.popLast == 3
  assert $a == "[1, 2]"
  assert a.len == 2

  # contains / in / notin
  assert 1 in a
  assert 9 notin a

  # iteration
  var total = 0
  for x in a:
    total += x
  assert total == 3
  var viaPairs = 0
  for i, x in a:
    viaPairs += i + x
  assert viaPairs == (0 + 1) + (1 + 2)

  # mitems mutates in place
  for x in a.mitems:
    x = x * 10
  assert $a == "[10, 20]"

  # growth past initial capacity preserves order
  var b = initDeque[int](2)
  for i in 0 ..< 100: b.addLast(i)
  assert b.len == 100
  assert b.peekFirst == 0
  assert b.peekLast == 99
  assert b[50] == 50

  # interleaved addFirst/addLast across a wrap boundary
  var c = initDeque[int]()
  c.addLast(1)
  c.addFirst(0)
  c.addLast(2)
  c.addFirst(-1)
  assert $c == "[-1, 0, 1, 2]"

  # toDeque from array, and string elements
  let d = toDeque([1, 2, 3])
  assert d.len == 3
  assert d.peekFirst == 1
  assert d.peekLast == 3

  var s = initDeque[string]()
  s.addLast("a")
  s.addLast("b")
  assert $s == "[a, b]"
  assert s.popFirst == "a"

  # clear
  a.clear()
  assert a.len == 0
  a.addLast(42)
  assert a.peekFirst == 42

  echo "deques: OK"

main()
