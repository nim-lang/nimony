import std/[syncio, assertions, heapqueue]

proc main =
  var h = initHeapQueue[int]()
  let items = [5, 1, 8, 3, 2, 7, 4, 6]
  for i in 0 ..< items.len:
    h.push(items[i])
  assert h.len == 8
  assert h[0] == 1

  var output: seq[int] = @[]
  while h.len > 0:
    output.add h.pop()
  assert output == @[1, 2, 3, 4, 5, 6, 7, 8]
  assert h.len == 0

  var h2 = toHeapQueue([3, 1, 2])
  assert h2.pop() == 1
  assert h2.pop() == 2
  assert h2.pop() == 3
  assert h2.len == 0

  h.push(42)
  h.push(7)
  assert h.len == 2
  h.clear()
  assert h.len == 0

  # find / contains / items
  var h3 = toHeapQueue([9, 5, 8])
  assert h3.find(5) >= 0
  assert h3.find(777) == -1
  assert 8 in h3
  assert 777 notin h3
  var isum = 0
  for x in h3.items: isum += x
  assert isum == 22

  # del removes any element and keeps the invariant
  var h4 = toHeapQueue([9, 5, 8, 1, 7])
  h4.del(0)              # remove the current root (min == 1)
  assert h4.len == 4
  var prev = -1
  while h4.len > 0:
    let v = h4.pop()
    assert v >= prev     # still drains in ascending order
    prev = v

  # replace / pushpop
  var h5 = toHeapQueue([5, 12])
  assert h5.replace(6) == 5
  assert h5[0] == 6
  assert h5.pushpop(4) == 4    # 4 <= min, returned unchanged
  assert h5.pushpop(20) == 6   # 20 > min, pops 6 and inserts 20

main()
