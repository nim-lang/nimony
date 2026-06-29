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

main()
