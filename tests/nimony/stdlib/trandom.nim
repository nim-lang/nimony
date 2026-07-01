import std/[assertions, random]

proc main =
  # determinism: the same seed yields the same sequence
  var r1 = initRand(123)
  var r2 = initRand(123)
  for i in 0 ..< 50:
    assert r1.next() == r2.next()

  var r = initRand(42)

  # bounded integers stay in range
  for i in 0 ..< 2000:
    let v = r.rand(10)
    assert v >= 0 and v <= 10
  for i in 0 ..< 2000:
    let v = r.rand(1 .. 6)
    assert v >= 1 and v <= 6

  # floats stay in range
  for i in 0 ..< 2000:
    let f = r.rand(1.0)
    assert f >= 0.0 and f <= 1.0
  let f2 = r.rand(2.0 .. 5.0)
  assert f2 >= 2.0 and f2 <= 5.0

  # sample returns an element of the input
  let arr = [10, 20, 30, 40, 50]
  for i in 0 ..< 200:
    assert r.sample(arr) in arr

  # shuffle is a permutation
  var data = @[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  r.shuffle(data)
  assert data.len == 10
  for v in 1 .. 10:
    var cnt = 0
    for x in data:
      if x == v: inc cnt
    assert cnt == 1

  # default-generator procs run and stay in range
  randomize(99)
  assert rand(5) >= 0 and rand(5) <= 5
  let dv = rand(1 .. 3)
  assert dv >= 1 and dv <= 3

main()
