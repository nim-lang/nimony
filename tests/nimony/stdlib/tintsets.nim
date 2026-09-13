
import std / [syncio, assertions, intsets]

proc main =
  echo "start"
  var s = initIntSet()
  for i in 5000..<6000:
    s.incl i
    assert s.contains i
  echo "500..<600"
  for i in 500..<600:
    s.incl i
    assert s.contains i

  echo "50000..<60000"
  for i in 50000..<60000:
    s.incl i
    assert s.contains i

  echo "0..<500"
  for i in 0..<500:
    assert not s.contains i

  echo "excl 50100"
  s.excl 50100
  assert not s.contains 50100

  assert not containsOrIncl(s, 7)
  assert containsOrIncl(s, 7)

  # equality and hashing see members, not how the blocks were filled
  var a = initIntSet()
  var b = initIntSet()
  a.incl 3
  a.incl 70000
  b.incl 70000
  b.incl 3
  b.incl 900
  b.excl 900
  assert a == b
  assert hash(a) == hash(b)
  assert a.len == 2
  b.incl 4
  assert not (a == b)
  echo "success"

main()
