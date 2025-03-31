
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
  echo "success"

main()
