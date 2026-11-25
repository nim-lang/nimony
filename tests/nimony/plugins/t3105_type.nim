import std / [assertions, tables]
import deps / [m3105, m3105type]

proc main() =
  var marked = mkMarked()
  marked.add Marker(value: 8)
  let values = mk()
  var table = initTable[string, string]()
  table["a"] = "b"
  assert marked.len == 2
  assert marked[0].value == 7
  assert marked[1].value == 8
  assert values[0].x == 1
  assert table.getOrDefault("a") == "b"

main()
