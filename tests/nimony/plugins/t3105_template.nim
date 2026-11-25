import std / [assertions, tables]
import deps / [m3105, m3105template]

keepModule()

proc main() =
  var values = mk()
  values.add Foo(x: 2)
  var table = initTable[string, string]()
  table["a"] = "b"
  assert values.len == 2
  assert values[0].x == 1
  assert values[1].x == 2
  assert table.getOrDefault("a") == "b"

main()
