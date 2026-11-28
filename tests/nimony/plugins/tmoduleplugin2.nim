
import std / [assertions, syncio, tables]
import deps / [m3105, mhiddenplugin]

eraseToplevelBlocks()

echo "this should not be erased"

block:
  echo "should be erased"

proc checkGenericHooks() =
  var values = mk()
  values.add Foo(x: 2)
  var table = initTable[string, string]()
  table["a"] = "b"
  assert values.len == 2
  assert values[0].x == 1
  assert values[1].x == 2
  assert table.getOrDefault("a") == "b"

checkGenericHooks()
