import std/strtabs
import std/assertions

func makeEmpty(): StringTableRef =
  newStringTable(modeCaseSensitive)

func readOnly(t: StringTableRef): string =
  assert t.hasKey("only")
  assert "only" in t
  assert t.mode == modeCaseSensitive
  assert t.getOrDefault("only") == "value"
  assert t.len == 1
  $t

proc foo =
  var x = newStringTable(modeCaseSensitive)
  try:
    x["k"] = "v"
    x["11"] = "22"
    x["565"] = "67"
    assert x["k"] == "v"
    assert x["11"] == "22"
    assert x["565"] == "67"
    x["11"] = "23"
    assert x["11"] == "23"

    let one = newStringTable(modeCaseSensitive)
    one["only"] = "value"
    assert readOnly(one) == "{only: value}"
    assert makeEmpty().len == 0

    # force the table to grow so the func-based rehash path is covered
    for i in 0..<200:
      x[$i] = $i
    for i in 0..<200:
      assert x[$i] == $i

    x.clear(modeCaseInsensitive)
    x["11"] = "22"
    assert x["11"] == "22"
  except:
    assert false

foo()