import std/strtabs
import std/assertions

proc foo {.nodestroy.} = # TODO: bug #1561
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

    x.clear(modeCaseInsensitive)
    x["11"] = "22"
    assert x["11"] == "22"
  except:
    assert false

foo()