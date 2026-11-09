import std / syncio

template unchanged(value: typed): untyped {.plugin: "deps/mcookbook_identity".}
template checked(body: typed) {.plugin: "deps/mcookbook_check".}

proc nextValue(calls: var int): int =
  inc calls
  calls

var calls = 0
echo unchanged(nextValue(calls))
echo "calls: ", calls
echo unchanged(42)
echo unchanged(unchanged(nextValue(calls)))
echo "calls: ", calls
checked:
  echo "checked"
