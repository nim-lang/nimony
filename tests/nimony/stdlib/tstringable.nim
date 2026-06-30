import std/[syncio, assertions]

# A fresh consumer (no local Stringable) that relies on system.Stringable
# being exported and auto-imported.
type Celsius = object
  deg: int

func `$`(c: Celsius): string =
  result = $c.deg
  result.add "C"

func show[T: Stringable](x: T): string = $x

proc main =
  assert show(42) == "42"
  assert show("hi") == "hi"
  assert show(Celsius(deg: 7)) == "7C"
  echo "stringable: OK"

main()
