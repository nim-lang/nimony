
import std / [syncio]

proc effect(s: string): string =
  echo s
  result = s

proc dispatch(x: string) =
  case effect(x)
  of "foo":
    echo "foo"
  of "bar":
    echo "bar"
  else:
    echo "unknown"

dispatch("foo")
dispatch("bar")
dispatch("other")
