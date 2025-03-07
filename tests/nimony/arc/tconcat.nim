
import std / syncio

proc main(inp: string) =
  let s = concat("a", inp, "c")
  echo s

main("b")
