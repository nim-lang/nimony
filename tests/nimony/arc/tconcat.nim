
import std / syncio

proc main(inp: string) =
  let s = "a" & inp & "c"
  echo s

main("b")
