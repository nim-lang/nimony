
import std / syncio

proc main =
  var abc = "abc"
  for i in 0..<2:
    abc.add "def"
    let other = move(abc)
    echo other, " ", other.len
    echo "#", abc, "#"

main()

import std/[assertions]

# proc cho(x: string): string =
#   result = x

proc bar(): string =
  var x = "1212334"
  x.add "123"
  return if true: x else: "123"

assert bar() == "1212334123"
