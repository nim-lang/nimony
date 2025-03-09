
import std / syncio

proc main =
  var abc = "abc"
  for i in 0..<2:
    abc.add "def"
    let other = move(abc)
    echo other, " ", other.len
    echo "#", abc, "#"

main()
