{.feature: "anonBlockBreaks".}
import std/syncio
proc main() =
  for i in 1..3:
    block:
      if i == 2: break
      echo "in ", i
    echo "after ", i
  block:
    echo "a"
    break
  echo "b"
  while true:
    block named:
      while true:
        break
      break
    echo "x"
    break
  echo "done"
main()
