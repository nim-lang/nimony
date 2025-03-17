import std/syncio

proc main() =
  let a = "abc"
  case a
  of "def", "ghi": echo "wrong"
  of "abc", "jkl": echo "correct"
  else: echo "also wrong"

main()
