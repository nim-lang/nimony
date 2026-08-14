import std/syncio
proc main =
  if cast[int](false) != cast[int](nil): echo "ne-arm taken (WRONG)" else: echo "ne-arm skipped (CORRECT)"
main()
