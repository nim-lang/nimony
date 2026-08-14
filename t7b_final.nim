import std/syncio
proc main =
  let e = cast[pointer](false) == nil
  echo "as expression: ", e          # true  (correct)
  if cast[pointer](false) == nil:
    echo "as if-guard: true (CORRECT)"
  else:
    echo "as if-guard: false (WRONG)"
main()
