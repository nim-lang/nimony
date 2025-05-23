import std / syncio

proc takesSet(s: set[char]) = discard

proc takesSets[T](a, b: set[T]) =
  let s: string
  if contains(a, 'a'): s = "T"
  else: s = "F"
  echo s

proc main =
  takesSet {}

  takesSets {'a'}, {}

main()
