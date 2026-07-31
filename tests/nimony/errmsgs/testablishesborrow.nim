# `establishesBorrow` describes a routine's result, so it is rejected elsewhere.

type
  Bad {.establishesBorrow.} = object
    x: int

var y {.establishesBorrow.} = 5
