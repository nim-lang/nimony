
type
  int* {.magic: Int.}

proc use*(x: int) = discard

type string* {.magic: String.}

proc useOverloaded*(x: string) = discard
proc useOverloaded*(x: int) = discard
