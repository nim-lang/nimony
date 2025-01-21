type bool {.magic: Bool.} = enum
  false = 0, true = 1

let x: bool = true # Error: expected `bool` but got: bool.0.abcdefgh
