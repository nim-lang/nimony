proc foo(): int =
  234

proc bar(x: var int) =
  let m = x

bar(foo())