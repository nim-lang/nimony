template foo(): int =
  foo()

template bar1(): int =
  bar2()

template bar2(): int =
  bar1()
