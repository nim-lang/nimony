block:
  let value = (1: 2)
  discard value

block:
  proc field(): int = 1
  let value = (field(): 2)
  discard value

block:
  template identity(value, steps: untyped): untyped = value
  proc plus(x, y: int): int = x + y
  let value = identity((identity(2): plus(3))):
    plus(4)
  discard value
