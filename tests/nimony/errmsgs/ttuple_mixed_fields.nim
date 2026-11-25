block:
  let value = (first: 1, 2)
  discard value

block:
  let value = (first: 1, second: 2, 3)
  discard value

block:
  let value = (outer: (inner: 1, 2))
  discard value
