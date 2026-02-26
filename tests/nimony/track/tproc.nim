proc add(a, b: int): int = a + b

let x = add(1, 2)
#       ^usages
let y = add(3, 4)
discard x + y
