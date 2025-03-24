template foo[T](): T = 123

let x: string = foo[float]()
