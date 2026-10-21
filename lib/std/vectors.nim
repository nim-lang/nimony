# Nim vector and broadcasting implementation

type
  Broadcastable*[T] = concept
    func `[]`(x: Self, idx: int): T
    func `[]=`(x: var Self, idx: int, value: T)
    func len(x: Self): int
    func similar(x: Self): Self

template `+.`[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len,
  ensures: result.len == a.len.} =
  
  result = similar(a)
  for 0..<a.len:
    result[i] = a[i] + b[i]

template `-.`[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len,
  ensures: result.len == a.len.} =
  
  result = similar(a)
  for 0..<a.len:
    result[i] = a[i] - b[i]

template `*.`[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len,
  ensures: result.len == a.len.} =
  
  result = similar(a)
  for 0..<a.len:
    result[i] = a[i] * b[i]

template `/.`[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len,
  ensures: result.len == a.len.} =
  
  result = similar(a)
  for 0..<a.len:
    result[i] = a[i] / b[i]
