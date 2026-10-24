# Nim vector and broadcasting implementation

type
  Broadcastable*[T] = concept
    func `[]`(x: Self, idx: int): T
    func `[]=`(x: var Self, idx: int, value: T)
    func len(x: Self): int
    func similar(x: Self): Self

func similar*[T](s: seq[T]): seq[T] {.untyped.} =
  result = newSeq[T](s.len)
func similar*[N: static[int], T](a: array[N, T]): array[N, T] {.untyped.} =
  result = default(array[N, T])
func len*[N: static[int], T](a: array[N, T]): int = N

template `@.`*() {.plugin: "deps/broadcast.nim".}
template `+.`*[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for 0..<a.len:
    tmp[i] = a[i] + b[i]

  tmp

template `-.`*[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for 0..<a.len:
    tmp[i] = a[i] - b[i]

  tmp

template `*.`*[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for 0..<a.len:
    tmp[i] = a[i] * b[i]

  tmp

template `/.`*[T, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for 0..<a.len:
    tmp[i] = a[i] / b[i]

  tmp
  
