# Nim vector and broadcasting implementation

type
  Broadcastable*[T] = concept
    func `[]`(x: Self, idx: int): T
    func `[]=`(x: var Self, idx: int, value: T)
    func len(x: Self): int
    func similar(x: Self): Self

  Arithmetic = concept
    func `+`(a,b: Self): Self
    func `-`(a,b: Self): Self
    func `*`(a,b: Self): Self

  ArithmeticAndDiv = concept of Arithmetic
    func `/`(a,b: Self): Self

  Mapper[T] = proc(x: T): T

func similar*[T](s: seq[T]): seq[T] {.untyped.} =
  result = newSeq[T](s.len)
func similar*[N: static[int], T](a: array[N, T]): array[N, T] {.untyped.} =
  result = default(array[N, T])
func len*[N: static[int], T](a: array[N, T]): int = N

template `@.`*[T: Arithmetic, C:Broadcastable[T]](expr: C): untyped {.untyped, plugin: "deps/broadcast.nim".}
func `+.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] + b[i]

  tmp

func `-.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] - b[i]

  tmp

func `*.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] * b[i]

  tmp

func `/.`*[T: ArithmeticAndDiv, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] / b[i]

  tmp

# Scalar broadcasting operations (container OP! scalar)
func `+!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] + b
  tmp

func `-!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] - b
  tmp

func `*!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] * b
  tmp

func `/!`*[T: ArithmeticAndDiv, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] / b
  tmp
  
template map*[T; C:Broadcastable[T]](a: C, f: Mapper[T]): C {.untyped.} =
  var tmp = similar(a)
  for i in 0..<tmp.len:
    tmp[i] = f(a[i])

  tmp