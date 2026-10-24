# Nim vector and broadcasting implementation

type
  Broadcastable*[T] = concept
    func `[]`(x: Self, idx: int): T
    func `[]=`(x: var Self, idx: int, value: T)
    func len(x: Self): int
    func similar(x: Self): Self

  Arithmetic* = concept
    func `+`(a,b: Self): Self
    func `-`(a,b: Self): Self
    func `*`(a,b: Self): Self

  ArithmeticAndDiv* = concept of Arithmetic
    func `/`(a,b: Self): Self

  HasPower* = concept
    func `^`(a,b: Self): Self

  HasBitShift* = concept
    func `shr`(a,b: Self): Self
    func `shl`(a,b: Self): Self

  HasBitwise* = concept
    func `and`(a,b: Self): Self
    func `or`(a,b: Self): Self
    func `xor`(a,b: Self): Self

  HasAmpersand* = concept
    func `&`(a,b: Self): Self

  HasDivMod* = concept
    func `div`(a,b: Self): Self
    func `mod`(a,b: Self): Self

  Mapper*[T] = proc(x: T): T

func similar*[T](s: seq[T]): seq[T] {.untyped.} =
  result = newSeq[T](s.len)
func similar*[N: static[int], T](a: array[N, T]): array[N, T] {.untyped.} =
  result = default(array[N, T])
func len*[N: static[int], T](a: array[N, T]): int = N

# Loop fusion operator.
# This is able to compute expressions like `a +. b *. c +! 5 + map(sin, arr)` in a single loop without temporary allocations
template `@.`*[T; C:Broadcastable[T]](expr: C): untyped {.untyped, plugin: "deps/broadcast.nim".}

# --- Arithmetic operators (. and !) ---
func `+.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] + b[i]
  tmp

func `+!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] + b
  tmp

func `-.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] - b[i]
  tmp

func `-!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] - b
  tmp

func `*.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] * b[i]
  tmp

func `*!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] * b
  tmp

func `/.`*[T: ArithmeticAndDiv, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] / b[i]
  tmp

func `/!`*[T: ArithmeticAndDiv, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] / b
  tmp

# --- Power operators ---
func `^.`*[T: HasPower, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] ^ b[i]
  tmp

func `^!`*[T: HasPower, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] ^ b
  tmp

# --- Bitshift operators ---
func `<<.`*[T: HasBitShift, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] shl b[i]
  tmp

func `<<!`*[T: HasBitShift, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] shl b
  tmp

func `>>.`*[T: HasBitShift, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] shr b[i]
  tmp

func `>>!`*[T: HasBitShift, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] shr b
  tmp

# --- Bitwise operators ---
func `&&.`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] and b[i]
  tmp

func `&&!`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] and b
  tmp

func `||.`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] or b[i]
  tmp

func `||!`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] or b
  tmp

func `^^.`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] xor b[i]
  tmp

func `^^!`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] xor b
  tmp

# --- Ampersand / concat operators ---
func `&.`*[T: HasAmpersand, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] & b[i]
  tmp

func `&!`*[T: HasAmpersand, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] & b
  tmp

# --- Integer division and modulo ---
func `//.`*[T: HasDivMod, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] div b[i]
  tmp

func `//!`*[T: HasDivMod, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] div b
  tmp

func `%.`*[T: HasDivMod, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] mod b[i]
  tmp

func `%!`*[T: HasDivMod, C:Broadcastable[T]](a: C, b: T): C =
  var tmp = similar(a)
  for i in 0..<a.len:
    tmp[i] = a[i] mod b
  tmp

template map*[T; C:Broadcastable[T]](a: C, f: Mapper[T]): C {.untyped.} =
  var tmp = similar(a)
  for i in 0..<tmp.len:
    tmp[i] = f(a[i])

  tmp

template `-`*[T; C:Broadcastable[T]](a: C): C {.untyped.} = map(a, `-`)

proc fill*[T; C: Broadcastable[T]](v: var C, val: T) {.untyped.} =
  for i in 0..<v.len:
    v[i] = val
