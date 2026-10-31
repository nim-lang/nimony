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
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] + b[i]


func `+!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] + b

template `+!`*[T: Arithmetic, C:Broadcastable[T]](a: T, b: C): C {.untyped.} = b +! a

func `-.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] - b[i]


func `-!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] - b

template `-!`*[T: Arithmetic, C:Broadcastable[T]](a: T, b: C): C {.untyped.} = -(b -! a)

func `*.`*[T: Arithmetic, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] * b[i]


func `*!`*[T: Arithmetic, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] * b

template `*!`*[T: Arithmetic, C:Broadcastable[T]](a: T, b: C): C {.untyped.} = b *! a

func `/.`*[T: ArithmeticAndDiv, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] / b[i]


func `/!`*[T: ArithmeticAndDiv, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] / b

func inv[T: ArithmeticAndDiv](v: T): T {.untyped.} = 1/v
template `/!`*[T: ArithmeticAndDiv, C:Broadcastable[T]](a: T, b: C): C {.untyped.} = map(b, inv) *! a

# --- Power operators ---
func `^.`*[T: HasPower, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] ^ b[i]


func `^!`*[T: HasPower, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] ^ b


# --- Bitshift operators ---
func `<<.`*[T: HasBitShift, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] shl b[i]
template `shl`*[T: HasBitShift, C:Broadcastable[T]](a, b: C): C = a <<. b

func `<<!`*[T: HasBitShift, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] shl b
template `shl`*[T: HasBitShift, C:Broadcastable[T]](a:C, b: T): C = a <<! b


func `>>.`*[T: HasBitShift, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] shr b[i]
template `shr`*[T: HasBitShift, C:Broadcastable[T]](a, b: C): C = a >>. b

func `>>!`*[T: HasBitShift, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] shr b
template `shr`*[T: HasBitShift, C:Broadcastable[T]](a:C, b: T): C = a >>! b

# --- Bitwise operators ---
func `&&.`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] and b[i]
template `and`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C = a &&. b

func `&&!`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] and b
template `and`*[T: HasBitwise, C:Broadcastable[T]](a:C, b: T): C = a &&! b
template `&&!`*[T: HasBitwise, C:Broadcastable[T]](a: T, b: C): C {.untyped.} = b &&! a

func `||.`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] or b[i]
template `or`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C = a ||. b

func `||!`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] or b
template `or`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C = a ||! b
template `||!`*[T: HasBitwise, C:Broadcastable[T]](a: T, b: C): C {.untyped.} = b ||! a

func `^^.`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] xor b[i]
template `xor`*[T: HasBitwise, C:Broadcastable[T]](a, b: C): C = a ^^. b

func `^^!`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] xor b
template `xor`*[T: HasBitwise, C:Broadcastable[T]](a: C, b: T): C = a ^^! b
template `^^!`*[T: HasBitwise, C:Broadcastable[T]](a: T, b: C): C {.untyped.} = b ^^! a

# --- Ampersand / concat operators ---
func `&.`*[T: HasAmpersand, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] & b[i]


func `&!`*[T: HasAmpersand, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] & b


# --- Integer division and modulo ---
func `//.`*[T: HasDivMod, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] div b[i]
template `div`*[T: HasDivMod, C:Broadcastable[T]](a, b: C): C = a //. b

func `//!`*[T: HasDivMod, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] div b
template `div`*[T: HasDivMod, C:Broadcastable[T]](a:C, b: T): C = a //! b

func `%.`*[T: HasDivMod, C:Broadcastable[T]](a, b: C): C {.requires: a.len == b.len.} =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] mod b[i]
template `mod`*[T: HasDivMod, C:Broadcastable[T]](a, b: C): C = a %. b

func `%!`*[T: HasDivMod, C:Broadcastable[T]](a: C, b: T): C =
  result = similar(a)
  for i in 0..<a.len:
    result[i] = a[i] mod b
template `mod`*[T: HasDivMod, C:Broadcastable[T]](a:C, b: T): C = a %! b


proc map*[T; C:Broadcastable[T]](a: C, f: Mapper[T]): C {.untyped.} =
  result = similar(a)
  for i in 0..<result.len:
    result[i] = f(a[i])

  result

template `-`*[T; C:Broadcastable[T]](a: C): C {.untyped.} = map(a, `-`)

proc fill*[T; C: Broadcastable[T]](v: var C, val: T) {.untyped.} =
  for i in 0..<v.len:
    v[i] = val
