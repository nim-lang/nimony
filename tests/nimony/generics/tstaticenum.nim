import std/syncio

# Value ("static") generic parameters whose element type is an enum. An enum
# field is the canonical typed value of its enum type, so it binds verbatim
# (issue #2089 follow-up: enum value parameters).

type Color = enum red, green, blue

# --- direct call with an explicit enum value ---
proc name[C: static[Color]](): int = ord(C)
echo name[green]()            # 1

# --- value parameter carried in an object type; bound by unification ---
type Holder[C: static[Color]] = object
  x: int
proc get[C: static[Color]](h: Holder[C]): int = ord(C)
var h: Holder[blue]
echo get(h)                   # 2

# --- enum and int value parameters together ---
type Grid[C: static[Color]; N: static[int]] = object
  cells: array[N, int]
proc colorOf[C: static[Color]; N: static[int]](g: Grid[C, N]): int = ord(C)
var g: Grid[red, 4]
echo colorOf(g)               # 0
echo sizeof(g)                # 4 * 8 = 32

# --- holey enum (explicit ordinal values) ---
type Status = enum ok = 200, notFound = 404
proc statusCode[S: static[Status]](): int = ord(S)
echo statusCode[notFound]()   # 404

# --- a `const` aliasing an enum value ---
const c = blue
proc viaConst[C: static[Color]](): int = ord(C)
echo viaConst[c]()            # 2

# --- a `const` in a type position (object field and constructor) ---
type Box[C: static[Color]] = object
  holder: Holder[C]

proc makeBox[C: static[Color]](): Box[C] =
  Box[C](holder: Holder[C](x: 0))

proc boxViaConst(): Box[c] =
  makeBox[c]()
discard boxViaConst()

# --- an enum ordinal drives type-level arithmetic ---
type Buf[C: static[Color]] = object
  data: array[ord(C) + 1, int]
var b: Buf[blue]
echo sizeof(b)                # (2 + 1) * 8 = 24
