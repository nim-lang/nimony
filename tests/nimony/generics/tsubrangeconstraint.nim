# A subrange satisfies a generic parameter's constraint when its base type
# does, at the cost of a subrange match; `is` answers the same.
import std/syncio
import deps/msubrangeconstraint

type
  H = enum
    h0, h1, h2
  RH = range[h0..h1]
  Holey = enum
    ha = 1, hb = 5, hc = 9

proc g[T: enum](x: T): int = ord(x)
proc n[T: SomeInteger](x: T): string = "SomeInteger"
proc i[T: int](x: T): string = "int"
proc c[T: char](x: T): string = "char"

var x: RH = h1
echo g(x), " ", $x
echo x
for v in RH(h0)..RH(h1): echo v
var ho: range[ha..hb] = hb
echo g(ho), " ", ho
echo RH is enum, " ", RH is SomeOrdinal, " ", Natural is SomeInteger

proc k[T](x: T): string =
  when T is enum: "enum" else: "other"
echo k(x)

var r: range[1..5] = 3
var nat: Natural = 4
var u: range[1'u8..5'u8] = 2
var ch: range['a'..'z'] = 'q'
echo n(r), " ", n(nat), " ", n(u), " ", i(r), " ", c(ch)

# the subrange reaches `enum` only through a subrange match, so the local
# candidate beats the imported `[T]` only by module of origin
proc p[T: enum](x: T): string = "local enum"
echo p(x)

# a constraint the subrange satisfies as it is still costs nothing
proc q[T: enum](x: T): string = "enum"
proc q[T: range](x: T): string = "range"
echo q(x)
