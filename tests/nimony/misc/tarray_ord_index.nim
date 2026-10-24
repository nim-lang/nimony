# nim-lang/nimony#2750: an integer index into an enum- or char-indexed array is
# the ordinal value, relative to the array's own index range.
import std/syncio

type E = enum ea, eb, ec
var d: array[eb..ec, int] = [5, 6]
var k = 1
echo d[k]
echo d[eb]
var ch: array['a'..'c', int] = [1, 2, 3]
echo ch[98]
echo ch['c']
