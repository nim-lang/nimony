# nim-lang/nimony#2750: any integer indexes an integer-indexed array relative
# to the array's own index range, never via the 0-based `toOpenArray(x)[i]`.
import std/syncio

var arr: array[1'u16..2'u16, int] = [1984, 451]
echo arr[1]
echo arr[2]
var i = 1
echo arr[i]
arr[i] = 5
echo arr[1]
let b: array[1'u8..2'u8, int] = [7, 8]
echo b[1]
var c: array[1..2, int] = [3, 4]
var j: int32 = 1
echo c[j]
var z: array[3, int] = [1, 2, 3]
var u: uint8 = 2
echo z[u]
