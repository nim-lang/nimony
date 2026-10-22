import std/syncio

var a: array[byte, int]
a[200] = 1
var i = 200'u8 # above 127
echo a[i]
