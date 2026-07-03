## Strings: concatenation, length, indexing, and `$` of an int.
import std/syncio

var s = "abc"
s = s & "def"
echo s
echo s.len
echo s[0]
echo s[5]
let n = 123
echo "n=" & $n
echo "empty:" & ""
