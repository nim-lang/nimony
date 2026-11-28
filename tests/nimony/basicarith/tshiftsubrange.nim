# A subrange shift count is an integer count.
import std/syncio
var n: Natural = 4
var b8: range[0'u8..7'u8] = 2
const cn: Natural = 3
const c = 1 shl cn
echo 256 shr n, " ", 1 shl n, " ", ashr(-256, n), " ", 256'u shr n, " ", 256 shr b8, " ", c
