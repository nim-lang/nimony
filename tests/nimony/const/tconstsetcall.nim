# issue #2725: consts computed by procs returning sets and enums
import std/syncio
type E = enum ea, eb, ec
proc g(): set[char] = {'a', 'z'}
proc h(): set[E] = {eb, ec}
proc k(): set[uint8] = {3'u8, 200'u8}
proc single(): E = eb
const c = g()
const d = h()
const e = k()
const s = single()
echo 'a' in c, " ", ' ' in c, " ", eb in d, " ", ea in d, " ", 200'u8 in e, " ", card(c), " ", card(e), " ", s
