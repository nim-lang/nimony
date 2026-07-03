## Sequences: construction, append, indexing, length, and iteration.
import std/syncio

var s = @[1, 2, 3]
s.add(4)
s.add(5)
echo s.len
echo s[0]
echo s[4]

var total = 0
for x in s:
  total = total + x
echo total                # 1+2+3+4+5 = 15
