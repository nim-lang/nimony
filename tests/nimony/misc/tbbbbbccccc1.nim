# to_stmts1

import std / syncio

proc mymain: int =
  var x = 'a'
  var i = 0
  var s = newString(10)
  while (let a = 8; a < 90) and i < 10:
    x = 'b'
    s[i] = if 5 <= i: 'c' else: x
    inc i
  echo s
  result = 44

discard mymain()
