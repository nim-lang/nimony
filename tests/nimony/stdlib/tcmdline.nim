import std/[assertions, cmdline]
try:
  var params = paramCount()
  assert params == 0
  assert paramStr(0).len > 0
  let s = commandLineParams()
  assert s.len == 0
except:
  assert false
