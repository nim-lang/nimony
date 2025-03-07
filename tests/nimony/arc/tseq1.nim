
import std / syncio

proc x(count: int; data: string) =
  var s = newSeq[string](count)
  for i in 0..<count:
    s[i] = data & "abc"

  echo s.len, " ", s[40]

x(1000, "hi")
