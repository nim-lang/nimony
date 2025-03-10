proc foo(s: var seq[(string, string)]; x, y: sink string) =
  s.add (x, y)

proc bar(a: var (string, string); x, y: sink string) =
  a = (x, y)
