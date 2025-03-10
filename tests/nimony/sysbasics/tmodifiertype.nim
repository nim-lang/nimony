proc foo(s: var seq[(string, string)]; x, y: sink string) =
  s.add (x, y)
