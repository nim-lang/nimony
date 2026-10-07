func at*(s: string; i: int): char {.requires: (i < len(s) and i >= 0).} =
  s[i]
