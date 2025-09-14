
proc contains*(s: HSlice[float, float]; value: char): bool =
  (value.int.float / 128) >= s.a and (value.int.float / 128) <= s.b
