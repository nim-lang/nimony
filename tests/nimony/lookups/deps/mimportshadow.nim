type
  Color* = enum
    red
    green

func tint*(c: Color): int =
  case c
  of red: 1
  of green: 2

proc thing*(x: int): string = "imported"
