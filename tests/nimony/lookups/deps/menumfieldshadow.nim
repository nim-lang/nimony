type
  Role* = enum
    lower
    upper

func tagRole*(role: Role): int =
  case role
  of lower: 1
  of upper: 2
