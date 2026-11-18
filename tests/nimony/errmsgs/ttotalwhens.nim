{.feature: "totalWhens".}

when defined(windows):
  proc foo() = discard
else:
  proc foo() = discard

proc a(): int =
  when defined(linux) and defined(amd64):
    result = 1
  else:
    result = 2

proc b(): int =
  when not defined(windows):
    result = 1

proc d(): int =
  # not a platform `when`: resolved by sem as always
  when defined(release):
    result = 1
  else:
    result = 3
