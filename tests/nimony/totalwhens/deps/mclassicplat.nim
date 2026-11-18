# No `totalWhens` here: classic `when`, resolved by sem for the build target.

proc classicOs*(): string =
  when defined(windows):
    result = "windows"
  elif defined(linux) or defined(bsd):
    result = "linux-or-bsd"
  elif defined(macosx):
    result = "macosx"
  else:
    result = "other"

proc classicCpu*(): int =
  when defined(amd64):
    result = 1
  elif defined(arm64):
    result = 2
  else:
    result = 3
