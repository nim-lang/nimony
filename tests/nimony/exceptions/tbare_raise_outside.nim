# Bare `raise` is only legal inside an `except` block. Unlike Nim 2,
# Nimony rejects it at sem time everywhere else: an empty raise outside
# any handler has no in-flight exception to re-raise, so it would only be
# implementable via thread-local storage with no real-world benefit.

proc bad() {.raises.} =
  raise

bad()
