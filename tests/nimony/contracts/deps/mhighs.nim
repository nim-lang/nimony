# Non-generic accessors over `len(s)` in a module of their own, as the
# system's string `high` is to its importers. See `tstringhigh`.

func lastIndex*(s: string): int {.inline.} = len(s) - 1
func roomy*(s: string): int = len(s) + MaxContainerLen
