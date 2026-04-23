import std/assertions

# Regression for: set subtraction over >64 elements (char sets) silently
# returned an empty set because `desugar.nim` checked `op == MinusSetX` after
# `op` had been rewritten to `BitandX`, suppressing the `bitnot` step.

const Ctrl = {'A', 'Z', 'E', 'S', 'O', 'U', 'X', 'R'}

assert card({'a'..'z'} - {'X'}) == 26
assert card({'a'..'z', '0'..'9', '_'} - Ctrl) == 37
assert 's' in ({'a'..'z', '0'..'9', '_'} - Ctrl)
assert 'X' notin ({'a'..'z', '0'..'9', '_'} - Ctrl)

proc runtimeDiff(a, b: set[char]): set[char] = a - b
assert card(runtimeDiff({'a'..'z'}, {'X'})) == 26
