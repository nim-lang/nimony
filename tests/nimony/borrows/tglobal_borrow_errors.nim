# Test: global variable access errors in func/.noSideEffect context.

var globalInt: int = 42

# Error: reading a global var in a func (not as borrow root):
func badRead(): int = globalInt

