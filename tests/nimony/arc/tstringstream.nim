import imp/mstringstream

proc foo*(x: StringStream) =
  discard

var s = newStringStream()
foo(s)

