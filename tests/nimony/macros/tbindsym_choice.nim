# bindSym with an overloaded name — sem at the macro def-site collects every
# matching symbol and emits a `(cchoice …)` (or `(ochoice …)` with brOpen).
# The call site does overload resolution against the choice using the actual
# argument types. Here `add` has three overloads in system (string+char,
# string+string, seq[T]+sink T); `add(buf, "hi")` resolves to string+string.
import std / [syncio, macros]

macro fillBuf(): untyped =
  let addSym = bindSym("add")
  result = newCall(addSym, [newIdentNode("buf"), newStrLitNode("hi")])

macro fillBufOpen(): untyped =
  # Same shape but brOpen: plugin output is (ochoice …) rather than (cchoice …).
  let addSym = bindSym("add", brOpen)
  result = newCall(addSym, [newIdentNode("buf"), newStrLitNode(" + open")])

var buf = ""
fillBuf()
fillBufOpen()
echo buf
