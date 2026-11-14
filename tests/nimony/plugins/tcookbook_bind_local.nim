import std / syncio

template appendBang(target: var string) =
  bind add
  add(target, "!")

proc add(target: var string; value: string) =
  target = "intercepted"

var text = "base"
appendBang(text)
echo text
