# #2689: `result = result` after an early `return result` made copyprop
# rename the tail `ret` to a deleted temporary.
import std/syncio
type Box = ref object
  v: string

proc mk(s: string): Box = Box(v: s)

proc pick(op: string): Box =
  result = mk(op)
  if op == "t":
    return result
  result          # implicit tail result

proc main() =
  echo pick("t").v
  echo pick("x").v

main()
