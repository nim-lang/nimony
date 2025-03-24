
import std / syncio

type
  X = object
    s: seq[string]

proc use(x: int) = discard

template copyInto(x: var X; body: untyped) =
  let oldLen = x.s.len
  body
  use oldLen

type SymId = distinct int

template `[]`*(syms: seq[string]; s: SymId): string = syms[s.int]

proc main =
  var x = X(s: @["abc", "def"])
  copyInto x:
    copyInto x:
      echo "yes ", x.s[SymId(1)]

main()
