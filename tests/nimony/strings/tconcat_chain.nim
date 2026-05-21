import std/[syncio, assertions]

proc getStr(s: string): string = s

proc bump(s: var int; v: string): string =
  inc s
  result = v

proc main =
  # Plain chain of four — exercises the n-ary lowering.
  let a = getStr("alpha")
  let b = getStr("beta")
  let c = getStr("gamma")
  let d = getStr("delta")
  let r = a & b & c & d
  assert r == "alphabetagammadelta"
  assert r.len == 19
  echo r

  # Side-effectful leaf in the middle — must be evaluated exactly once,
  # because the lowering computes `.len` and then `.add` separately.
  var counter = 0
  let s = a & bump(counter, "X") & b
  assert s == "alphaXbeta"
  assert counter == 1
  echo s

  # Right-leaning sub-chain (parenthesised on the right) is still flattened.
  let t = a & (b & c & d)
  assert t == "alphabetagammadelta"
  echo t

main()
