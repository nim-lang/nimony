proc cAssert(x: bool) {.importc: "assert", header: "<assert.h>".}

proc main() =
  let x = newSeq[int](3)
  when false:
    var s = default(seq[int])
    s.add(123)
    s.add(456)
    s.add(newSeq[int](3))
    s[3] = 789
    let s2 = @[123, 456, 0, 789, 0] # fails with "could not infer type"
    cAssert(s.len == s2.len)
    var i = 0
    while i < s.len:
      let a = s[i]
      let b = s2[i]
      cAssert(a == b)
      inc i
