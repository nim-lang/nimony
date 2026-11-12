# issue planetis-m/nimony-bugs-fuzzers#12: the loop variable of a `for` over
# a `let` container is a view into it, so it must not be written either.

type
  O = object
    v: int
  H = object
    data: seq[int]

proc setv(x: var int) = x = 99

iterator vitems(s: seq[int]): var int =
  var i = 0
  while i < s.len:
    yield s[i]
    inc i

iterator lrows(m: seq[seq[int]]): lent seq[int] =
  var i = 0
  while i < m.len:
    yield m[i]
    inc i

let gs = @[1, 2]
for x in gs:
  x = 5             # INVALID: global `let`

proc locals() =
  let s = @[1, 2]
  for x in s:
    setv(x)         # INVALID: `var` argument
  for i, x in s:
    x = i           # INVALID: `pairs`
  let a = [1, 2]
  for x in a:
    x = 5           # INVALID: array
  let os = @[O(v: 1)]
  for o in os:
    o.v = 77        # INVALID: field of an element
  let h = H(data: @[1, 2])
  for x in h.data:
    x = 5           # INVALID: field of a `let` object
  for x in vitems(s):
    x = 5           # INVALID: user iterator returning `var T`
  let m = @[@[1, 2], @[3]]
  for row in lrows(m):
    for x in row:
      x = 9         # INVALID: through a `lent` row of a `let`

proc stepped(p: seq[int]) =
  let s = @[1, 2]
  for x in s:
    inc x           # INVALID: `inc` of a `let` element
  for x in p:
    dec x, 2        # INVALID: `dec` of a parameter's element
  let m = @[@[1, 2], @[3]]
  for row in m:
    for x in row:
      x = 9         # INVALID: nested loop over a `let`
  for row in m:
    for x in row:
      setv(x)       # INVALID: likewise, as a `var` argument
  let cube = @[@[@[1]]]
  for a in cube:
    for b in a:
      for x in b:
        inc x       # INVALID: three levels deep

proc sets(p: seq[set[char]]) =
  for st in p:
    st.incl 'a'     # INVALID: `incl` of a parameter's element
  let s = @[{'b'}]
  for st in s:
    excl st, 'b'    # INVALID: `excl` of a `let` element

proc generic[T](v, w: T) =
  let s = @[v, v]
  for x in s:
    x = w           # INVALID: in a generic instance

locals()
stepped(@[1])
sets(@[{'b'}])
generic(3, 4)
