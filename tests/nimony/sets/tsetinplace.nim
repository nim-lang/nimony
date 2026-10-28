import std/assertions

# `desugar.genSetOp` tests membership in a set bigger than a word without
# copying it when the element is a plain location: the set is read where it is,
# or through a pointer temporary when its expression has to run first. These
# check that the set is still evaluated once and before the element.

type
  Holder = object
    cs: set[char]
  State = object
    calls: int
    ch: char
    cs: set[char]
    h: Holder

proc byValue(st: var State): set[char] =
  inc st.calls
  st.ch = 'z'
  result = {'a'}

proc holder(st: var State): ptr Holder =
  inc st.calls
  st.ch = 'b'
  result = addr st.h

proc grow(st: var State): char =
  inc st.calls
  st.cs.incl 'q'
  result = 'q'

func member(s: set[char]; c: char): bool = c in s
func memberAt(data: seq[set[char]]; i: int; c: char): bool = c in data[i]
func memberField(x: Holder; c: char): bool = c in x.cs

block plain:
  let data = @[{'a'..'z'}, {'0'..'9'}]
  assert member({'x', '\255'}, 'x')
  assert member({'x', '\255'}, '\255')
  assert not member({'x'}, 'y')
  assert memberAt(data, 0, 'q') and memberAt(data, 1, '7')
  assert not memberAt(data, 1, 'q')
  assert memberField(Holder(cs: {'b'}), 'b')
  assert not memberField(Holder(cs: {'b'}), 'a')

block setRunsFirst:
  # The set's side effects happen before the element is read, and only once.
  var st = State(calls: 0, ch: 'a', cs: {}, h: Holder(cs: {'b'}))
  assert not (st.ch in byValue(st))
  st.ch = 'a'
  assert st.ch in holder(st)[].cs
  assert st.calls == 2

block elementWithEffect:
  # An element with side effects is still evaluated once.
  var st = State(calls: 0, ch: 'a', cs: {'a'}, h: Holder(cs: {}))
  discard grow(st) in st.cs
  assert st.calls == 1 and 'q' in st.cs
