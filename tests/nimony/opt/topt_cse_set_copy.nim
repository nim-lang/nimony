# CSE reads a copied set where it is instead of copying it.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_cse_set_copy.nim
# Expected: see topt_cse_set_copy.output
#
# `desugar` evaluates the set of `x in s` once by copying it into a local, which
# for a `set[char]` copies 32 bytes to read one. CSE forwards such a copy: its
# uses read the set itself while nothing can have changed it since, and the copy
# goes. These check the answers stay those of the copy around calls and stores
# that change the set.

import std / [syncio, assertions]

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

proc grow(st: var State): char {.noinline.} =
  inc st.calls
  st.cs.incl 'q'
  result = 'q'

func member(s: set[char]; c: char): bool = c in s
func memberAt(data: seq[set[char]]; i: int; c: char): bool = c in data[i]
func memberField(x: Holder; c: char): bool = c in x.cs
proc memberPtr(x: ptr Holder; c: char): bool = c in x.cs

proc main =
  let data = @[{'a'..'z'}, {'0'..'9'}]
  var h = Holder(cs: {'b'})
  echo member({'x', '\255'}, 'x'), member({'x', '\255'}, '\255'), member({'x'}, 'y')
  echo memberAt(data, 0, 'q'), memberAt(data, 1, '7'), memberAt(data, 1, 'q')
  echo memberField(h, 'b'), memberField(h, 'a'), memberPtr(addr h, 'b')

  # The set runs first, and once.
  var st = State(calls: 0, ch: 'a', cs: {}, h: Holder(cs: {'b'}))
  echo st.ch in byValue(st)
  st.ch = 'a'
  echo st.ch in holder(st)[].cs, " ", st.calls

  # The element's call changes the set.
  st.cs = {'a'}
  echo grow(st) in st.cs, " ", 'q' in st.cs

  # A store after each test does not reach back into it.
  st.cs = {'a'}
  var n = 0
  for ch in "aab":
    if ch in st.cs:
      inc n
      st.cs.incl 'b'
      st.cs.excl 'a'
  assert n == 2
  echo n, " ", st.cs == {'b'}

main()
