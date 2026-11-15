{.feature: "staticContracts".}

#
#
#            Nim's Runtime Library
#        (c) Copyright 2010 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains support for a `rope`:idx: data type.
## Ropes can represent very long strings efficiently; in particular, concatenation
## is done in O(1) instead of O(n). They are essentially concatenation
## trees that are only flattened when converting to a native Nim
## string. The empty string is represented by `nil`. Ropes are immutable and
## subtrees can be shared without copying.
## Leaves can be cached for better memory efficiency at the cost of
## runtime efficiency.

import std/[assertions, streams, syncio]

const
  countCacheMisses = false

var
  cacheEnabled {.threadvar.}: bool

type
  RopeObj {.acyclic.} = object
    left, right: Rope
    length: int
    data: string # not empty if a leaf
  Rope* = nil ref RopeObj
    ## A rope data type. The empty rope is represented by `nil`.

# Note that the left and right pointers are not needed for leafs.
# Leaves have relatively high memory overhead (~30 bytes on a 32
# bit machine) and we produce many of them. This is why we cache and
# share leafs across different rope trees.
# To cache them they are inserted in another tree, a splay tree for best
# performance. But for the caching tree we use the leaf's left and right
# pointers.

func len*(a: Rope): int =
  ## The rope's length.
  if a == nil: 0 else: a.length

func newRope(): Rope =
  Rope(left: nil, right: nil, length: 0, data: "")

func newRope(data: string): Rope =
  Rope(left: nil, right: nil, length: data.len, data: data)

var
  cache {.threadvar.}: Rope # the root of the cache tree
  N {.threadvar.}: Rope     # dummy rope needed for splay algorithm

when countCacheMisses:
  var misses, hits: int

proc splay(s: string, tree: Rope, cmpres: var int): Rope =
  if tree == nil:
    cmpres = 0
    return nil
  if N == nil:
    N = newRope()
  let n = N
  if n == nil:
    cmpres = 0
    return nil
  var c: int = 0
  var t = tree
  n.left = nil
  n.right = nil # reset to nil
  var le = n
  var r = n
  while t != nil:
    c = cmp(s, t.data)
    if c < 0:
      let tl = t.left
      if tl != nil and s < tl.data:
        let y = tl
        t.left = y.right
        y.right = t
        t = y
      let next = t.left
      if next == nil: break
      if r == nil: break
      r.left = t
      r = t
      t = next
    elif c > 0:
      let tr = t.right
      if tr != nil and s > tr.data:
        let y = tr
        t.right = y.left
        y.left = t
        t = y
      let next = t.right
      if next == nil: break
      if le == nil: break
      le.right = t
      le = t
      t = next
    else:
      break
  if t == nil or le == nil or r == nil:
    return nil
  cmpres = c
  le.right = t.left
  r.left = t.right
  t.left = n.right
  t.right = n.left
  result = t

proc insertInCache(s: string, tree: Rope): Rope =
  var t = tree
  if t == nil:
    result = newRope(s)
    when countCacheMisses: inc(misses)
    return
  var cmp: int = 0
  let sp = splay(s, t, cmp)
  if sp == nil:
    result = newRope(s)
    return
  t = sp
  if cmp == 0:
    # We get here if it's already in the Tree
    # Don't add it again
    result = t
    when countCacheMisses: inc(hits)
  else:
    when countCacheMisses: inc(misses)
    let r = newRope(s)
    if r == nil:
      result = nil
      return
    if cmp < 0:
      r.left = t.left
      r.right = t
      t.left = nil
    else:
      # i > t.item:
      r.right = t.right
      r.left = t
      t.right = nil
    result = r

proc rope*(s: string = ""): Rope =
  ## Converts a string to a rope.
  runnableExamples:
    let r = rope("I'm a rope")
    doAssert $r == "I'm a rope"

  if s.len == 0:
    result = nil
  else:
    if cacheEnabled:
      result = insertInCache(s, cache)
      if result != nil:
        cache = result
    else:
      result = newRope(s)

proc rope*(i: BiggestInt): Rope =
  ## Converts an int to a rope.
  runnableExamples:
    let r = rope(429)
    doAssert $r == "429"

  result = rope($i)

proc rope*(f: BiggestFloat): Rope =
  ## Converts a float to a rope.
  runnableExamples:
    let r = rope(4.29)
    doAssert $r == "4.29"

  result = rope($f)

proc enableCache*() =
  ## Enables the caching of leaves. This reduces the memory footprint at
  ## the cost of runtime efficiency.
  cacheEnabled = true

proc disableCache*() =
  ## The cache is discarded and disabled. The GC will reuse its used memory.
  cache = nil
  cacheEnabled = false

func `&`*(a, b: Rope): Rope =
  ## The concatenation operator for ropes.
  runnableExamples:
    let r = rope("Hello, ") & rope("Nim!")
    doAssert $r == "Hello, Nim!"

  if a == nil:
    result = b
  elif b == nil:
    result = a
  else:
    result = Rope(left: a, right: b, length: a.length + b.length, data: "")

proc `&`*(a: Rope, b: string): Rope =
  ## The concatenation operator for ropes.
  runnableExamples:
    let r = rope("Hello, ") & "Nim!"
    doAssert $r == "Hello, Nim!"

  result = a & rope(b)

proc `&`*(a: string, b: Rope): Rope =
  ## The concatenation operator for ropes.
  runnableExamples:
    let r = "Hello, " & rope("Nim!")
    doAssert $r == "Hello, Nim!"

  result = rope(a) & b

func `&`*(a: openArray[Rope]): Rope =
  ## The concatenation operator for an `openArray` of ropes.
  runnableExamples:
    let r = &[rope("Hello, "), rope("Nim"), rope("!")]
    doAssert $r == "Hello, Nim!"
  result = nil
  for item in a: result = result & item

proc add*(a: var Rope, b: Rope) =
  ## Adds `b` to the rope `a`.
  runnableExamples:
    var r = rope("Hello, ")
    r.add(rope("Nim!"))
    doAssert $r == "Hello, Nim!"

  a = a & b

proc add*(a: var Rope, b: string) =
  ## Adds `b` to the rope `a`.
  runnableExamples:
    var r = rope("Hello, ")
    r.add("Nim!")
    doAssert $r == "Hello, Nim!"

  a = a & b

func `[]`*(r: Rope, i: int): char =
  ## Returns the character at position `i` in the rope `r`. This is quite
  ## expensive! Worst-case: O(n). If `i >= r.len or i < 0`, `\0` is returned.
  runnableExamples:
    let r = rope("Hello, Nim!")

    doAssert r[0] == 'H'
    doAssert r[7] == 'N'
    doAssert r[22] == '\0'
  result = '\0'
  var x = r
  var j = i
  if x == nil or i < 0 or i >= r.len: return
  while x != nil:
    if x.data.len > 0:
      # leaf
      let data = x.data
      {.assume: 0 <= j and j < data.len.}
      return data[j]
    else:
      let left = x.left
      if left == nil:
        return
      if left.length > j:
        x = left
      else:
        let right = x.right
        if right == nil:
          return
        dec(j, left.length)
        x = right

iterator leaves*(r: Rope): string =
  ## Iterates over any leaf string in the rope `r`.
  runnableExamples:
    let r = rope("Hello") & rope(", Nim!")
    let s = ["Hello", ", Nim!"]
    var index = 0
    for leave in r.leaves:
      doAssert leave == s[index]
      inc(index)

  if r != nil:
    var stack = @[r]
    while stack.len > 0:
      var it = stack.pop
      while it != nil:
        let left = it.left
        if left == nil:
          break
        let right = it.right
        assert right != nil
        if right == nil:
          break
        stack.add(right)
        it = left
      if it != nil:
        yield it.data

iterator items*(r: Rope): char =
  ## Iterates over any character in the rope `r`.
  for s in leaves(r):
    for c in items(s): yield c

proc write*(f: File, r: Rope) =
  ## Writes a rope to a file.
  for s in leaves(r): write(f, s)

proc write*(s: Stream, r: Rope) {.raises.} =
  ## Writes a rope to a stream.
  for rs in leaves(r): write(s, rs)

func `$`*(r: Rope): string =
  ## Converts a rope back to a string.
  result = newStringOfCap(r.len)
  for s in leaves(r): add(result, s)

proc `%`*(frmt: string, args: openArray[Rope]): Rope {.raises.} =
  ## `%` substitution operator for ropes. Does not support the `$identifier`
  ## nor `${identifier}` notations.
  runnableExamples:
    let r1 = "$1 $2 $3" % [rope("Nim"), rope("is"), rope("a great language")]
    doAssert $r1 == "Nim is a great language"

    let r2 = "$# $# $#" % [rope("Nim"), rope("is"), rope("a great language")]
    doAssert $r2 == "Nim is a great language"

    let r3 = "${1} ${2} ${3}" % [rope("Nim"), rope("is"), rope("a great language")]
    doAssert $r3 == "Nim is a great language"

  var i = 0
  result = nil
  var num = 0
  while i < frmt.len:
    {.assume: 0 <= i and i < frmt.len.}
    if frmt[i] == '$':
      inc(i)
      if i >= frmt.len:
        raise ValueError
      {.assume: 0 <= i and i < frmt.len.}
      case frmt[i]
      of '$':
        add(result, "$")
        inc(i)
      of '#':
        inc(i)
        if num >= args.len:
          raise ValueError
        add(result, args[num])
        inc(num)
      of '0'..'9':
        var j = 0
        while true:
          {.assume: 0 <= i and i < frmt.len.}
          j = j * 10 + ord(frmt[i]) - ord('0')
          inc(i)
          if i >= frmt.len or frmt[i] notin {'0'..'9'}: break
        if j < 1 or j > args.len:
          raise ValueError
        add(result, args[j-1])
      of '{':
        inc(i)
        var j = 0
        while i < frmt.len and frmt[i] in {'0'..'9'}:
          {.assume: 0 <= i and i < frmt.len.}
          j = j * 10 + ord(frmt[i]) - ord('0')
          inc(i)
        if i >= frmt.len or frmt[i] != '}':
          raise ValueError
        inc(i)
        if j < 1 or j > args.len:
          raise ValueError
        add(result, args[j-1])
      else: raise ValueError
    let start = i
    while i < frmt.len:
      {.assume: 0 <= i and i < frmt.len.}
      if frmt[i] != '$': inc(i)
      else: break
    if i > start:
      add(result, substr(frmt, start, i - 1))

proc addf*(c: var Rope, frmt: string, args: openArray[Rope]) {.raises.} =
  ## Shortcut for `add(c, frmt % args)`.
  runnableExamples:
    var r = rope("Dash: ")
    r.addf "$1 $2 $3", [rope("Nim"), rope("is"), rope("a great language")]
    doAssert $r == "Dash: Nim is a great language"

  add(c, frmt % args)

when not defined(js) and not defined(nimscript):
  const
    bufSize = 1024 # 1 KB is reasonable

  proc equalsFile*(r: Rope, f: File): bool =
    ## Returns true if the contents of the file `f` equal `r`.
    var
      buf {.noinit.}: array[bufSize, char]
      bpos = buf.len
      blen = buf.len

    for s in leaves(r):
      var spos = 0
      let slen = s.len
      while spos < slen:
        if bpos == blen:
          # Read more data
          bpos = 0
          blen = readBuffer(f, addr(buf[0]), buf.len)
          if blen == 0: # no more data in file
            return false
        let n = min(blen - bpos, slen - spos)
        {.assume: 0 <= bpos and bpos < buf.len.}
        # TODO: There's gotta be a better way of comparing here...
        if cmpMem(addr(buf[bpos]), cast[pointer](readRawData(s, spos)), n) != 0:
          return false
        spos += n
        bpos += n

    result = readBuffer(f, addr(buf[0]), 1) == 0 # check that we've read all

  proc equalsFile*(r: Rope, filename: string): bool =
    ## Returns true if the contents of the file `f` equal `r`. If `f` does not
    ## exist, false is returned.
    var f: File
    result = open(f, filename)
    if result:
      result = equalsFile(r, f)
      close(f)
