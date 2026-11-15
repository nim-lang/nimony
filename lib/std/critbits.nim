{.feature: "staticContracts".}

#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a `crit bit tree`:idx: which is an efficient
## container for a sorted set of strings, or for a sorted mapping of strings. Based on the
## [excellent paper by Adam Langley](https://www.imperialviolet.org/binary/critbit.pdf).
## (A crit bit tree is a form of `radix tree`:idx: or `patricia trie`:idx:.)

import std/assertions

type
  NodeObj[T] {.acyclic.} = object
    byte: int ## byte index of the difference
    otherBits: char
    case isLeaf: bool
    of false: child: array[2, nil Node[T]]
    of true:
      key: string
      when T isnot void:
        val: T

  Node[T] = ref NodeObj[T]
  CritBitTree*[T] = object ## The crit bit tree can either be used
                           ## as a mapping from strings to
                           ## some type `T` or as a set of
                           ## strings if `T` is `void`.
    root: nil Node[T]
    count: int

func len*[T](c: CritBitTree[T]): int {.inline, ensures: result == c.count.} =
  ## Returns the number of elements in `c` in O(1).
  runnableExamples:
    let c = ["key1", "key2"].toCritBitTree
    doAssert c.len == 2

  result = c.count

func byteAt(s: string; i: int): char {.inline.} =
  ## Returns byte `i`, or `'\0'` when it is past either end.
  if i >= 0 and i < s.len:
    s[i]
  else:
    '\0'

func rawGet[T](c: CritBitTree[T], key: string): nil Node[T] =
  result = nil
  var it = c.root
  while it != nil:
    if not it.isLeaf:
      let ch = byteAt(key, it.byte)
      let dir = ((1 + (ch.ord or it.otherBits.ord)) shr 8) and 1
      it = it.child[dir]
    else:
      return if it.key == key: it else: nil

func contains*[T](c: CritBitTree[T], key: string): bool {.inline.} =
  ## Returns true if `c` contains the given `key`.
  runnableExamples:
    var c: CritBitTree[void]
    incl(c, "key")
    doAssert c.contains("key")

  result = rawGet(c, key) != nil

func hasKey*[T](c: CritBitTree[T], key: string): bool {.inline.} =
  ## Alias for `contains <#contains,CritBitTree[T],string>`_.
  result = rawGet(c, key) != nil

# The set and map insertions duplicate the tree algorithm because a generic
# `Node[T](...)` constructor cannot be instantiated at `T = void`.
proc rawInsert(c: var CritBitTree[void], key: string): Node[void] =
  if c.root == nil:
    let n = Node[void](isLeaf: true, key: key)
    c.root = n
    inc c.count
    return n

  var it = c.root
  var leafParent: nil Node[void] = nil
  var leafParentDir: range[0..1] = 0
  while it != nil and not it.isLeaf:
    let ch = byteAt(key, it.byte)
    let dir = ((1 + (ch.ord or it.otherBits.ord)) shr 8) and 1
    leafParent = it
    leafParentDir = dir
    it = it.child[dir]

  if it == nil:
    # A tree built by this module never has a nil child. If one is seen,
    # attach the new leaf at the last valid position instead of dereferencing
    # a nil node.
    let n = Node[void](isLeaf: true, key: key)
    if leafParent != nil:
      leafParent.child[leafParentDir] = n
    else:
      c.root = n
    inc c.count
    return n

  let leaf = it

  var newOtherBits = 0
  var newByte = 0
  block blockX:
    while newByte < key.len:
      let ch = byteAt(leaf.key, newByte)
      if ch != byteAt(key, newByte):
        newOtherBits = ch.ord xor byteAt(key, newByte).ord
        break blockX
      inc newByte
    if newByte < leaf.key.len:
      newOtherBits = byteAt(leaf.key, newByte).ord
    else:
      return leaf
  while (newOtherBits and (newOtherBits-1)) != 0:
    newOtherBits = newOtherBits and (newOtherBits-1)
  newOtherBits = newOtherBits xor 255
  let ch = byteAt(leaf.key, newByte)
  let dir = ((1 + (ord(ch) or newOtherBits)) shr 8) and 1

  assert newOtherBits >= 0 and newOtherBits <= 255
  let newLeaf = Node[void](isLeaf: true, key: key)
  let inner = Node[void](isLeaf: false, byte: newByte,
                         otherBits: chr(newOtherBits), child: [nil, nil])
  inner.child[1 - dir] = newLeaf

  var parent: nil Node[void] = nil
  var parentDir: range[0..1] = 0
  var q = c.root
  while q != nil:
    if q.isLeaf: break
    if q.byte > newByte: break
    if q.byte == newByte and q.otherBits.ord > newOtherBits: break
    let edgeCh = byteAt(key, q.byte)
    let nextDir = ((1 + (edgeCh.ord or q.otherBits.ord)) shr 8) and 1
    parent = q
    parentDir = nextDir
    q = q.child[nextDir]
  inner.child[dir] = q
  if parent != nil:
    parent.child[parentDir] = inner
  else:
    c.root = inner
  inc c.count
  result = newLeaf

proc rawInsert[T: not void](c: var CritBitTree[T], key: string): Node[T] =
  if c.root == nil:
    let n = Node[T](isLeaf: true, key: key)
    c.root = n
    inc c.count
    return n

  var it = c.root
  var leafParent: nil Node[T] = nil
  var leafParentDir: range[0..1] = 0
  while it != nil and not it.isLeaf:
    let ch = byteAt(key, it.byte)
    let dir = ((1 + (ch.ord or it.otherBits.ord)) shr 8) and 1
    leafParent = it
    leafParentDir = dir
    it = it.child[dir]

  if it == nil:
    # A tree built by this module never has a nil child. If one is seen,
    # attach the new leaf at the last valid position instead of dereferencing
    # a nil node.
    let n = Node[T](isLeaf: true, key: key)
    if leafParent != nil:
      leafParent.child[leafParentDir] = n
    else:
      c.root = n
    inc c.count
    return n

  let leaf = it

  var newOtherBits = 0
  var newByte = 0
  block blockX:
    while newByte < key.len:
      let ch = byteAt(leaf.key, newByte)
      if ch != byteAt(key, newByte):
        newOtherBits = ch.ord xor byteAt(key, newByte).ord
        break blockX
      inc newByte
    if newByte < leaf.key.len:
      newOtherBits = byteAt(leaf.key, newByte).ord
    else:
      return leaf
  while (newOtherBits and (newOtherBits-1)) != 0:
    newOtherBits = newOtherBits and (newOtherBits-1)
  newOtherBits = newOtherBits xor 255
  let ch = byteAt(leaf.key, newByte)
  let dir = ((1 + (ord(ch) or newOtherBits)) shr 8) and 1

  assert newOtherBits >= 0 and newOtherBits <= 255
  let newLeaf = Node[T](isLeaf: true, key: key)
  let inner = Node[T](isLeaf: false, byte: newByte,
                      otherBits: chr(newOtherBits), child: [nil, nil])
  inner.child[1 - dir] = newLeaf

  var parent: nil Node[T] = nil
  var parentDir: range[0..1] = 0
  var q = c.root
  while q != nil:
    if q.isLeaf: break
    if q.byte > newByte: break
    if q.byte == newByte and q.otherBits.ord > newOtherBits: break
    let edgeCh = byteAt(key, q.byte)
    let nextDir = ((1 + (edgeCh.ord or q.otherBits.ord)) shr 8) and 1
    parent = q
    parentDir = nextDir
    q = q.child[nextDir]
  inner.child[dir] = q
  if parent != nil:
    parent.child[parentDir] = inner
  else:
    c.root = inner
  inc c.count
  result = newLeaf

proc exclImpl[T](c: var CritBitTree[T], key: string): int =
  var p = c.root
  if p == nil: return c.count
  var parent: nil Node[T] = nil
  var parentDir: range[0..1] = 0
  var grandParent: nil Node[T] = nil
  var grandParentDir: range[0..1] = 0
  while p != nil and not p.isLeaf:
    let ch = byteAt(key, p.byte)
    let dir = ((1 + (ch.ord or p.otherBits.ord)) shr 8) and 1
    grandParent = parent
    grandParentDir = parentDir
    parent = p
    parentDir = dir
    p = p.child[dir]
  if p != nil and p.key == key:
    # Drop the leaf's parent and promote its other child. The link to the
    # parent comes from its own parent, not from the parent itself.
    if parent == nil:
      c.root = nil
    else:
      let sibling = parent.child[(1 - parentDir) and 1]
      if grandParent == nil:
        c.root = sibling
      else:
        grandParent.child[grandParentDir] = sibling
    dec c.count

  return c.count

proc excl*[T](c: var CritBitTree[T], key: string) =
  ## Removes `key` (and its associated value) from the set `c`.
  ## If the `key` does not exist, nothing happens.
  ##
  ## **See also:**
  ## * `incl proc <#incl,CritBitTree[void],string>`_
  ## * `incl proc <#incl,CritBitTree[T],string,T>`_
  runnableExamples:
    var c: CritBitTree[void]
    incl(c, "key")
    excl(c, "key")
    doAssert not c.contains("key")

  discard exclImpl(c, key)

proc missingOrExcl*[T](c: var CritBitTree[T], key: string): bool =
  ## Returns true if `c` does not contain the given `key`. If the key
  ## does exist, `c.excl(key)` is performed.
  ##
  ## **See also:**
  ## * `excl proc <#excl,CritBitTree[T],string>`_
  ## * `containsOrIncl proc <#containsOrIncl,CritBitTree[T],string,T>`_
  ## * `containsOrIncl proc <#containsOrIncl,CritBitTree[void],string>`_
  runnableExamples:
    block:
      var c: CritBitTree[void]
      doAssert c.missingOrExcl("key")
    block:
      var c: CritBitTree[void]
      incl(c, "key")
      doAssert not c.missingOrExcl("key")
      doAssert not c.contains("key")

  let oldCount = c.count
  discard exclImpl(c, key)
  result = c.count == oldCount

proc containsOrIncl*[T: not void](c: var CritBitTree[T], key: string, val: sink T): bool =
  ## Returns true if `c` contains the given `key`. If the key does not exist,
  ## `c[key] = val` is performed.
  ##
  ## **See also:**
  ## * `incl proc <#incl,CritBitTree[void],string>`_
  ## * `incl proc <#incl,CritBitTree[T],string,T>`_
  ## * `containsOrIncl proc <#containsOrIncl,CritBitTree[void],string>`_
  ## * `missingOrExcl proc <#missingOrExcl,CritBitTree[T],string>`_
  runnableExamples:
    block:
      var c: CritBitTree[int]
      doAssert not c.containsOrIncl("key", 42)
      doAssert c.contains("key")
    block:
      var c: CritBitTree[int]
      incl(c, "key", 21)
      doAssert c.containsOrIncl("key", 42)
      doAssert c["key"] == 21

  let oldCount = c.count
  let n = rawInsert(c, key)
  result = c.count == oldCount
  if not result:
    n.val = val

proc containsOrIncl*(c: var CritBitTree[void], key: string): bool =
  ## Returns true if `c` contains the given `key`. If the key does not exist,
  ## it is inserted into `c`.
  ##
  ## **See also:**
  ## * `incl proc <#incl,CritBitTree[void],string>`_
  ## * `incl proc <#incl,CritBitTree[T],string,T>`_
  ## * `containsOrIncl proc <#containsOrIncl,CritBitTree[T],string,T>`_
  ## * `missingOrExcl proc <#missingOrExcl,CritBitTree[T],string>`_
  runnableExamples:
    block:
      var c: CritBitTree[void]
      doAssert not c.containsOrIncl("key")
      doAssert c.contains("key")
    block:
      var c: CritBitTree[void]
      incl(c, "key")
      doAssert c.containsOrIncl("key")

  let oldCount = c.count
  discard rawInsert(c, key)
  result = c.count == oldCount

proc inc*(c: var CritBitTree[int]; key: string, val: int = 1) =
  ## Increments `c[key]` by `val`.
  runnableExamples:
    var c: CritBitTree[int]
    c["key"] = 1
    inc(c, "key")
    doAssert c["key"] == 2

  let n = rawInsert(c, key)
  inc n.val, val

proc incl*(c: var CritBitTree[void], key: string) =
  ## Includes `key` in `c`.
  ##
  ## **See also:**
  ## * `excl proc <#excl,CritBitTree[T],string>`_
  ## * `incl proc <#incl,CritBitTree[T],string,T>`_
  runnableExamples:
    var c: CritBitTree[void]
    incl(c, "key")
    doAssert c.hasKey("key")

  discard rawInsert(c, key)

proc incl*[T: not void](c: var CritBitTree[T], key: string, val: sink T) =
  ## Inserts `key` with value `val` into `c`.
  ##
  ## **See also:**
  ## * `excl proc <#excl,CritBitTree[T],string>`_
  ## * `incl proc <#incl,CritBitTree[void],string>`_
  runnableExamples:
    var c: CritBitTree[int]
    incl(c, "key", 42)
    doAssert c["key"] == 42

  let n = rawInsert(c, key)
  n.val = val

proc `[]=`*[T: not void](c: var CritBitTree[T], key: string, val: sink T) =
  ## Alias for `incl <#incl,CritBitTree[T],string,T>`_.
  ##
  ## **See also:**
  ## * `[] proc <#[],CritBitTree[T],string>`_
  ## * `[] proc <#[],CritBitTree[T],string_2>`_
  let n = rawInsert(c, key)
  n.val = val

func getNode[T](c: CritBitTree[T], key: string): Node[T] {.raises.} =
  let n = rawGet(c, key)
  if n == nil:
    raise KeyError
  n

proc `[]`*[T](c: CritBitTree[T], key: string): var T {.inline, raises.} =
  ## Retrieves the value at `c[key]`. The value can be modified.
  ## If `key` is not in `t`, the `KeyError` exception is raised. One can
  ## check with `hasKey` whether the key exists.
  ##
  ## **See also:**
  ## * `[]= proc <#[]=,CritBitTree[T],string,T>`_
  getNode(c, key).val

iterator leaves[T](n: nil Node[T]): Node[T] =
  if n != nil:
    # XXX actually we could compute the necessary stack size in advance:
    # it's roughly log2(c.count).
    var stack = @[n]
    while stack.len > 0:
      var it = stack.pop
      while it != nil:
        if it.isLeaf: break
        stack.add(it.child[1])
        it = it.child[0]
      if it != nil:
        yield it

iterator keys*[T](c: CritBitTree[T]): string =
  ## Yields all keys in lexicographical order.
  for x in leaves(c.root): yield x.key

iterator values*[T](c: CritBitTree[T]): lent T =
  ## Yields all values of `c` in the lexicographical order of the
  ## corresponding keys.
  ##
  ## **See also:**
  ## * `mvalues iterator <#mvalues.i,CritBitTree[T]>`_
  for x in leaves(c.root): yield x.val

iterator mvalues*[T](c: var CritBitTree[T]): var T {.sideEffect.} =
  ## Yields all values of `c` in the lexicographical order of the
  ## corresponding keys. The values can be modified.
  ##
  ## **See also:**
  ## * `values iterator <#values.i,CritBitTree[T]>`_
  for x in leaves(c.root): yield x.val

iterator items*[T](c: CritBitTree[T]): string =
  ## Alias for `keys <#keys.i,CritBitTree[T]>`_.
  for x in leaves(c.root): yield x.key

iterator pairs*[T](c: CritBitTree[T]): tuple[key: string, val: T] =
  ## Yields all `(key, value)`-pairs of `c` in the lexicographical order of the
  ## corresponding keys.
  ##
  ## **See also:**
  ## * `mpairs iterator <#mpairs.i,CritBitTree[T]>`_
  for x in leaves(c.root): yield (x.key, x.val)

iterator mpairs*[T](c: var CritBitTree[T]): tuple[key: string, val: var T] {.sideEffect.} =
  ## Yields all `(key, value)`-pairs of `c` in the lexicographical order of the
  ## corresponding keys. The yielded values can be modified.
  ##
  ## **See also:**
  ## * `pairs iterator <#pairs.i,CritBitTree[T]>`_
  for x in leaves(c.root): yield (x.key, x.val)

func allprefixedAux[T](c: CritBitTree[T], key: string): nil Node[T] =
  result = nil
  var p = c.root
  var top = p
  while p != nil:
    if p.isLeaf: break
    let qbyte = p.byte
    let ch = byteAt(key, p.byte)
    let dir = ((1 + (ch.ord or p.otherBits.ord)) shr 8) and 1
    p = p.child[dir]
    if qbyte < key.len: top = p
  if p != nil:
    for i in 0 ..< key.len:
      if i >= p.key.len or p.key[i] != key[i]: return
    result = top

iterator keysWithPrefix*[T](c: CritBitTree[T], prefix: string): string =
  ## Yields all keys starting with `prefix`.
  let top = allprefixedAux(c, prefix)
  for x in leaves(top): yield x.key

iterator valuesWithPrefix*[T](c: CritBitTree[T], prefix: string): lent T =
  ## Yields all values of `c` starting with `prefix` of the
  ## corresponding keys.
  ##
  ## **See also:**
  ## * `mvaluesWithPrefix iterator <#mvaluesWithPrefix.i,CritBitTree[T],string>`_
  let top = allprefixedAux(c, prefix)
  for x in leaves(top): yield x.val

iterator mvaluesWithPrefix*[T](c: var CritBitTree[T], prefix: string): var T {.sideEffect.} =
  ## Yields all values of `c` starting with `prefix` of the
  ## corresponding keys. The values can be modified.
  ##
  ## **See also:**
  ## * `valuesWithPrefix iterator <#valuesWithPrefix.i,CritBitTree[T],string>`_
  let top = allprefixedAux(c, prefix)
  for x in leaves(top): yield x.val

iterator itemsWithPrefix*[T](c: CritBitTree[T], prefix: string): string =
  ## Alias for `keysWithPrefix <#keysWithPrefix.i,CritBitTree[T],string>`_.
  let top = allprefixedAux(c, prefix)
  for x in leaves(top): yield x.key

iterator pairsWithPrefix*[T](c: CritBitTree[T],
                             prefix: string): tuple[key: string, val: T] =
  ## Yields all (key, value)-pairs of `c` starting with `prefix`.
  ##
  ## **See also:**
  ## * `mpairsWithPrefix iterator <#mpairsWithPrefix.i,CritBitTree[T],string>`_
  let top = allprefixedAux(c, prefix)
  for x in leaves(top): yield (x.key, x.val)

iterator mpairsWithPrefix*[T](c: var CritBitTree[T],
                              prefix: string): tuple[key: string, val: var T] {.sideEffect.} =
  ## Yields all (key, value)-pairs of `c` starting with `prefix`.
  ## The yielded values can be modified.
  ##
  ## **See also:**
  ## * `pairsWithPrefix iterator <#pairsWithPrefix.i,CritBitTree[T],string>`_
  let top = allprefixedAux(c, prefix)
  for x in leaves(top): yield (x.key, x.val)

func `$`*[T](c: CritBitTree[T]): string =
  ## Turns `c` into a string representation.
  runnableExamples:
    doAssert $CritBitTree[int].default == "{:}"
    doAssert $toCritBitTree({"key1": 1, "key2": 2}) == """{"key1": 1, "key2": 2}"""
    doAssert $CritBitTree[void].default == "{}"
    doAssert $toCritBitTree(["key1", "key2"]) == """{"key1", "key2"}"""

  if c.len == 0:
    when T is void:
      result = "{}"
    else:
      result = "{:}"
  else:
    # an educated guess is better than nothing:
    var avgItemLen = 16
    when T is void:
      avgItemLen = 8
    result = newStringOfCap(c.count * avgItemLen)
    result.add("{")
    var first = true
    when T is void:
      for key in keys(c):
        if first: first = false
        else: result.add(", ")
        result.addQuoted(key)
    else:
      for key, val in pairs(c):
        if first: first = false
        else: result.add(", ")
        result.addQuoted(key)
        result.add(": ")
        result.addQuoted(val)
    result.add("}")

func commonPrefixLen*[T](c: CritBitTree[T]): int {.inline, ensures: result >= 0.} =
  ## Returns the length of the longest common prefix of all keys in `c`.
  ## If `c` is empty, returns 0.
  runnableExamples:
    var c: CritBitTree[void]
    doAssert c.commonPrefixLen == 0
    incl(c, "key1")
    doAssert c.commonPrefixLen == 4
    incl(c, "key2")
    doAssert c.commonPrefixLen == 3

  let root = c.root
  if root != nil:
    if root.isLeaf: result = len(root.key)
    else: result = root.byte
  else:
    result = 0

proc toCritBitTree*[T: not void](pairs: sink openArray[(string, T)]): CritBitTree[T] =
  ## Creates a new `CritBitTree` that contains the given `pairs`.
  runnableExamples:
    doAssert {"a": "0", "b": "1", "c": "2"}.toCritBitTree is CritBitTree[string]
    doAssert {"a": 0, "b": 1, "c": 2}.toCritBitTree is CritBitTree[int]

  result = default(CritBitTree[T])
  for item in pairs:
    let n = rawInsert(result, item[0])
    n.val = item[1]

proc toCritBitTree*(items: sink openArray[string]): CritBitTree[void] =
  ## Creates a new `CritBitTree` that contains the given `items`.
  runnableExamples:
    doAssert ["a", "b", "c"].toCritBitTree is CritBitTree[void]
  result = default(CritBitTree[void])
  for item in items: result.incl item
