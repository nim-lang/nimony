#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements singly and doubly linked lists and their operations.
##
## This is the Nimony port. It provides `SinglyLinkedList[T]` and
## `DoublyLinkedList[T]`.
##
## Ownership and copying
## ----------------------
##
## The lists have **value semantics**, like `seq` and `string`: copying a list
## deep-copies its nodes, so the two values are independent. Each list uniquely
## owns its nodes through the `head` -> `next` -> ... chain; the `tail` pointer
## (and, in the doubly linked list, every `prev` pointer) is a non-owning
## `{.cursor.}` back-reference, so the structure has no reference cycle and is
## reclaimed by plain reference counting.
##
## Because of the unique-ownership invariant each list carries a custom,
## **non-recursive** `=destroy` hook: freeing a very long list walks the chain
## in a loop rather than recursing node by node, so it cannot overflow the
## stack. The hook frees the contained elements too.
##
## The ring variants of Nim 2's `std/lists` are deliberately omitted: a ring is
## a genuine ownership cycle (its last node's `next` refers back to the head),
## and Nimony reclaims memory by reference counting without a cycle collector,
## so a ring would leak. That needs a different design and is left for a
## follow-up.

type
  SinglyLinkedNode*[T] = ref object
    ## A single node of a `SinglyLinkedList`.
    next*: SinglyLinkedNode[T]
    value*: T
  SinglyLinkedList*[T] = object
    ## A singly linked list.
    head*: SinglyLinkedNode[T]
    tail* {.cursor.}: SinglyLinkedNode[T]

  DoublyLinkedNode*[T] = ref object
    ## A single node of a `DoublyLinkedList`.
    next*: DoublyLinkedNode[T]
    prev* {.cursor.}: DoublyLinkedNode[T]
    value*: T
  DoublyLinkedList*[T] = object
    ## A doubly linked list.
    head*: DoublyLinkedNode[T]
    tail* {.cursor.}: DoublyLinkedNode[T]

  Stringable = concept
    ## A type that can be rendered with `$`.
    func `$`(x: Self): string

  Equatable = concept
    ## A type whose values can be compared with `==`.
    func `==`(a, b: Self): bool

# ---------------------------------------------------------------------------
# Internal helpers.
#
# Everything that dereferences a node lives in ordinary procs, never directly
# inside a `=destroy`/`=dup`/`=copy` hook: dereferencing a generic `ref object`
# from within a lifetime hook currently trips a C-codegen bug, so the hooks
# only ever delegate here.
# ---------------------------------------------------------------------------

proc freeChain[T](head: sink SinglyLinkedNode[T]) =
  ## Frees an owning `head` -> `next` chain iteratively (non-recursive). Moving
  ## each successor out before destroying its predecessor keeps the per-node
  ## destructor from recursing into `next`.
  var it = head
  while it != nil:
    var nxt = move it.next
    `=destroy`(it)
    `=wasMoved`(it)
    it = move nxt

proc freeChain[T](head: sink DoublyLinkedNode[T]) =
  var it = head
  while it != nil:
    var nxt = move it.next
    `=destroy`(it)
    `=wasMoved`(it)
    it = move nxt

proc cloneList[T](src: SinglyLinkedList[T]): SinglyLinkedList[T] =
  result = default(SinglyLinkedList[T])
  var it {.cursor.} = src.head
  while it != nil:
    let n = SinglyLinkedNode[T](value: it.value)
    if result.head == nil: result.head = n
    else: result.tail.next = n
    result.tail = n
    it = it.next

proc cloneList[T](src: DoublyLinkedList[T]): DoublyLinkedList[T] =
  result = default(DoublyLinkedList[T])
  var it {.cursor.} = src.head
  while it != nil:
    let n = DoublyLinkedNode[T](value: it.value)
    n.prev = result.tail
    if result.head == nil: result.head = n
    else: result.tail.next = n
    result.tail = n
    it = it.next

# ---------------------------------------------------------------------------
# Lifetime hooks (value semantics; non-recursive teardown).
# ---------------------------------------------------------------------------

proc `=destroy`[T](x: SinglyLinkedList[T]) {.nodestroy.} =
  freeChain(x.head)

proc `=wasMoved`[T](x: var SinglyLinkedList[T]) {.nodestroy.} =
  x.head = nil
  x.tail = nil

proc `=dup`[T](src: SinglyLinkedList[T]): SinglyLinkedList[T] {.nodestroy.} =
  cloneList(src)

proc `=copy`[T](dst: var SinglyLinkedList[T]; src: SinglyLinkedList[T]) {.nodestroy.} =
  `=destroy`(dst)
  `=wasMoved`(dst)
  var tmp = cloneList(src)
  dst.head = move tmp.head
  dst.tail = tmp.tail
  `=wasMoved`(tmp)

proc `=destroy`[T](x: DoublyLinkedList[T]) {.nodestroy.} =
  freeChain(x.head)

proc `=wasMoved`[T](x: var DoublyLinkedList[T]) {.nodestroy.} =
  x.head = nil
  x.tail = nil

proc `=dup`[T](src: DoublyLinkedList[T]): DoublyLinkedList[T] {.nodestroy.} =
  cloneList(src)

proc `=copy`[T](dst: var DoublyLinkedList[T]; src: DoublyLinkedList[T]) {.nodestroy.} =
  `=destroy`(dst)
  `=wasMoved`(dst)
  var tmp = cloneList(src)
  dst.head = move tmp.head
  dst.tail = tmp.tail
  `=wasMoved`(tmp)

# ---------------------------------------------------------------------------
# SinglyLinkedList
# ---------------------------------------------------------------------------

func initSinglyLinkedList*[T](): SinglyLinkedList[T] =
  ## Creates a new empty singly linked list.
  result = default(SinglyLinkedList[T])

func newSinglyLinkedNode*[T](value: T): SinglyLinkedNode[T] =
  ## Creates a new singly linked node holding `value`.
  result = SinglyLinkedNode[T](value: value)

proc prepend*[T](L: var SinglyLinkedList[T], n: sink SinglyLinkedNode[T]) =
  ## Prepends node `n` to the front of `L`, taking ownership of it.
  n.next = L.head
  L.head = n
  if L.tail == nil: L.tail = L.head

proc prepend*[T](L: var SinglyLinkedList[T], value: T) =
  ## Prepends a new node holding `value` to the front of `L`.
  prepend(L, newSinglyLinkedNode(value))

proc append*[T](L: var SinglyLinkedList[T], n: sink SinglyLinkedNode[T]) =
  ## Appends node `n` to the back of `L`, taking ownership of it.
  n.next = nil
  if L.tail != nil: L.tail.next = n
  L.tail = n
  if L.head == nil: L.head = n

proc append*[T](L: var SinglyLinkedList[T], value: T) =
  ## Appends a new node holding `value` to the back of `L`.
  append(L, newSinglyLinkedNode(value))

proc add*[T](L: var SinglyLinkedList[T], value: T) =
  ## Alias for `append`.
  append(L, value)

iterator items*[T](L: SinglyLinkedList[T]): T =
  ## Yields every value in `L`, front to back.
  var it = L.head
  while it != nil:
    yield it.value
    it = it.next

iterator nodes*[T](L: SinglyLinkedList[T]): SinglyLinkedNode[T] =
  ## Yields every node in `L`, front to back. Safe to remove the yielded node
  ## while iterating (the successor is captured before it is yielded).
  var it = L.head
  while it != nil:
    let nxt = it.next
    yield it
    it = nxt

func find*[T: Equatable](L: SinglyLinkedList[T], value: T): SinglyLinkedNode[T] =
  ## Returns the first node whose value equals `value`, or `nil`.
  result = L.head
  while result != nil:
    if result.value == value: return result
    result = result.next

func contains*[T: Equatable](L: SinglyLinkedList[T], value: T): bool =
  ## Returns `true` when `value` is present in `L`.
  find(L, value) != nil

proc remove*[T](L: var SinglyLinkedList[T], n: SinglyLinkedNode[T]): bool =
  ## Removes node `n` from `L`. Returns `true` when `n` was found and unlinked.
  ## The unlinked node is freed unless the caller still holds a reference to it.
  result = false
  if n == nil: return
  # `self` takes the removed node out of the owning chain so that splicing the
  # gap shut cannot free it mid-operation; it is released when `self` goes out
  # of scope (freed only if no other reference remains).
  if L.head == n:
    var self = move L.head        # L.head = nil, self owns n
    L.head = move self.next       # L.head owns n's successor (nil if none)
    if L.head == nil: L.tail = nil
    self.next = nil
    return true
  var it {.cursor.} = L.head
  while it != nil and it.next != n:
    it = it.next
  if it != nil:
    var self = move it.next       # it.next = nil, self owns n
    it.next = move self.next      # it.next owns n's successor
    if L.tail == n: L.tail = it
    self.next = nil
    result = true

func `$`*[T: Stringable](L: SinglyLinkedList[T]): string =
  ## Returns a textual representation of `L`, e.g. `[1, 2, 3]`.
  result = "["
  var first = true
  var it {.cursor.} = L.head
  while it != nil:
    if not first: result.add ", "
    result.add $it.value
    first = false
    it = it.next
  result.add "]"

# ---------------------------------------------------------------------------
# DoublyLinkedList
# ---------------------------------------------------------------------------

func initDoublyLinkedList*[T](): DoublyLinkedList[T] =
  ## Creates a new empty doubly linked list.
  result = default(DoublyLinkedList[T])

func newDoublyLinkedNode*[T](value: T): DoublyLinkedNode[T] =
  ## Creates a new doubly linked node holding `value`.
  result = DoublyLinkedNode[T](value: value)

proc prepend*[T](L: var DoublyLinkedList[T], n: sink DoublyLinkedNode[T]) =
  ## Prepends node `n` to the front of `L`, taking ownership of it.
  n.prev = nil
  n.next = L.head
  if L.head != nil: L.head.prev = n
  L.head = n
  if L.tail == nil: L.tail = L.head

proc prepend*[T](L: var DoublyLinkedList[T], value: T) =
  ## Prepends a new node holding `value` to the front of `L`.
  prepend(L, newDoublyLinkedNode(value))

proc append*[T](L: var DoublyLinkedList[T], n: sink DoublyLinkedNode[T]) =
  ## Appends node `n` to the back of `L`, taking ownership of it.
  n.next = nil
  n.prev = L.tail
  if L.tail != nil: L.tail.next = n
  L.tail = n
  if L.head == nil: L.head = n

proc append*[T](L: var DoublyLinkedList[T], value: T) =
  ## Appends a new node holding `value` to the back of `L`.
  append(L, newDoublyLinkedNode(value))

proc add*[T](L: var DoublyLinkedList[T], value: T) =
  ## Alias for `append`.
  append(L, value)

iterator items*[T](L: DoublyLinkedList[T]): T =
  ## Yields every value in `L`, front to back.
  var it = L.head
  while it != nil:
    yield it.value
    it = it.next

iterator nodes*[T](L: DoublyLinkedList[T]): DoublyLinkedNode[T] =
  ## Yields every node in `L`, front to back. Safe to remove the yielded node
  ## while iterating (the successor is captured before it is yielded).
  var it = L.head
  while it != nil:
    let nxt = it.next
    yield it
    it = nxt

func find*[T: Equatable](L: DoublyLinkedList[T], value: T): DoublyLinkedNode[T] =
  ## Returns the first node whose value equals `value`, or `nil`.
  result = L.head
  while result != nil:
    if result.value == value: return result
    result = result.next

func contains*[T: Equatable](L: DoublyLinkedList[T], value: T): bool =
  ## Returns `true` when `value` is present in `L`.
  find(L, value) != nil

proc remove*[T](L: var DoublyLinkedList[T], n: DoublyLinkedNode[T]): bool =
  ## Removes node `n` from `L`. Returns `true` when `n` was unlinked. Runs in
  ## O(1): the node's own `prev`/`next` links locate its neighbours. The
  ## unlinked node is freed unless the caller still holds a reference to it.
  result = false
  if n == nil: return
  let p {.cursor.} = n.prev        # predecessor (non-owning), nil at head
  # Lift the node out of the owning chain into `self` first, so splicing the
  # gap shut cannot free it mid-operation.
  var self: DoublyLinkedNode[T]
  if p != nil: self = move p.next  # p.next = nil, self owns n
  else: self = move L.head         # L.head = nil, self owns n
  var s = move self.next           # s owns n's successor (nil if none)
  if s != nil: s.prev = p
  if L.tail == n: L.tail = p
  if p != nil: p.next = move s     # p.next owns the successor
  else: L.head = move s            # L.head owns the successor
  self.prev = nil
  self.next = nil
  result = true

func `$`*[T: Stringable](L: DoublyLinkedList[T]): string =
  ## Returns a textual representation of `L`, e.g. `[1, 2, 3]`.
  result = "["
  var first = true
  var it {.cursor.} = L.head
  while it != nil:
    if not first: result.add ", "
    result.add $it.value
    first = false
    it = it.next
  result.add "]"
