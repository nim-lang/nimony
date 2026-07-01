#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a singly linked list and its operations.
##
## This is the Nimony port. It currently provides `SinglyLinkedList[T]` only.
## A singly linked list holds no reference cycles, so it needs no ownership
## annotations; the doubly linked and ring variants of Nim 2's `std/lists`
## depend on `{.cursor.}` (issue #1518) to break the `prev` back-reference
## cycle and are therefore left for a follow-up.
##
## The list stores `ref` nodes, so copying a `SinglyLinkedList` value shares the
## underlying nodes (the same aliasing behaviour as Nim's `lists`).

type
  SinglyLinkedNode*[T] = ref object
    ## A single node of a `SinglyLinkedList`.
    next*: SinglyLinkedNode[T]
    value*: T
  SinglyLinkedList*[T] = object
    ## A singly linked list.
    head*: SinglyLinkedNode[T]
    tail*: SinglyLinkedNode[T]

  Stringable = concept
    ## A type that can be rendered with `$`.
    func `$`(x: Self): string

  Equatable = concept
    ## A type whose values can be compared with `==`.
    func `==`(a, b: Self): bool

func initSinglyLinkedList*[T](): SinglyLinkedList[T] =
  ## Creates a new empty singly linked list.
  result = default(SinglyLinkedList[T])

func newSinglyLinkedNode*[T](value: T): SinglyLinkedNode[T] =
  ## Creates a new singly linked node holding `value`.
  result = SinglyLinkedNode[T](value: value)

proc prepend*[T](L: var SinglyLinkedList[T], n: SinglyLinkedNode[T]) =
  ## Prepends an existing node `n` to the front of `L`.
  n.next = L.head
  L.head = n
  if L.tail == nil: L.tail = n

proc prepend*[T](L: var SinglyLinkedList[T], value: T) =
  ## Prepends a new node holding `value` to the front of `L`.
  prepend(L, newSinglyLinkedNode(value))

proc append*[T](L: var SinglyLinkedList[T], n: SinglyLinkedNode[T]) =
  ## Appends an existing node `n` to the back of `L`.
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
  result = false
  if n == nil: return
  if L.head == n:
    L.head = n.next
    if L.tail == n: L.tail = nil
    n.next = nil
    return true
  var it = L.head
  while it != nil and it.next != n:
    it = it.next
  if it != nil:
    it.next = n.next
    if L.tail == n: L.tail = it
    n.next = nil
    result = true

func `$`*[T: Stringable](L: SinglyLinkedList[T]): string =
  ## Returns a textual representation of `L`, e.g. `[1, 2, 3]`.
  result = "["
  var first = true
  var it = L.head
  while it != nil:
    if not first: result.add ", "
    result.add $it.value
    first = false
    it = it.next
  result.add "]"
