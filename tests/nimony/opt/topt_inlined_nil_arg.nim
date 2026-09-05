# A substituted `nil` must keep a type.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_inlined_nil_arg.nim
# Expected (see .output):
#     -1
#     7
#     1 nil ok set ok
#
# `nil` is the language's one type-polymorphic literal. The intra-module
# inliner substitutes literal arguments at every use instead of binding them to
# a `(var :p T arg)` copy — so whatever type the eliminated binding gave the
# value has to come from the value itself. It does: the frontend types every
# `nil` it emits (`trNil` in derefs.nim), so the splice carries `(nil T)` and
# the use site keeps a real pointer. Before that, splicing the bare `(nil)`
# handed the rest of the pipeline an untyped nil and a use that dereferences it
# reached the C backend as `(*NIM_NIL).field`: "member reference base type
# 'void' is not a structure or union". See nimony#2317.
#
# `getVal` is the shape that broke: an inlinable proc whose body reads through
# the reference parameter, called with a literal `nil`. `setNext` is the other
# direction — the substituted nil is *stored* into a reference field, so the
# assignment's destination type has to agree with it.

import std / [syncio]

type
  Node = ref object
    next: nil Node
    val: int

proc getVal(n: nil Node): int {.inline.} =
  if n == nil:
    result = -1
  else:
    result = n.val

proc setNext(n: Node; x: nil Node) {.inline.} =
  n.next = x

proc main =
  echo getVal(nil)
  let a = Node(val: 7)
  echo getVal(a)

  let head = Node(val: 1)
  setNext(head, nil)
  var parts = ""
  parts.add $head.val
  if head.next == nil: parts.add " nil ok"
  setNext(head, a)
  if head.next != nil: parts.add " set ok"
  echo parts

main()
