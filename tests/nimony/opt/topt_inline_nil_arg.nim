# An inlinable proc called with a LITERAL nil argument, where the body
# assigns the param to a ref field: the assignment's lowered `=dup` guard
# derefs the (dead) nil. The inliner must not splice the bare `(nil)` into
# the body — nil is the one type-polymorphic literal, and the raw splice
# loses the type, so codegen emits `(*NIM_NIL).field` and the C does not
# compile. Regression for #2303's intramodinliner rewrite.
{.feature: "lenientnils".}
import std/syncio

type
  UINodeObj = object of RootObj
    focused: bool
  UINode = ref UINodeObj

  FocusManager = ref object
    focusedNode: UINode

method onFocusLeave(n: UINode) {.base.} = discard

proc setFocus(fm: FocusManager, node: UINode) =
  if fm.focusedNode == node:
    return
  if fm.focusedNode != nil:
    fm.focusedNode.focused = false
    fm.focusedNode.onFocusLeave()
  fm.focusedNode = node

proc clearFocus(fm: FocusManager) =
  fm.setFocus(nil)

proc main =
  var fm = FocusManager()
  fm.setFocus(UINode())
  fm.clearFocus()
  echo "ok"

main()
