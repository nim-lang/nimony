
import std/syncio

type
  CustomBase* {.inheritable.} = object

method `=destroy`(b: CustomBase) = discard
method `=trace`(b: var CustomBase; p: pointer) = discard

method reportIdentity*(b: CustomBase) =
  echo "CustomBase"
