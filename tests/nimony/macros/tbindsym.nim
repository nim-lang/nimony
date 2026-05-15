# bindSym resolves a symbol in the *macro definition* scope at sem time and
# the plugin emits it as a NIF Symbol token, so the call site bypasses
# scope-based lookup.
import std / [syncio, macros]

# Uses bindSym to bind `succ` against the macro's def-site (system module).
# The macro emits `succ(41)` as a NimNode; sem at the call site sees the
# pre-resolved Symbol and skips name resolution.
macro nextOf(): untyped =
  let succSym = bindSym("succ")
  result = newCall(succSym, [newIntLitNode(41)])

echo nextOf()
