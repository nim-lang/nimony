import std/syncio

# Regression: a closure PARAM whose own parameter is itself a closure
# (`proc(fn: proc() {.closure.}) {.closure.}`). Upstream #2074's env==nil
# else-branch de-closures the callee via `toNonClosureProcType` and calls it
# bare; for a closure-of-closure parameter the reconstructed cast cannot match
# the tuple-lowered argument, so the (dead, for a genuine closure) else-branch
# emitted a type-invalid call the C backend rejected
# ("incompatible type … void (*)(void)"). Skip the nil-check when the callee has
# a closure-typed parameter — nothing passes a bare non-closure proc to such a
# parameter — and use the plain tuple-unpack call.

proc caller(deferFn: proc(fn: proc() {.closure.}) {.closure.}) =
  deferFn(proc () {.closure.} = echo "inner")

proc make(): proc(fn: proc() {.closure.}) {.closure.} =
  let tag = "outer"
  result = proc(fn: proc() {.closure.}) {.closure.} =
    echo tag
    fn()

caller(make())
