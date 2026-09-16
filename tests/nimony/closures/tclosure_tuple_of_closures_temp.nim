# A tuple OF closures handed straight from a call into a parameter. The
# duplifier binds the call's result to a temp so it can be destroyed after
# the call; typenav answers the call's type from the callee's semchecked
# signature, `(tuple (proctype … closure) …)`, never lowered here. The temp,
# and the tuple's =destroy hook, must use the lowered layout the returned
# value has — a struct of (fn, env) pairs — not a struct of bare function
# pointers, or the C compiler rejects the program (hikaru's
# buildVolume3DCallbacks).
import std/syncio

type
  Ui = ref object
    scale: float32
    hits: int

  Callbacks = tuple[
    onDrag: proc(x, y: float32) {.closure.},
    onWheel: proc(delta: float32): bool {.closure.},
    isActive: proc(): bool {.closure.}]

proc buildCallbacks(ui: Ui): Callbacks =
  let onDrag = proc(x, y: float32) {.closure.} =
    ui.hits += 1
    echo "drag ", x * ui.scale, " ", y * ui.scale
  let onWheel = proc(delta: float32): bool {.closure.} =
    ui.hits += 1
    delta * ui.scale > 1.0'f32
  let isActive = proc(): bool {.closure.} = ui.hits > 0
  result = (onDrag: onDrag, onWheel: onWheel, isActive: isActive)

proc install(cbs: Callbacks) =
  cbs.onDrag(1.0, 2.0)
  echo cbs.onWheel(3.0)
  echo cbs.isActive()

proc main() =
  let ui = Ui(scale: 2.0)
  install(buildCallbacks(ui))
  echo ui.hits

main()
echo "ok"
