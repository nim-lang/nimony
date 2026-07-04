## Nim -> JS callbacks: register a Nim proc as a JS event handler on a real host
## `EventTarget` (the DOM event base class), dispatch an `Event`, and have the
## handler fire back into Nim and read the event's properties. This is the
## mechanism a DOM binding's `addEventListener` sits on.
import std/syncio
import jsffi

var fired = 0

proc onPing(ev: JsValue) =
  inc fired
  echo "Nim handler fired (" & $fired & "); event.type = " & ev.get("type").toStr

let target = newOf("EventTarget")
discard target.call("addEventListener", toJs("ping"), toJs(onPing))

# Dispatch twice — proves the same Nim proc keeps firing and state persists.
discard target.call("dispatchEvent", newOf("Event", toJs("ping")))
discard target.call("dispatchEvent", newOf("Event", toJs("ping")))

echo "after dispatch, fired = " & $fired
