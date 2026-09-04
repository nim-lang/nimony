# closure call through an object FIELD (dispatcher pattern)
import std/syncio

type
  Event = object
    code: int
  Handler = object
    handler: proc (e: Event) {.closure.}
  Dispatcher = object
    handlers: seq[Handler]

var total = 0

proc register(d: var Dispatcher; h: proc (e: Event) {.closure.}) =
  d.handlers.add Handler(handler: h)

proc dispatch(d: Dispatcher; e: Event) =
  for h in d.handlers:
    h.handler(e)

var d = Dispatcher(handlers: @[])
register(d, proc (e: Event) {.closure.} = total = total + e.code)
dispatch(d, Event(code: 3))
echo total
