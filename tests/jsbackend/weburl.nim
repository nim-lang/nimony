## GENERATED from @webref/idl (url.idl) by gen/idl2nim.js — do not edit by hand.
## Regenerate: node gen/idl2nim.js url URL weburl.nim
##
## A jsffi binding for the WHATWG/W3C `URL` interface. Each member
## marshals through jsffi exactly as the hand-written dom.nim does.
import jsffi

type
  URL* = JsValue

proc newURL*(url: string): URL =
  newOf("URL", [toJs(url)])

proc newURL*(url: string; base: string): URL =
  newOf("URL", [toJs(url), toJs(base)])

proc href*(self: URL): string = $self.get("href")
proc `href=`*(self: URL; value: string) = self.set("href", toJs(value))

proc origin*(self: URL): string = $self.get("origin")

proc protocol*(self: URL): string = $self.get("protocol")
proc `protocol=`*(self: URL; value: string) = self.set("protocol", toJs(value))

proc username*(self: URL): string = $self.get("username")
proc `username=`*(self: URL; value: string) = self.set("username", toJs(value))

proc password*(self: URL): string = $self.get("password")
proc `password=`*(self: URL; value: string) = self.set("password", toJs(value))

proc host*(self: URL): string = $self.get("host")
proc `host=`*(self: URL; value: string) = self.set("host", toJs(value))

proc hostname*(self: URL): string = $self.get("hostname")
proc `hostname=`*(self: URL; value: string) = self.set("hostname", toJs(value))

proc port*(self: URL): string = $self.get("port")
proc `port=`*(self: URL; value: string) = self.set("port", toJs(value))

proc pathname*(self: URL): string = $self.get("pathname")
proc `pathname=`*(self: URL; value: string) = self.set("pathname", toJs(value))

proc search*(self: URL): string = $self.get("search")
proc `search=`*(self: URL; value: string) = self.set("search", toJs(value))

proc searchParams*(self: URL): JsValue = self.get("searchParams")

proc hash*(self: URL): string = $self.get("hash")
proc `hash=`*(self: URL; value: string) = self.set("hash", toJs(value))

proc toJSON*(self: URL): string = $self.call("toJSON")

## SKIPPED (not yet generated — extend gen/idl2nim.js to cover):
##   - static op parse
##   - static op canParse
