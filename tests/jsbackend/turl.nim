## Exercises `weburl.nim` — a binding GENERATED from the official WebIDL
## (@webref/idl url.idl) by gen/idl2nim.js. Proves that spec-generated bindings
## compile and run end-to-end over the JS backend. `URL` is a native Node global,
## so this needs no DOM environment.
import std/syncio
import jsffi
import weburl

let u = newURL("https://user:pw@example.com:8080/a/b?x=1&y=2#frag")
echo "protocol: ", u.protocol
echo "hostname: ", u.hostname
echo "port: ", u.port
echo "pathname: ", u.pathname
echo "search: ", u.search
echo "hash: ", u.hash
echo "username: ", u.username

# a read-write attribute set, then read the recomputed href back
u.hash = "#updated"
echo "href: ", u.href

# an interface-typed attribute (URLSearchParams) reached as a JsValue, then a
# jsffi method call on it
echo "searchParams x: ", $u.searchParams.call("get", toJs("x"))

# a generated operation
echo "json: ", u.toJSON()
