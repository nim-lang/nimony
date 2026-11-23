# nim-lang/nimony#3105: a module plugin that changes nothing keeps a valid
# module valid, also when it uses a generic instance made in another module.
import std / [syncio, tables]
import deps / m3105

{.plugin: "deps/midentitymodule".}

proc main() =
  let s = mk()
  var t = initTable[string, string]()
  t["a"] = "b"
  echo s.len, " ", t.len

main()
