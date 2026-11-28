# A `const` string converts to `cstring` like a string literal does (#3216).
import std/syncio
import deps/mconstcstring

proc show(s: cstring) =
  var r = ""
  var i = 0
  while s[i] != '\0':
    r.add s[i]
    inc i
  echo r

const Greeting = "hello const"
const Combined = Greeting & " joined"
const Alias = Combined

show "literal"
show Greeting
show Combined
show Alias
show Imported
let x: cstring = Greeting
show x
proc wrap(s: cstring): cstring = s
show wrap(Imported)
