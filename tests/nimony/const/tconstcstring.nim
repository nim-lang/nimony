# A `const` string converts to `cstring` like a string literal does (#3216).
import deps/mconstcstring

proc puts(s: cstring) {.importc: "puts", header: "<stdio.h>".}

const Greeting = "hello const"
const Combined = Greeting & " joined"
const Alias = Combined

puts "literal"
puts Greeting
puts Combined
puts Alias
puts Imported
let x: cstring = Greeting
puts x
proc wrap(s: cstring): cstring = s
puts wrap(Imported)
