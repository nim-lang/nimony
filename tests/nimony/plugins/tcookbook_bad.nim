template checked(body: typed) {.plugin: "deps/mcookbook_check".}

proc blocked() = discard

checked:
  blocked()
