# A plugin called without arguments gets the call site as its input's
# location, so an error in its output is reported there, not at `???`.

template badCall() {.plugin: "deps/mbadcall".}

proc takesInt(x: int) = discard

badCall()
