import std / syncio

template dumpRepr(s: typed) {.plugin: "deps/mtreerepr".}

dumpRepr:
  discard 1
