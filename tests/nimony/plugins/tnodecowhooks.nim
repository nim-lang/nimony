import std / syncio

template checkNodeCow() {.plugin: "deps/mnodecowhooks".}

checkNodeCow()
