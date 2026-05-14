# Nimony-compiled mirror of tnodetreestress.nim. The plugin source switches
# `import nimonyplugins` → `import nim3plugins`; the `.plugin` pragma selects
# the nimony compiler via the version tuple.
import std / syncio

template stressNodeTree(s: string) {.plugin: ("deps/mnodetreestress_nim3", "nimony").}

stressNodeTree("payload")
