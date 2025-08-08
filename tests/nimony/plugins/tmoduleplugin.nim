
import std / syncio

template generateEcho(s: string) = {.plugin: "deps/mplugin2".}

echo "should be erased"

generateEcho("Hello, world!")
