
import std / syncio

template eraseToplevelBlocks() = {.plugin: "deps/mmoduleplugin".}

echo "this should not be erased"

block:
  echo "should be erased"

eraseToplevelBlocks()
