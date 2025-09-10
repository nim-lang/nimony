
import std / syncio

{.plugin: "deps/mmoduleplugin".}

echo "this should not be erased"

block:
  echo "should be erased"
