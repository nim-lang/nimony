
import std / syncio
import deps/mhiddenplugin

eraseToplevelBlocks()

echo "this should not be erased"

block:
  echo "should be erased"
