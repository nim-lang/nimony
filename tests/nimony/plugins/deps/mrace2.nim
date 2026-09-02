import std / syncio
{.plugin: "mraceplugin".}

proc race2*(): int =
  block:
    echo "must be erased 2"
  result = 2
