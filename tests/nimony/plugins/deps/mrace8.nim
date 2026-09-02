import std / syncio
{.plugin: "mraceplugin".}

proc race8*(): int =
  block:
    echo "must be erased 8"
  result = 8
