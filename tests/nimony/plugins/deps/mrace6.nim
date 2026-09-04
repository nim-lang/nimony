import std / syncio
{.plugin: "mraceplugin".}

proc race6*(): int =
  block:
    echo "must be erased 6"
  result = 6
