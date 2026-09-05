import std / syncio
{.plugin: "mraceplugin".}

proc race4*(): int =
  block:
    echo "must be erased 4"
  result = 4
