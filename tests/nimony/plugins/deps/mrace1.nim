import std / syncio
{.plugin: "mraceplugin".}

proc race1*(): int =
  block:
    echo "must be erased 1"
  result = 1
