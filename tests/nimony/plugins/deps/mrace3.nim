import std / syncio
{.plugin: "mraceplugin".}

proc race3*(): int =
  block:
    echo "must be erased 3"
  result = 3
