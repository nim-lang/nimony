import std / syncio
{.plugin: "mraceplugin".}

proc race5*(): int =
  block:
    echo "must be erased 5"
  result = 5
