import std / syncio
{.plugin: "mraceplugin".}

proc race7*(): int =
  block:
    echo "must be erased 7"
  result = 7
