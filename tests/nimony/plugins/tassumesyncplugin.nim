import std / syncio

{.feature: "assumeSync".}
{.plugin: "deps/massumesyncplugin".}

var counter = 1

proc bump() =
  inc counter

bump()
echo counter
