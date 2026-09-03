# Driver for the incremental suite's dependency-tracking phases. Both values
# below come from files this module does not import: one read by a plugin, one
# folded by `slurp`. The test machinery edits and restores those data files in
# place, so anything depending on their exact content lives in
# `src/hastur/incrementaltests.nim`.
import std/syncio

template showPluginData() {.plugin: "deps/mplugindep".}

const slurped = slurp("slurpdata.txt")

showPluginData()
echo slurped
