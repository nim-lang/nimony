## Imported by `optionuser.nim` and not by `optionother.nim`: its sem output
## depends on a define, so it must be re-semmed whenever a build with other
## defines reaches it, whatever builds ran in the nimcache in between.
proc probe*(): string =
  when defined(incrementalStale):
    result = "probe-on"
  else:
    result = "probe-off"
