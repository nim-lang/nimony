import std/[syncio, assertions]

when defined(nimony):
  discard "good"
else:
  discard "bad"

when defined(undefinedDefine):
  discard "bad"
else:
  discard "good"

when defined(nimony) and not defined(undefinedDefine):
  discard "good"
else:
  discard "bad"

when defined(undefinedDefine) or not defined(nimony):
  discard "bad"
else:
  discard "good"

# `declared(module.name)` used to crash nimsem with an unconsumed-children
# assertion (issue #2927).

proc localProc() = discard

assert not declared(system.missingSymbol)
assert declared(system.newSeq)
assert declared(syncio.echo)
assert declared(tdefinedwhen.localProc)
assert not declared(tdefinedwhen.missingSymbol)
assert not declared(missingModule.missingSymbol)
assert not declared(missingSymbol)
assert declared(localProc)
