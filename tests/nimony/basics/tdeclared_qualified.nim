# `declared(module.name)` used to crash nimsem with an unconsumed-children
# assertion (issue #2927).

import std/[syncio, assertions]

proc localProc() = discard

echo declared(system.missingSymbol)
echo declared(system.newSeq)
echo declared(syncio.echo)
echo declared(tdeclared_qualified.localProc)
echo declared(tdeclared_qualified.missingSymbol)
echo declared(missingModule.missingSymbol)
echo declared(missingSymbol)
echo declared(localProc)
