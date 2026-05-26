# Test error reporting when resolving overloads while passing a module
# This used to crash in nifcursors.nim in `proc span` because the cursor was at the end.
import std/[syncio]

echo syncio
