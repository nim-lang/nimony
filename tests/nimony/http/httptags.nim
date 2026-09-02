# Shared tag setup for this directory's tests.
#
# The HTTP tag pool is process-global and sealed exactly once, and a joined
# group is one process. With the registration written into each test, whichever
# member's module init ran first fixed the vocabulary for all of them — and the
# one that sealed without registering `x-trace-id` made every later
# registration the defect `registerHeader` is right to reject. Which member
# goes first is a `walkDir` and a sort, which has nothing to do with what the
# tests mean.
#
# An imported module initialises once, before any of its importers, so
# registering here makes the group independent of that order. It is also the
# shape the design prescribes for an application: register the headers you
# index on in one place during init, then seal.
import std/http/httpmsg

let hTrace* = registerHeader("x-trace-id")
sealHttpTags()
