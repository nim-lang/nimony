# Shared tag setup for this directory's tests.
#
# The HTTP tag pool is process-global and a joined group is one process, so
# with the registration written into each test the ids a test sees would
# depend on which member's module init happened to run first — a `walkDir` and
# a sort, which has nothing to do with what the tests mean.
#
# An imported module initialises once, before any of its importers, so
# registering here makes the group independent of that order. It is also the
# shape the design prescribes for an application: register the headers you
# index on in one place during init, before the first connection.
import std/http/httpmsg

let hTrace* = registerHeader("x-trace-id")
