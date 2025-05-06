#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

##[

Check the code for nil-deref errors. Uses the CFG so that we get things correct without
too much trouble for `return` and other unstructured control flow. We do this before
derefs.nim so that we don't deal with lots of irrelevant hidden deref instructions.

]##

import std / [assertions]

include nifprelude

import ".." / models / tags
import nimony_model, programs, decls, typenav, sembasics, reporters, renderer, controlflow

proc checkForNil*(input: var TokenBuf) =
  let cfg = toControlflow(beginRead(input))

