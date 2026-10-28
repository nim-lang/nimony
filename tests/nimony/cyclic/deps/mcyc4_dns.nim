# Deliberately WITHOUT `{.cyclic.}`: the build still classifies the module as
# part of the group through the other edges (chan -> stream -> dns), so this
# intra-group import has to be deferred anyway.
import mcyc4_chan

type
  Box* = object
    tx: Sender[int]

proc make*(): Box = Box(tx: newSender())

var scratch*: seq[int]
  ## A plain `seq` in the importing module: an instantiated `seq[char]` in a
  ## cyclic sibling must not make the name `seq` ambiguous here.
