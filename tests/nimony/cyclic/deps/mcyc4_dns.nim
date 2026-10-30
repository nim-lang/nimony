import mcyc4_chan {.cyclic.}

type
  Box* = object
    tx: Sender[int]

proc make*(): Box = Box(tx: newSender())

var scratch*: seq[int]
  ## A plain `seq` in the importing module: an instantiated `seq[char]` in a
  ## cyclic sibling must not make the name `seq` ambiguous here.
