type
  E = enum
    eNone, eA, eB, eC

const names: array[succ(low(E))..high(E), string] = ["a", "b", "c"]

discard names[eA]
