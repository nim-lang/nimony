type
  E1 = enum value1=4, value2

# Out of order keys in constructor
let Lookuptable = [
  value2: "1",
  value1: "2"
]
let Lookuptable2 = [
  value1: "1",
  value2: "2"
]

let Lookuptable3 = [
  1: "1",
  5: "2"
]