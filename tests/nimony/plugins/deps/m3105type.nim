type Marker* {.plugin: "midentitymodule".} = object
  value*: int

proc mkMarked*(): seq[Marker] = @[Marker(value: 7)]
