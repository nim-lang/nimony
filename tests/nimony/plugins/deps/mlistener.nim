
type
  Listener* {.plugin: "mtypeplugin".} = object
    i*: int
    s*: string

proc mkListeners*(): seq[Listener] = @[Listener(i: 7, s: "first")]
