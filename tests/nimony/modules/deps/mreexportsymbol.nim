# re-exports one symbol of `mreexportsymbol_source`, not the module
import mreexportsymbol_source as owners

type Stream* = owners.Conn
proc open*(): Stream = owners.newConn()

export owners.value
