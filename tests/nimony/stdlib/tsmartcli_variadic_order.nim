import std/smartcli

discard cliapp("""Tool
Arguments:
  ITEM...  Items
  DEST     Destination""", @["a", "b"])
