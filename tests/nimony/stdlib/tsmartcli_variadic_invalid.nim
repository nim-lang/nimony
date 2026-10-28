import std/smartcli

discard cliapp("""Tool
Commands:
  install ITEM...  Install items
  remove ITEM      Remove one item""", @["install", "a"])
