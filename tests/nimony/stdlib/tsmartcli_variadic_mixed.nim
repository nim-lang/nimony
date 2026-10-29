import std/smartcli

discard cliapp("""Tool
Commands:
  remove ITEM      Remove
  install ITEM...  Install""", @["install", "one", "two"])
