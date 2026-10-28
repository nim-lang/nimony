import std/smartcli

discard cliapp("""Tool
Commands:
  install ITEM... DEST  Install items""", @["install", "a", "b"])
