import std/smartcli

discard cliapp("""Tool
Commands:
  install ITEM...  Install
Options:
  --item=VALUE  Item""", @["install", "a"])
