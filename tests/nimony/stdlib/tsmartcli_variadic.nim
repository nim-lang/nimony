import std/[assertions, smartcli]

block:
  let options = cliapp("""Tool
Commands:
  install DEST ITEM...  Install items
  remove ITEM...        Remove items
  status                Show status
Options:
  -v, --verbose  Verbose""", @["install", "target", "one", "-v", "two", "three", "four"])
  assert options.command == cmdInstall
  assert options.dest == "target"
  assert options.item == @["one", "two", "three", "four"]
  assert options.verbose

block:
  let options = cliapp("""Tool
Arguments:
  FIRST    First
  REST...  Remaining""", @["a", "b", "c"])
  assert options.first == "a"
  assert options.rest == @["b", "c"]

block:
  let options = cliapp("""Tool
Commands:
  run  Run
Arguments:
  ITEM...  Items""", @["run", "one"])
  assert options.command == cmdRun
  assert options.item == @["one"]

block:
  let options = cliapp("""Tool
Commands:
  install ITEM-NAME...  Install
  remove ITEM_NAME...   Remove
  status                Status""", @["status"])
  assert options.command == cmdStatus
  assert options.itemName.len == 0
