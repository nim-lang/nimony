import std/[assertions, smartcli]

var
  nifcArgc {.importc: "cmdCount".}: int32
  nifcArgv {.importc: "cmdLine".}: ptr UncheckedArray[cstring]

block:
  nifcArgc = 6
  let cargv = [
    cstring"backup",
    cstring"--mode=delta",
    cstring"--output=backup.log",
    cstring"run",
    cstring"src",
    cstring"dst"
  ]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  let options = cliapp"""Backup v0.1
Copies files to a target.

Usage: backup [options] (run SOURCE DEST | version)

Commands:
  run SOURCE DEST  Start the backup
  version  Display version and quit

Arguments:
  SOURCE  Source path
  DEST    Destination path

Options:
  --mode=full|delta  Backup mode
  --output=FILE      Log file
  -v, --verbose      Enable verbose output
  -h, --help         Show this help and exit"""

  assert options.source == "src"
  assert options.dest == "dst"
  assert options.output == "backup.log"
  assert $options.mode == "cliModeDelta"
  assert $options.command == "cmdRun"

block:
  nifcArgc = 7
  let cargv = [
    cstring"backup",
    cstring"run",
    cstring"--mode=full",
    cstring"src",
    cstring"--output=after.log",
    cstring"dst",
    cstring"-v"
  ]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  let options = cliapp"""Backup v0.1
Copies files to a target.

Usage: backup [options] (run SOURCE DEST | version)

Commands:
  run SOURCE DEST  Start the backup
  version  Display version and quit

Arguments:
  SOURCE  Source path
  DEST    Destination path

Options:
  --mode=full|delta  Backup mode
  --output=FILE      Log file
  -v, --verbose      Enable verbose output
  -h, --help         Show this help and exit"""

  assert options.source == "src"
  assert options.dest == "dst"
  assert options.output == "after.log"
  assert options.verbose
  assert $options.mode == "cliModeFull"
  assert $options.command == "cmdRun"
