import std/[assertions, smartcli]

var
  nifcArgc {.importc: "cmdCount".}: int32
  nifcArgv {.importc: "cmdLine".}: ptr UncheckedArray[cstring]

block:
  nifcArgc = 3
  let cargv = [
    cstring"deploy",
    cstring"status",
    cstring"-v"
  ]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  let options = cliapp"""Deploy v0.1
Runs deployment tasks.

Usage: deploy [options] status|run|version

Commands:
  status             Show current deployment status
  run ENV TARGET     Execute a deployment
  version            Show version and quit

Options:
  --mode=fast|safe  Execution mode
  -v, --verbose     Enable verbose output
  -h, --help        Show help and exit"""

  assert $options.command == "cmdStatus"
  assert options.env == ""
  assert options.target == ""
  assert options.verbose

block:
  nifcArgc = 6
  let cargv = [
    cstring"deploy",
    cstring"run",
    cstring"--mode=safe",
    cstring"prod",
    cstring"--verbose",
    cstring"api"
  ]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  let options = cliapp"""Deploy v0.1
Runs deployment tasks.

Usage: deploy [options] status|run|version

Commands:
  status             Show current deployment status
  run ENV TARGET     Execute a deployment
  version            Show version and quit

Options:
  --mode=fast|safe  Execution mode
  -v, --verbose     Enable verbose output
  -h, --help        Show help and exit"""

  assert $options.command == "cmdRun"
  assert options.env == "prod"
  assert options.target == "api"
  assert $options.mode == "cliModeSafe"
  assert options.verbose

block:
  nifcArgc = 5
  let cargv = [
    cstring"deploy",
    cstring"--verbose",
    cstring"run",
    cstring"prod",
    cstring"api"
  ]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  let options = cliapp"""Deploy v0.1
Runs deployment tasks.

Usage: deploy [options] status|run|version

Commands:
  status 	Show current deployment status
  run ENV TARGET		Execute a deployment
  version		Show version and quit

Options:
  -v, --verbose 	Enable verbose output
  -h, --help		Show help and exit"""

  assert $options.command == "cmdRun"
  assert options.env == "prod"
  assert options.target == "api"
  assert options.verbose
