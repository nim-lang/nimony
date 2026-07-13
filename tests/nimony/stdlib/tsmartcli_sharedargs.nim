import std/[assertions, smartcli]

var
  nifcArgc {.importc: "cmdCount".}: int32
  nifcArgv {.importc: "cmdLine".}: ptr UncheckedArray[cstring]

block:
  nifcArgc = 5
  let cargv = [
    cstring"greeter",
    cstring"--output=out.txt",
    cstring"-v",
    cstring"greet",
    cstring"input.txt"
  ]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  let options = cliapp"""Greeter v0.1
This program greets.

Usage: greeter [options] greet|version INPUT

Commands:
  greet    Greets NAME
  version  Displays version and quits

Arguments:
  INPUT  Input file

Options:
  --mode=fast|slow  Output mode
  --output=FILE     Output file
  -v, --verbose     Enable verbose output
  -h, --help        Show this help and exit"""

  assert options.input == "input.txt"
  assert options.output == "out.txt"
  assert options.verbose
  assert $options.command == "cmdGreet"

block:
  nifcArgc = 4
  let cargv = [
    cstring"greeter",
    cstring"--mode=slow",
    cstring"greet",
    cstring"input.txt"
  ]
  nifcArgv = cast[ptr UncheckedArray[cstring]](cargv.addr)

  let options = cliapp"""Greeter v0.1
This program greets.

Usage: greeter [options] greet|version INPUT

Commands:
  greet    Greets NAME
  version  Displays version and quits

Arguments:
  INPUT  Input file

Options:
  --mode=fast|slow  Output mode
  --output=FILE     Output file
  -v, --verbose     Enable verbose output
  -h, --help        Show this help and exit"""

  assert options.input == "input.txt"
  assert $options.mode == "cliModeSlow"
