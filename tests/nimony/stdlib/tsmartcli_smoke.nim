import std/[assertions, smartcli]

# The command line is handed to `cliapp` explicitly rather than faked into the
# process argv: a Windows entry point receives no argv at all, so there is no
# such vector to write.

block:
  let options = cliapp("""Greeter v0.1
This program greets.

Usage: greeter [options] (greet INPUT | version)

Commands:
  greet INPUT  Greets NAME
  version  Displays version and quits

Arguments:
  INPUT  Input file

Options:
  --mode=fast|slow  Output mode
  --output=FILE     Output file
  -v, --verbose     Enable verbose output
  -h, --help        Show this help and exit""",
    @["--output=out.txt", "-v", "greet", "input.txt"])

  assert options.input == "input.txt"
  assert options.output == "out.txt"
  assert options.verbose
  assert $options.command == "cmdGreet"

block:
  let options = cliapp("""Greeter v0.1
This program greets.

Usage: greeter [options] (greet INPUT | version)

Commands:
  greet INPUT  Greets NAME
  version  Displays version and quits

Arguments:
  INPUT  Input file

Options:
  --mode=fast|slow  Output mode
  --output=FILE     Output file
  -v, --verbose     Enable verbose output
  -h, --help        Show this help and exit""",
    @["--mode=slow", "greet", "input.txt"])

  assert options.input == "input.txt"
  assert $options.mode == "cliModeSlow"

block:
  let options = cliapp("""Greeter v0.1
This program greets.

Usage: greeter [options] (greet INPUT | version)

Commands:
  greet INPUT  Greets NAME
  version  Displays version and quits

Arguments:
  INPUT  Input file

Options:
  --mode=fast|slow  Output mode
  --output=FILE     Output file
  -v, --verbose     Enable verbose output
  -h, --help        Show this help and exit""",
    @["greet", "--output=late.txt", "input.txt", "-v"])

  assert options.input == "input.txt"
  assert options.output == "late.txt"
  assert options.verbose
  assert $options.command == "cmdGreet"
