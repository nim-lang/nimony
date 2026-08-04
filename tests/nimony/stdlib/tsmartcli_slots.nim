import std/[assertions, smartcli]

# The command line is handed to `cliapp` explicitly rather than faked into the
# process argv: a Windows entry point receives no argv at all, so there is no
# such vector to write.

block:
  let options = cliapp("""Backup v0.1
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
  -h, --help         Show this help and exit""",
    @["--mode=delta", "--output=backup.log", "run", "src", "dst"])

  assert options.source == "src"
  assert options.dest == "dst"
  assert options.output == "backup.log"
  assert $options.mode == "cliModeDelta"
  assert $options.command == "cmdRun"

block:
  let options = cliapp("""Backup v0.1
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
  -h, --help         Show this help and exit""",
    @["run", "--mode=full", "src", "--output=after.log", "dst", "-v"])

  assert options.source == "src"
  assert options.dest == "dst"
  assert options.output == "after.log"
  assert options.verbose
  assert $options.mode == "cliModeFull"
  assert $options.command == "cmdRun"
