import std/[assertions, smartcli]

# The command line is handed to `cliapp` explicitly rather than faked into the
# process argv: a Windows entry point receives no argv at all, so there is no
# such vector to write.

block:
  let options = cliapp("""Deploy v0.1
Runs deployment tasks.

Usage: deploy [options] status|run|version

Commands:
  status             Show current deployment status
  run ENV TARGET     Execute a deployment
  version            Show version and quit

Options:
  --mode=fast|safe  Execution mode
  -v, --verbose     Enable verbose output
  -h, --help        Show help and exit""",
    @["status", "-v"])

  assert $options.command == "cmdStatus"
  assert options.env == ""
  assert options.target == ""
  assert options.verbose

block:
  let options = cliapp("""Deploy v0.1
Runs deployment tasks.

Usage: deploy [options] status|run|version

Commands:
  status             Show current deployment status
  run ENV TARGET     Execute a deployment
  version            Show version and quit

Options:
  --mode=fast|safe  Execution mode
  -v, --verbose     Enable verbose output
  -h, --help        Show help and exit""",
    @["run", "--mode=safe", "prod", "--verbose", "api"])

  assert $options.command == "cmdRun"
  assert options.env == "prod"
  assert options.target == "api"
  assert $options.mode == "cliModeSafe"
  assert options.verbose

block:
  let options = cliapp("""Deploy v0.1
Runs deployment tasks.

Usage: deploy [options] status|run|version

Commands:
  status 	Show current deployment status
  run ENV TARGET		Execute a deployment
  version		Show version and quit

Options:
  -v, --verbose 	Enable verbose output
  -h, --help		Show help and exit""",
    @["--verbose", "run", "prod", "api"])

  assert $options.command == "cmdRun"
  assert options.env == "prod"
  assert options.target == "api"
  assert options.verbose
