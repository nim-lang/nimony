import std/[assertions, syncio, smartcli]

# Commands without any positional arguments: once the command is chosen, every
# further positional argument is rejected.

block:
  let options = cliapp("""Svc v0.1
Controls the service.

Usage: svc [options] status|version

Commands:
  status   Show service status
  version  Show version and quit

Options:
  -h, --help  Show help and exit""",
    @["status"])

  assert options.command == cmdStatus
  echo "parsed status"

block:
  discard cliapp("""Svc v0.1
Controls the service.

Usage: svc [options] status|version

Commands:
  status   Show service status
  version  Show version and quit

Options:
  -h, --help  Show help and exit""",
    @["status", "extra"])

  echo "unreachable"
