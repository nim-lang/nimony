import std/syncio

when defined(argsFileRead):
  echo "args file read"
else:
  echo "args file ignored"
