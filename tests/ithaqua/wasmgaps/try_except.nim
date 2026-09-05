import std/[syncio, strutils]

try:
  echo parseInt("-0042")
except:
  echo "parse failed"
