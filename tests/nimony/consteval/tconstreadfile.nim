import std/[syncio, strutils, assertions]

const version = readFile("doc/version.md").splitLines()[0]

echo "version: ", version
assert version == "Nimony 0.1.0-dev"
