import std/[syncio, strutils, assertions, compilation]

const version = readFile("doc/version.md").splitLines()[0]

echo "version: ", version
assert version == "Nimony 0.1.0-dev"

# `staticRead` resolves a relative path against this file's directory too,
# not against the directory of `std/compilation`.
const staticVersion = staticRead("doc/version.md")
echo "staticRead: ", staticVersion.splitLines()[0]
