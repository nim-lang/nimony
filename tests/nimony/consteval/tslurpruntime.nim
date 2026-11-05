import std/[syncio, assertions]

# `slurp` outside a `const` initializer used to crash nimsem. The contract
# pass folds ordinals without a `SemContext`, and the native `slurp` folding
# then dereferenced it once the file existed next to the source. Such a call
# is now left alone, so it runs `slurp`'s body: a runtime `readFile`, with
# the path relative to the working directory. Tests run from the repository
# root.

# Crashed nimsem: the path exists relative to this file.
discard slurp("tslurp_data.txt")

echo slurp("tests/nimony/consteval/tslurp_data.txt")
let s = slurp("tests/nimony/consteval/tslurp_data.txt")
assert s == "nimony 0.1.2\n"
echo s.len

proc main =
  echo slurp("tests/nimony/consteval/tslurp_data.txt").len
  echo slurp("does/not/exist.txt").len
  discard slurp("tslurp_data.txt")

main()
