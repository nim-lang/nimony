import std/syncio

# The program that evaluates a `const` (`exprexec`) is checked beside the
# declarations the evaluated code uses. The names that program introduces
# itself must not be ones the user's code can declare too.

type E = enum dest, other   # `dest` was the serializer's parameter

proc pair(): array[2, E] = [other, dest]

const ce = pair()
echo ce[0], " ", ce[1]
