# Private implementation included by std/posix/posix; not a standalone module.

# arm64 overrides the asm-generic value: its 0o200000 slot is O_DIRECT.
# Using the x86 value here makes opendir fail on arm64.
const O_DIRECTORY = cint(0o40000)
