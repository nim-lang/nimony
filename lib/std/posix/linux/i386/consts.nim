# Private implementation included by std/posix/posix; not a standalone module.

# O_DIRECTORY differs between AArch64 and x86.
const O_DIRECTORY = cint(0o200000)
