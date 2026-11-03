# A libc function that is not a system call: a freestanding native image has no
# loader to bind it, and must say so by its C name (nimony#2844).

proc atoi(s: cstring): cint {.importc: "atoi".}

discard atoi("42")
