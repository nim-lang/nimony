# A variadic libc call from the native backend: SysV passes the double in xmm0
# and counts it in `al`; the output is buffered by C stdio, so it only appears if
# `main` returns to crt, whose `exit` flushes it.

proc printf(fmt: cstring): cint {.importc: "printf", varargs, discardable.}

printf("%d %.1f %s\n", 42, 1.5, cstring"libc")
