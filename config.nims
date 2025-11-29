
task build, "Build Hastur and Nimony bins":
  exec "nim c -o:bin/hastur src/hastur.nim"
  exec "./bin/hastur build all"

