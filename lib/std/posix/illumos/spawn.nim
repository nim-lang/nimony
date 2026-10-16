# Private include. illumos <spawn.h>: both public handles contain one
# implementation-private pointer, not the internal libc allocation itself.
type
  SpawnAttr* {.pure.} = object
    impl: nil pointer
  SpawnFileActions* {.pure.} = object
    impl: nil pointer

const POSIX_SPAWN_SETPGROUP* = cshort(0x0002)

# Like all spawn operations, these return an error number, not -1/errno.
proc posix_spawn*(pid: var Pid; path: cstring;
                  actions: nil ptr SpawnFileActions; attr: nil ptr SpawnAttr;
                  argv, envp: CCharArray): cint {.importc: "posix_spawn", sideEffect.}
proc posix_spawn_file_actions_init*(actions: var SpawnFileActions): cint {.importc: "posix_spawn_file_actions_init".}
proc posix_spawn_file_actions_destroy*(actions: var SpawnFileActions): cint {.importc: "posix_spawn_file_actions_destroy".}
proc posix_spawn_file_actions_adddup2*(actions: var SpawnFileActions; oldfd, newfd: cint): cint {.importc: "posix_spawn_file_actions_adddup2".}
proc posix_spawn_file_actions_addclose*(actions: var SpawnFileActions; fd: cint): cint {.importc: "posix_spawn_file_actions_addclose".}
# The unsuffixed action is part of POSIX.1-2024 and available on the
# supported illumos baseline. Never emulate this with parent-side chdir.
proc posix_spawn_file_actions_addchdir*(actions: var SpawnFileActions; path: cstring): cint {.importc: "posix_spawn_file_actions_addchdir".}
proc posix_spawnattr_init*(attr: var SpawnAttr): cint {.importc: "posix_spawnattr_init".}
proc posix_spawnattr_destroy*(attr: var SpawnAttr): cint {.importc: "posix_spawnattr_destroy".}
proc posix_spawnattr_setflags*(attr: var SpawnAttr; flags: cshort): cint {.importc: "posix_spawnattr_setflags".}
proc posix_spawnattr_setpgroup*(attr: var SpawnAttr; group: Pid): cint {.importc: "posix_spawnattr_setpgroup".}
proc pipe2*(fds: ptr cint; flags: cint): cint {.importc: "pipe2", sideEffect.}
