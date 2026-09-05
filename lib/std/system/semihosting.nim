## ARM semihosting — the console a bare-metal image has when a debug agent is
## attached, written here rather than synthesized by the back end.
##
## The protocol is one instruction: `bkpt #0xAB` with an operation number in r0
## and, for every operation that needs more than a number, the address of a
## parameter block in r1. The agent — QEMU, or a probe — reads the registers,
## does the work on the host, and leaves the answer in r0.
##
## That single instruction is the only part a language cannot express, and it is
## the only part written in assembler here. Everything above it — the parameter
## blocks, the `:tt` handle the console is opened once under, the conversion from
## the protocol's "bytes NOT written" to POSIX's "bytes written" — is ordinary
## Nimony, where it can be read and changed without rebuilding a compiler.
##
## Included by `system.nim` under `--os:embedded`. It costs nothing on a target
## that has an OS to ask instead.

const
  SemiBkpt = 0xAB'i32
    ## The immediate that IS the semihosting call on M-profile. A-profile uses
    ## `svc 0x123456`; nothing here targets it.
  SysOpen = 0x01'i32
  SysWrite = 0x05'i32
  SysExitExtended = 0x20'i32
  AdpStoppedApplicationExit = 0x20026
    ## The `reason` an ordinary exit reports. `SYS_EXIT` (0x18) cannot carry a
    ## status on a 32-bit target — its r1 IS the reason code rather than a
    ## pointer — so the extended form is the only one that can say what the
    ## program returned.
  SysOpenModeW = 4'i32
    ## The "w" mode. `":tt"` opened with it is the console.

proc bkpt(imm: int32) {.instruction: "bkpt".}

proc semihostCall(op {.register: "r0".}: int32;
                  arg {.register: "r1".}: pointer): int32 {.assembler.} =
  ## THE semihosting primitive, and the whole of the assembler in this file.
  ##
  ## The body is one instruction because the ABI has already done the rest: the
  ## operation is in r0 and the parameter block address in r1 because that is
  ## where AAPCS32 puts the first two arguments, and the agent's answer is in r0
  ## because that is where a result is returned from. What `{.register.}` adds is
  ## the promise that this remains true — the pins are checked against the ABI,
  ## so a change to either side is a compile error rather than a call that traps
  ## with the wrong registers loaded.
  ##
  ## The row for `bkpt` cannot say that r0 comes back changed (no column
  ## describes a register), which is exactly why this is an `{.assembler.}` proc
  ## and not an `{.instruction.}` call in ordinary code.
  bkpt(SemiBkpt)

var
  ttyName: array[4, char] = [':', 't', 't', '\0']
    ## `":tt"`, NUL-terminated, as `SYS_OPEN` wants it. An array rather than a
    ## string literal: what the agent is handed is an ADDRESS, and a `var` array
    ## is the spelling whose address is a plain pointer to those four bytes.
  ttyHandle: int32 = 0
    ## The console, opened once. Zero means "not yet": a semihosting handle is
    ## never 0.

proc semihostConsole(): int32 =
  ## The handle `SYS_WRITE` needs, opening `:tt` on first use.
  ##
  ## Not a raw `1`. `SYS_WRITE`'s first field is a semihosting HANDLE, not a
  ## POSIX fd, and QEMU forgives the confusion while a hardware probe does not:
  ## passing 1 writes nothing AND reports success, because the call returns
  ## "0 bytes not written". Opening the console properly is what makes the same
  ## image work on both.
  if ttyHandle == 0:
    # Built as a literal rather than filled in field by field: the parameter
    # block is handed to the agent by ADDRESS, and an array whose elements were
    # assigned one at a time is not something the initialization analysis can
    # prove complete before its address escapes.
    var blk = [cast[int](addr ttyName[0]), int(SysOpenModeW), 3]
    ttyHandle = semihostCall(SysOpen, addr blk[0])
  result = ttyHandle

proc semihostWrite*(buf: pointer; n: int): int =
  ## `write(buf, n)` through the debug agent. Returns the number of bytes
  ## written, as POSIX `write` does.
  ##
  ## Semihosting reports the bytes it did NOT write (0 means everything went),
  ## so the conversion happens here — once, where the protocol is — and every
  ## caller stays an ordinary one.
  ##
  ## There is no `fd`: semihosting has ONE console, so stdout and stderr are the
  ## same stream and a file descriptor would be a distinction the transport
  ## cannot make.
  var blk = [int(semihostConsole()), cast[int](buf), n]
  let notWritten = int(semihostCall(SysWrite, addr blk[0]))
  result = n - notWritten

proc semihostExit*(code: int) {.noreturn.} =
  ## End the program, handing `code` to whoever is attached.
  ##
  ## A hosted `exit` gives its status to a parent process; here the debug agent
  ## is the only thing that can receive one, which is also why this is the
  ## semihosting build's `exit` and a board with no agent needs a different
  ## answer entirely (there is nowhere to send a status, and the honest end is to
  ## park the core).
  var blk = [AdpStoppedApplicationExit, code]
  discard semihostCall(SysExitExtended, addr blk[0])
  while true: discard        # SYS_EXIT does not return; this is for the compiler
