# illumos POSIX and event ports

The initial ABI transcription supports **illumos/amd64 (LP64)**. Other CPUs
are rejected until their layouts have been verified; Oracle Solaris is not
implicitly treated as illumos. All illumos configurations link libc. The
freestanding native backend and `nimNoLibc` are unsupported.

`std/posix/posix` includes the platform declarations. `std/posix/event_port`
is the public facade for event ports and their POSIX AIO notification API;
consumers should not import the implementation includes directly. A future
Solaris port can supply a separate implementation behind this facade.

Socket declarations use the X/Open ABI and its `__xnet_*` entry points.
Directory entries are copied from libc records into a synthetic `Dirent`:
illumos has no native `d_type`, so `std/dirs` uses its existing `lstat` fallback.

## I/O ring

The illumos backend combines `PORT_SOURCE_FD` readiness with `PORT_SOURCE_AIO`
completion notifications. Each reactor lane owns its port and mutable state.
As elsewhere in the ring, cancellation/close must occur on the submitting
lane, and only one foreign (non-worker) submitter may drive its shared lane.

- `submitRead`/`submitWrite` operate on nonblocking streams. Blocking streams
  are rejected with `-EINVAL`. Regular files follow the shared ring's
  synchronous transfer path, which may block the polling lane.
- On illumos, `submitReadAt`/`submitWriteAt` accept explicit file offsets and
  use POSIX AIO for regular files. Other file types return `-ENOTSUP`.
  An explicit position avoids an unsafe `lseek`/read emulation on a shared
  open-file description. POSIX AIO does not promise to preserve that
  description's current offset; do not mix it with implicit-position I/O.
- AIO admission is bounded to `MaxOps` physical requests and 64 MiB of staging
  memory per lane. Resource pressure returns `-EAGAIN`; there is no blocking
  synchronous fallback. libc AIO may itself use worker threads.
- Writes copy into backend-owned memory before submission. Reads copy out
  only on successful, still-live completion. The caller's buffer must remain
  valid until its logical completion, including cancellation or timeout.
- Control blocks, notification records, staging memory, and duplicated file
  descriptors remain alive until the physical completion event is consumed.
  Cancellation may lose to I/O: a timed-out/cancelled write can still modify
  the file. It cannot access a caller buffer released after completion.
- Shutdown stops pool workers first, then cancels and drains outstanding AIO
  before closing ports. It can wait for an uncancellable operation to finish.

FD association cookies reject stale events after deregistration/reuse. AIO
slot generations and suppression flags reject late completion of a logically
cancelled request. Each accepted AIO request is reaped exactly once.

## Validation

`tests/nimony/stdlib/tposixabi.nim` checks header constants, layouts, wait
macros, pthread widths, and event-port/AIO records. The C probe uses
`_XOPEN_SOURCE=600`, `__EXTENSIONS__`, and `-m64` on illumos; the host C compiler
may otherwise default to ILP32.

`teventport.nim` tests the public low-level interface. `teventportaio.nim`
tests the ring's positioned I/O, errors, deadlines, cancellation/reuse, TCP,
and shutdown. Existing readiness/timer tests also exercise this backend.

Illumos always uses the native allocator (`nimNativeAlloc`). `-d:useLibc`
selects libc stdio but retains that allocator; `-d:useLibcIo` does the same.
Explicit `-d:useMimalloc` is rejected, including when combined with other
allocator flags. This policy is independent of libc linkage: all illumos
builds link libc, and freestanding mode remains unsupported.

`tests/illumosalloc/setup.nim` checks allocator/IO flag selection, mimalloc
rejection, and allocation/reallocation at runtime.
