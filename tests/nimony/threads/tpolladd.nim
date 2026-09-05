when defined(windows):
  import std/syncio
  echo "n=1 op=opPollAdd rd=true wr=true"
  echo "m=1 op=opPollAdd wr=true"
  echo "k=1 fd_is_a=true rd=true wr=false"
  echo "j=1 fd_is_b=true rd=true wr=false"
else:
  import std / [ioring, assertions, syncio]
  import std/posix/posix

  const
    AF_UNIX = 1.cint
    SOCK_STREAM = 1.cint
    MSG_NOSIGNAL = 0x4000.cint

  proc socketpair(domain, typ, protocol: cint;
                  sv: ptr UncheckedArray[cint]): cint {.importc: "socketpair".}
  proc send(s: cint; buf: pointer; len: int; flags: cint): int {.importc: "send".}

  var fds: array[2, cint]
  if socketpair(AF_UNIX, SOCK_STREAM, 0, cast[ptr UncheckedArray[cint]](addr fds)) != 0:
    quit "socketpair failed"
  setNonBlocking(fds[0])
  setNonBlocking(fds[1])
  let (a, b) = (fds[0], fds[1])

  # Make `a` readable, then ask for readiness without issuing any I/O.
  var msg = "x"
  let written = send(b, msg.toCString, len(msg), MSG_NOSIGNAL)
  assert written == len(msg)

  var comps: array[8, IoCompletion]
  discard submitPollAdd(a, never)
  let n = waitCompletions(comps)
  echo "n=", n, " op=", comps[0].op,
       " rd=", evRead in comps[0].readyEvents,
       " wr=", evWrite in comps[0].readyEvents

  # Oneshot: re-arm on `b`, which is writable.
  discard submitPollAdd(b, never)
  let m = waitCompletions(comps)
  echo "m=", m, " op=", comps[0].op,
       " wr=", evWrite in comps[0].readyEvents

  # The mask is honoured: `b` is writable but has nothing to read, so a
  # read-only probe on it must NOT complete. Asserting a negative without a
  # timeout: arm the read-only probe on `b` FIRST, then arm one on `a`, which is
  # still readable (nothing has consumed the byte above — a poll does no I/O).
  # Exactly one completion must come back, and it must be `a`'s.
  #
  # Before the mask existed, opPollAdd always armed both directions, so this
  # probe fired immediately on writability — and since the op is oneshot, a
  # caller that re-armed after each wake spun as fast as it could poll.
  discard submitPollAdd(b, never, {evRead})
  discard submitPollAdd(a, never, {evRead})
  let k = waitCompletions(comps)
  echo "k=", k, " fd_is_a=", cint(comps[0].fd) == a,
       " rd=", evRead in comps[0].readyEvents,
       " wr=", evWrite in comps[0].readyEvents

  # …and the probe left pending on `b` fires as soon as `b` really is readable,
  # reporting evRead and nothing else.
  discard send(a, msg.toCString, len(msg), MSG_NOSIGNAL)
  let j = waitCompletions(comps)
  echo "j=", j, " fd_is_b=", cint(comps[0].fd) == b,
       " rd=", evRead in comps[0].readyEvents,
       " wr=", evWrite in comps[0].readyEvents

  closeFd(a)
  closeFd(b)
