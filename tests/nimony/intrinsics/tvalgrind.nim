import std/[syncio, assertions]

## The valgrind client request (`VgClientRequest`), checked WITHOUT valgrind.
##
## That is not a gap, it is the interesting half. The request is a sequence of
## instructions chosen so that hardware executing it does nothing observable —
## four rotates that sum to a full 64-bit turn, then `orr x10, x10, x10` — and
## valgrind's JIT replaces it when it is watching. So "runs correctly unobserved"
## is a real property with a real way to fail, and it is the one every program
## that is NOT being valgrinded depends on. Running the suite under valgrind is
## how the other half gets checked; see `lib/std/valgrind`.

const vgAvailable = defined(nimNoLibc) and defined(arm64)
  ## Where the row has a lowering (`src/lib/intrinsics.nim`: `tgA64`). The C
  ## backend has none — under C the whole mechanism is valgrind's own headers and
  ## mimalloc's tracking — so the body below must vanish rather than fail to link.

when vgAvailable:
  proc vgClientRequest(args: pointer): uint {.intrinsic: "VgClientRequest".}

  const RunningOnValgrind = 0x1001'u

  proc request(r, a1, a2, a3, a4: uint): uint =
    var blk: array[6, uint] = [r, a1, a2, a3, a4, 0'u]
    result = vgClientRequest(cast[pointer](addr blk[0]))

  proc checkAnswerIsStable() =
    ## `RUNNING_ON_VALGRIND` is 0 when nothing intercepts the sequence and non-zero
    ## when memcheck does, and this test is run BOTH ways (`hastur native` and
    ## `hastur nativevalgrind`), so the assertion cannot be either literal. What
    ## holds in both worlds is that the answer is one of those two things and does
    ## not change between two adjacent requests — which is the property that fails
    ## if the result register is picking up garbage rather than an answer.
    let first = request(RunningOnValgrind, 0, 0, 0, 0)
    let second = request(RunningOnValgrind, 0, 0, 0, 0)
    assert first == second
    assert first == 0'u or first == 1'u

  proc checkRegistersSurvive() =
    ## Values held across the request come back unchanged.
    ##
    ## Read this for what it is: arkham's ABI planner does not keep a caller's value
    ## in x3/x4 — the registers valgrind's protocol commandeers — across an
    ## expression, so no arrangement of Nim locals reaches the case this would most
    ## like to test. That was checked by deleting the encoder's x4 restore and
    ## watching this test, parameters-in-x0..x7, and a request in call-argument
    ## position all still pass. The encoder saves x3/x4 regardless (see
    ## `emitVgClientRequest`); the aliasing and restore ORDER are covered where they
    ## are actually decidable, on the emitted bytes, in nativenif's tester.
    ##
    ## What remains here is still worth having: it is the check that a request in
    ## ordinary code disturbs nothing around it.
    var a = 11'u
    var b = 22'u
    var c = 33'u
    var d = 44'u
    var e = 55'u
    var f = 66'u
    var g = 77'u
    var h = 88'u
    let answer = request(RunningOnValgrind, a, b, c, d)
    assert answer == 0'u or answer == 1'u
    assert a == 11'u and b == 22'u and c == 33'u and d == 44'u
    assert e == 55'u and f == 66'u and g == 77'u and h == 88'u
    # ... and the same again with the values consumed AFTER a second request, so a
    # restore that merely happens to survive one call does not pass by luck.
    let total = a + b + c + d + e + f + g + h
    discard request(RunningOnValgrind, e, f, g, h)
    assert a + b + c + d + e + f + g + h == total
    assert total == 396'u

  checkAnswerIsStable()
  checkRegistersSurvive()

echo "vgreq ok"
