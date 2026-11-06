# ctgrind check of `std/crypto/private/ctbignum`: valgrind memcheck with
# every secret marked undefined reports each branch and each memory address
# computed from one. Run by `tests/ctgrind/setup.nim` on both backends
# (`bin/hastur tests/ctgrind`); by hand:
#
#   bin/nimony c -d:release -d:useLibc --passC:-DMI_TRACK_VALGRIND=1 \
#     --nimcache:/tmp/ctg tests/ctgrind/deps/ctbignum_ctgrind.nim
#   valgrind --error-exitcode=1 /tmp/ctg/c/<module>/ctbignum_ctgrind
#
#   bin/nimony n -d:release --nimcache:/tmp/ctgn tests/ctgrind/deps/ctbignum_ctgrind.nim
#   valgrind --error-exitcode=1 /tmp/ctgn/n/<module>/ctbignum_ctgrind
#
# Expected: "ctgrind ok", no memcheck error, exit code 0. With
# `-d:ctgrindSelfTest` the driver also branches on the (still marked)
# output of `rsaCrtRaw` and runs `bignum.modExp` (variable time) on a secret
# exponent, and memcheck must then report errors in both: proof that the
# marking reaches through the whole computation and is detected.
#
# Secret: p, q, dP, dQ, qInv and the blinding inputs, from before
# `initRsaCrt` on. The fault check (`rsaCrtCheckMask`) runs on the still
# marked output of `rsaCrtRaw`. On the C backend the result of `initRsaCrt`
# (a refused key is public), the check's verdict and the signature are then
# declassified with `VALGRIND_MAKE_MEM_DEFINED` and checked in the same pass.
#
# The native backend has no C header to take the client requests from, so
# there a secret is TAINTED instead: `taint` adds `u1 - u2` to every limb,
# where `u1` and `u2` are the same never-written stack word read through two
# slots, a zero whose bits memcheck tracks as undefined. Nothing can be
# declassified that way, so the tainted pass only computes (`initRsaCrt`,
# `rsaCrtRaw`, `rsaCrtCheckMask`) and a second, untainted pass checks the
# results.

import std/syncio
import std/crypto/bignum
import std/crypto/private/ctbignum

when defined(nimNativeBackend) and defined(amd64):
  type W32 = ptr UncheckedArray[uint32]

  proc subq(d: var uint64; s: uint64) {.instruction: "sub".}
  proc addq(d: var uint64; s: uint64) {.instruction: "add".}
  proc inci(d: var int) {.instruction: "inc".}
  proc cmpi(a, b: int) {.instruction: "cmp".}
  proc zf(): bool {.instruction: "zf".}

  proc taintWords(p {.register: "rdi".}: W32; n {.register: "rsi".}: int) {.assembler.} =
    var u {.stack.}: uint64
    var u2 {.stack.}: uint64
    var d {.register: "rax".}: uint64
    var e {.register: "rcx".}: uint64
    var i {.register: "rdx".}: int
    d = u                      # never written: undefined to memcheck
    u2 = d
    e = u2                     # the same value, through memory: VEX cannot fold
    subq(d, e)                 # zero, still undefined
    i = 0
    while true:
      var x32 {.register: "r8".}: uint32
      x32 = p[i]
      var x {.register: "r8".}: uint64
      addq(x, d)
      var lo {.register: "r8".}: uint32
      p[i] = lo
      inci(i)
      cmpi(i, n)
      if zf(): break

  const Tainting = true

  proc secret(x: var BigNum) =
    taintWords(cast[W32](addr x.limbs), MaxLimbs)

  proc secret(x: var seq[byte]) =
    if x.len >= 4: taintWords(cast[W32](addr x[0]), x.len div 4)

  proc public(x: var seq[byte]) = discard
  proc public(x: var BigNum) = discard
  proc public(x: var bool) = discard
  proc public(x: var uint32) = discard
else:
  proc makeMemUndefined(p: pointer; len: int) {.
    importc: "VALGRIND_MAKE_MEM_UNDEFINED", header: "<valgrind/memcheck.h>".}
  proc makeMemDefined(p: pointer; len: int) {.
    importc: "VALGRIND_MAKE_MEM_DEFINED", header: "<valgrind/memcheck.h>".}

  const Tainting = false

  proc secret(x: var BigNum) =
    makeMemUndefined(addr x.limbs, sizeof(Limbs))

  proc secret(x: var seq[byte]) =
    if x.len > 0: makeMemUndefined(addr x[0], x.len)

  proc public(x: var seq[byte]) =
    if x.len > 0: makeMemDefined(addr x[0], x.len)

  proc public(x: var BigNum) =
    makeMemDefined(addr x.limbs, sizeof(Limbs))

  proc public(x: var bool) =
    makeMemDefined(addr x, sizeof(bool))

  proc public(x: var uint32) =
    makeMemDefined(addr x, sizeof(uint32))

# a throwaway 2048-bit key made with `openssl genpkey` for the tests only,
# with OpenSSL's raw private operation on two inputs
const K =
  (name: "2048a", n: "c35edd07a4de334799f43cb9db36c16797e94ce01906ac3ce9a673e360257a4d532ab264de2246dd7cf42c782217b718b7e47563491bddda72695d5d793feb1029794eb189dafe493a73159187b2635a761fef467942a74549f17416a47871b5f962d47785b2e3e0a426083356f69b81eb0da1cd11b9f2ab42d141ce4f425486aebe5fd5aeb59fc1256b763cb16ff4956d36cc224179f26e6afea43c34f7cbd71724089a8cdbb891a4f4694c6b8bbf3caabf90604b5fc9438f78da1848daefac8d579f6ff8d1bcb7fe7a95b03d15b8f06d7fe160c74e20a4a7389440b35baa514620dd7c0a417bf00f179893e743ff43591f6db729eb344048f016012d3bc519", e: "10001",
   p: "fb3c8e046cdc6baf0c0b38f674accb6fbf9c52b8d3a0b315df5ed1cc985a0a99871ed8f1b44ac01c81ad6923f38385fa9422e59554535fa91e9822ba4e00b31d1426bd41b85f57dea5c77b940329aca56a3e5abee676bb3e127af6603056bc6c9e530d51e69579d31858b2527adb130c1f579c46de29a80912cbf2cdb0cee9cd",
   q: "c71325d7edf663f4d32d11ac5a8acb14349c5b4a5944274bfc1e25a2473e1ab0c05756fa92843dcb9797d12ec586cf88a77bda46adca2064d7ee3a99b9d9fb58c03c772b50df545c2c6102e7323116ef75fe3243dec043057979ddf9c585bf68622d92abe9f2baa2ed0ddb515989827ca4d8254daea3690c42c50fca24360c7d",
   dP: "9d93887bac0fc016a596305f1c24f8e0192d22113ce3111009eebde5f6349b6b59fd76127b835ae5464c8ea276ecaa4ed534e20c7c3c62c67f22eabd31896cce33426cf7d87d2c6ec049d2efade028a20376229293982aaeaf06a29b08168ece0dab87b233ced2ff1cc91ce7709911644c10363b98c245db93078a3d8ac4ec8d",
   dQ: "549362f988e4abf4c09f1e2a0e517713bf824a1a9d30224ddaefe7e738c33ecda6f9b43f426e225ed3a9b13a60baf482c73d0c3008f79da99a3f60f01c906a837b30752496f83abbea32d0ebee5c02b57b78a25c494f7f69dd011478601d8183546aae2c91790927dcf48d1fd39d632d974f291206c6f003407ebe458ffad275",
   qInv: "a4556bdbd3d1e365b616676436d63cba4ddfc743132db43e44faf97b07e63ec400408b366a6c54318153fef79793d8ed5eccd9caf5e0b4680ca57a85c7bfaa0a110b77ac9af9f463429aaf7da87489daa1e56174029c972886d5d103fc3d5c9c0b2cf6edeafe7044dca4e5cafbebcc44283cf0d63cb79ab5144fb4dee1155e32",
   sigs: [("1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00ca76a3b779966b02badf084fd1ac2042e165f993f1e606f9e34672593e2edd764c5936ed1e258cad13c3a6218e860f962615ce", "834416fa41562cae297c424328a508a6e5fab5853f737511c41527ec04ee930c6d6cc9513304508f42d6e957b9c2ea3292c449cbe266bc78eaffe7e75a01b0e3cdb50ad96f6d8d746a96fcee23ce54f69bc71c76d2448b1761f611b42fbe5a4f0cbb51b452f65ce5437dc1a13fab65d1562894020d17fb9e6f09d70b54f5ee205243125a65edddf5d50a5e5085fb0ad86bed3247af915cfc4fdbd2e4ac69330c156428921d9ad1338b7ccb9f2dd3cc551eead61a85fbe44fa195708766e2b9e57df94036dd8bec050f198b724b96e1fd3dfbe5e1d3e71f0c194b42fd318108bd4a95a8f8e06b90f469d1da321917bf430d3d8909562403010ebee81fee2917a3"), ("5445173877f13b6166e2bfddb1592ae5e35d375fe7daee3845a5a289493795c556a8c3f02bbb94ed57267739a4bbb71d37da35c82cf4d7bb4c5c940dda0b614f34337c60950d236e158bdd197dc72a0938ebf5a34017a8bb3cb9442c8ac1ad09210f5bdc9eb610a1909a757f0aca68e11007bba0fb6ead438fcb72edf4485c60809b9818d0a3031bcae61c1ef9399765df289d0bf4881ea58bd67097227af6118a3a841642b8314d2d2eac6ce7db04cb322d1f9eef436f08f5496ed077439fbd5f6a6089ac41ea04a9d50725d553cb8a979cec90062f60e131d8ed2682a060940b8007ec76f9491b54f082be00f9aae30444ee9c47cd73a519d88c3b02a8096", "a2cdfb0b450222375e44ba806c27333ff9330b4a54a58ad72114a3aa553f3a46f3d95b65d91257b753be4c28db0f1d90c8a5e8de2804d35ba74d3d6d36e62db58b231b797684edc939f437e2b94f8f314df03bf83e32662f2b1eaf88bbb114d3047ced55a779b6bbda10e62e5785f37da4028e91631bd81dcf6321f09ccea1c5cd63c4ac03d41197158850e90c43475f3f1849617eca7d2e35ffcfa50fe2460f76d943d24c5dd2b47a787298929706b9577fbedd81d1df1b27c41a20640e2d93551c8f738aedf0a415950879023e9a96b6d579ff4eddaccd8e7ddb496733727736f5e2841ed6c7bb07832b974f70c3e287d3179a531b3a3997873d036fa81b5"), ("0", "0"), ("1", "1"), ("c35edd07a4de334799f43cb9db36c16797e94ce01906ac3ce9a673e360257a4d532ab264de2246dd7cf42c782217b718b7e47563491bddda72695d5d793feb1029794eb189dafe493a73159187b2635a761fef467942a74549f17416a47871b5f962d47785b2e3e0a426083356f69b81eb0da1cd11b9f2ab42d141ce4f425486aebe5fd5aeb59fc1256b763cb16ff4956d36cc224179f26e6afea43c34f7cbd71724089a8cdbb891a4f4694c6b8bbf3caabf90604b5fc9438f78da1848daefac8d579f6ff8d1bcb7fe7a95b03d15b8f06d7fe160c74e20a4a7389440b35baa514620dd7c0a417bf00f179893e743ff43591f6db729eb344048f016012d3bc518", "c35edd07a4de334799f43cb9db36c16797e94ce01906ac3ce9a673e360257a4d532ab264de2246dd7cf42c782217b718b7e47563491bddda72695d5d793feb1029794eb189dafe493a73159187b2635a761fef467942a74549f17416a47871b5f962d47785b2e3e0a426083356f69b81eb0da1cd11b9f2ab42d141ce4f425486aebe5fd5aeb59fc1256b763cb16ff4956d36cc224179f26e6afea43c34f7cbd71724089a8cdbb891a4f4694c6b8bbf3caabf90604b5fc9438f78da1848daefac8d579f6ff8d1bcb7fe7a95b03d15b8f06d7fe160c74e20a4a7389440b35baa514620dd7c0a417bf00f179893e743ff43591f6db729eb344048f016012d3bc518")])
proc run(mark: bool): int =
  ## Signs the two inputs; with `mark`, from secrets marked for memcheck.
  ## Returns the number of wrong results, or 0 when the results cannot be
  ## looked at (a tainted pass).
  result = 0
  let check = not (mark and Tainting)
  let n = fromHex(K.n)
  let e = fromHex(K.e)
  var p = fromHex(K.p)
  var q = fromHex(K.q)
  var dP = fromHex(K.dP)
  var dQ = fromHex(K.dQ)
  var qInv = fromHex(K.qInv)
  let h = p.n
  if mark:
    secret(p)
    secret(q)
    secret(dP)
    secret(dQ)
    secret(qInv)
  var key = default(RsaCrt)
  var ok = initRsaCrt(n, e, p, q, dP, dQ, qInv, key)
  if mark: public(ok)
  if check and not ok: quit "ctgrind: key refused"
  for i in 0 ..< 2:
    let c = fromHex(K.sigs[i][0])
    var rawP = newSeq[byte](4 * h + 8)
    var rawQ = newSeq[byte](4 * h + 8)
    for j in 0 ..< rawP.len:
      rawP[j] = byte((j * 37 + i * 11 + 5) and 0xFF)
      rawQ[j] = byte((j * 101 + i * 7 + 3) and 0xFF)
    if mark:
      secret(rawP)
      secret(rawQ)
    var bP = default(Limbs)
    var bQ = default(Limbs)
    if not ctFromBytesBE(rawP, bP) or not ctFromBytesBE(rawQ, bQ):
      quit "ctgrind: blinding"
    if mark:
      # the byte buffers are dead from here on; mimalloc's `free` reads them
      public(rawP)
      public(rawQ)
    let bl = (rawP.len + 3) div 4
    if bl < 1 or bl > MaxLimbs - 2: quit "ctgrind: blinding width"
    var s = rsaCrtRaw(key, c, bP, bQ, bl)
    var verdict = rsaCrtCheckMask(key, s, c)
    when defined(ctgrindSelfTest):
      # the output must still carry the marking: memcheck must complain here
      if mark and s.limbs[0] == 0'u32: echo "selftest"
    if mark:
      public(s)
      public(verdict)
    if check:
      if verdict != 0xFFFFFFFF'u32: inc result
      s.n = n.n
      if cmp(s, fromHex(K.sigs[i][1])) != 0: inc result
  when defined(ctgrindSelfTest):
    # variable time on a secret exponent: memcheck must complain here
    if mark:
      var ctx = default(Mont)
      if initMont(n, ctx):
        let leak = modExp(fromHex(K.sigs[0][0]), dP, ctx)
        if leak.limbs[0] == 0'u32: echo "selftest"
  wipe(key)

proc main() =
  var failures = run(true)
  if Tainting: failures = run(false)
  if failures == 0: echo "ctgrind ok"
  else: echo "ctgrind: wrong result"

main()
