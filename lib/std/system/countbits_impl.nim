# slightly modified from original countbits_impl

# C compiler defines seem to not be defined yet:
const useBuiltins = not defined(noIntrinsicsBitOpts)
const useGCC_builtins = (defined(gcc) or defined(llvm_gcc) or
                         defined(clang)) and useBuiltins
const useICC_builtins = defined(icc) and useBuiltins
const useVCC_builtins = defined(vcc) and useBuiltins
const arch64 = true # sizeof(int) == 8

template countBitsImpl(n: uint32): int =
  # generic formula is from: https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
  var v = uint32(n)
  v = v - ((v shr 1) and 0x55555555'u32)
  v = (v and 0x33333333'u32) + ((v shr 2) and 0x33333333'u32)
  (((v + (v shr 4) and 0xF0F0F0F'u32) * 0x1010101'u32) shr 24).int

template countBitsImpl(n: uint64): int =
  # generic formula is from: https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
  var v = uint64(n)
  v = v - ((v shr 1) and 0x5555555555555555'u64)
  v = (v and 0x3333333333333333'u64) + ((v shr 2) and 0x3333333333333333'u64)
  v = (v + (v shr 4) and 0x0F0F0F0F0F0F0F0F'u64)
  ((v * 0x0101010101010101'u64) shr 56).int


when useGCC_builtins:
  # Returns the number of set 1-bits in value.
  proc builtin_popcount(x: cuint): cint {.importc: "__builtin_popcount", cdecl.}
  proc builtin_popcountll(x: culonglong): cint {.
      importc: "__builtin_popcountll", cdecl.}

elif useVCC_builtins:
  # Counts the number of one bits (population count) in a 16-, 32-, or 64-byte unsigned integer.
  func builtin_popcnt16(a2: uint16): uint16 {.
      importc: "__popcnt16", header: "<intrin.h>".}
  func builtin_popcnt32(a2: uint32): uint32 {.
      importc: "__popcnt", header: "<intrin.h>".}
  func builtin_popcnt64(a2: uint64): uint64 {.
      importc: "__popcnt64", header: "<intrin.h>".}

elif useICC_builtins:
  # Intel compiler intrinsics: http://fulla.fnal.gov/intel/compiler_c/main_cls/intref_cls/common/intref_allia_misc.htm
  # see also: https://software.intel.com/en-us/node/523362
  # Count the number of bits set to 1 in an integer a, and return that count in dst.
  func builtin_popcnt32(a: cint): cint {.
      importc: "_popcnt", header: "<immintrin.h>".}
  func builtin_popcnt64(a: uint64): cint {.
      importc: "_popcnt64", header: "<immintrin.h>".}

proc countBits32*(x: uint32): int {.inline.} =
  when useGCC_builtins:
    result = builtin_popcount(x.cuint).int
  elif useVCC_builtins:
    result = builtin_popcnt32(x.uint32).int
  elif useICC_builtins:
    result = builtin_popcnt32(x.cint).int
  else:
    result = countBitsImpl(x.uint32)

proc countBits64*(x: uint64): int {.inline.} =
  when useGCC_builtins:
    result = builtin_popcountll(x.culonglong).int
  elif useVCC_builtins:
    when arch64: result = builtin_popcnt64(x.uint64).int
    else: result = builtin_popcnt32((x.uint64 and 0xFFFFFFFF'u64).uint32).int +
                    builtin_popcnt32((x.uint64 shr 32'u64).uint32).int
  elif useICC_builtins:
    when arch64: result = builtin_popcnt64(x.uint64).int
    else: result = builtin_popcnt32((x.uint64 and 0xFFFFFFFF'u64).cint).int +
                    builtin_popcnt32((x.uint64 shr 32'u64).cint).int
  else:
    result = countBitsImpl(x.uint64)
