# `{.instruction: "rol"/"ror".}` at every width. A rotate is the one ALU
# operation whose result depends on the width it runs at: a `uint32` rotated as
# 64 bits moves its top bits into the upper half instead of wrapping them
# around, and both the allocated path and `.assembler` once did exactly that.
import std/syncio

proc rol8(x: uint8; n: int): uint8 {.instruction: "rol".}
proc rol16(x: uint16; n: int): uint16 {.instruction: "rol".}
proc rol32(x: uint32; n: int): uint32 {.instruction: "rol".}
proc rol64(x: uint64; n: int): uint64 {.instruction: "rol".}
proc ror32(x: uint32; n: int): uint32 {.instruction: "ror".}

proc compiled(x: uint32): uint32 = rol32(x, 8)

proc inAsm(x {.register: "rdi".}: uint32): uint32 {.assembler.} =
  var r {.register: "rax".}: uint32
  r = rol32(x, 4)
  result = r

proc main =
  echo rol8(0x81'u8, 1)                 # 0x03
  echo rol16(0x8001'u16, 4)             # 0x0018
  echo rol32(0x80000001'u32, 1)         # 3
  echo ror32(0x00000003'u32, 1)         # 0x80000001
  echo rol64(0x8000000000000001'u64, 1) # 3
  echo compiled(0xAABBCCDD'u32)         # 0xBBCCDDAA
  echo inAsm(0xF000000F'u32)            # 0x000000FF

main()
