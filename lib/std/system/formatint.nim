## Integer → decimal text, included into `system`.
##
## A port of the integer `Display` code in Rust's `core::fmt::num`. Digits are
## produced from the least significant end, right-aligned in a fixed buffer:
## four at a time from a table of the two-digit pairs `00`..`99`, then two,
## then the last one. `NumBuffer` and `formatInto` are the allocation-free
## interface; `$` and `addInt` are built on them.

const
  MaxDecN = 20
    ## Decimal digits of `high(uint64)`; also `-` plus the 19 digits of
    ## `low(int64)`.
  DecimalPairs: array[200, char] = [
    '0', '0', '0', '1', '0', '2', '0', '3', '0', '4', '0', '5', '0', '6', '0', '7', '0', '8', '0', '9',
    '1', '0', '1', '1', '1', '2', '1', '3', '1', '4', '1', '5', '1', '6', '1', '7', '1', '8', '1', '9',
    '2', '0', '2', '1', '2', '2', '2', '3', '2', '4', '2', '5', '2', '6', '2', '7', '2', '8', '2', '9',
    '3', '0', '3', '1', '3', '2', '3', '3', '3', '4', '3', '5', '3', '6', '3', '7', '3', '8', '3', '9',
    '4', '0', '4', '1', '4', '2', '4', '3', '4', '4', '4', '5', '4', '6', '4', '7', '4', '8', '4', '9',
    '5', '0', '5', '1', '5', '2', '5', '3', '5', '4', '5', '5', '5', '6', '5', '7', '5', '8', '5', '9',
    '6', '0', '6', '1', '6', '2', '6', '3', '6', '4', '6', '5', '6', '6', '6', '7', '6', '8', '6', '9',
    '7', '0', '7', '1', '7', '2', '7', '3', '7', '4', '7', '5', '7', '6', '7', '7', '7', '8', '7', '9',
    '8', '0', '8', '1', '8', '2', '8', '3', '8', '4', '8', '5', '8', '6', '8', '7', '8', '8', '8', '9',
    '9', '0', '9', '1', '9', '2', '9', '3', '9', '4', '9', '5', '9', '6', '9', '7', '9', '8', '9', '9']
    ## Every two-digit number `00`..`99`, back to back.

type
  NumBuffer* = object
    ## Room for any 64-bit integer in decimal, sign included. `formatInto`
    ## writes into it and returns a view of the text; nothing is allocated.
    buf: array[MaxDecN, char]

func fmtInner(x: uint64; buf: var array[MaxDecN, char]): Natural {.ensures: result <= MaxDecN.} =
  ## Writes the digits of `x` right-aligned into `buf` and returns the index
  ## of the first one. The `offset >= n` tests always hold, since 20 digits fit
  ## any `uint64`; they are what proves the stores in bounds.
  var offset: Natural = buf.len
  var remain = x
  # Four digits per round from the pair table.
  while offset >= 4 and remain > 999'u64:
    offset = offset - 4
    let quad = remain mod 10000'u64
    remain = remain div 10000'u64
    let pair1 = int(quad div 100'u64)
    let pair2 = int(quad mod 100'u64)
    buf[offset + 0] = DecimalPairs[pair1 * 2 + 0]
    buf[offset + 1] = DecimalPairs[pair1 * 2 + 1]
    buf[offset + 2] = DecimalPairs[pair2 * 2 + 0]
    buf[offset + 3] = DecimalPairs[pair2 * 2 + 1]
  # Two digits from the pair table.
  if offset >= 2 and remain > 9'u64:
    offset = offset - 2
    let pair = int(remain mod 100'u64)
    remain = remain div 100'u64
    buf[offset + 0] = DecimalPairs[pair * 2 + 0]
    buf[offset + 1] = DecimalPairs[pair * 2 + 1]
  # The last digit, if any; zero is the one number with a leading zero.
  if offset >= 1 and (remain != 0'u64 or x == 0'u64):
    offset = offset - 1
    # `remain < 10` here; the mask is what lets the prover bound the index.
    let last = int(remain and 15'u64)
    buf[offset] = DecimalPairs[last * 2 + 1]
  result = offset

func formatInto*(buf: var NumBuffer; x: uint64): openArray[char] {.establishesBorrow.} =
  ## Writes `x` in decimal into `buf` and returns the text, a view into `buf`.
  let offset = fmtInner(x, buf.buf)
  result = toOpenArray(buf.buf, offset, buf.buf.len - 1)

func formatInto*(buf: var NumBuffer; x: int64): openArray[char] {.establishesBorrow.} =
  ## Writes `x` in decimal into `buf` and returns the text, a view into `buf`.
  let magnitude = if x < 0: 0'u64 - cast[uint64](x) else: uint64(x)
  var offset = fmtInner(magnitude, buf.buf)
  if x < 0 and offset >= 1:
    offset = offset - 1
    buf.buf[offset] = '-'
  result = toOpenArray(buf.buf, offset, buf.buf.len - 1)

func addChars(s: var string; chars: openArray[char]) {.inline.} =
  if chars.len > 0:
    let start = s.len
    copyMem(beginStore(s, start + chars.len, start), addr chars[0], chars.len)
    # `endStore` refreshes the cached first `AlwaysAvail` chars; past them
    # there is nothing to refresh.
    if start < AlwaysAvail:
      endStore(s)

func addInt*(s: var string; x: int64) =
  ## Appends `x` in decimal to `s`.
  var buf {.noinit.}: NumBuffer
  s.addChars(buf.formatInto(x))

func addInt*(s: var string; x: uint64) =
  ## Appends `x` in decimal to `s`.
  var buf {.noinit.}: NumBuffer
  s.addChars(buf.formatInto(x))

func `$`*(x: uint64): string =
  result = ""
  result.addInt(x)

func `$`*(x: int64): string =
  result = ""
  result.addInt(x)

func `$`*(x: int32): string =
  $(int64(x))
