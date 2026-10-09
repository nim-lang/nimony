# (c) 2026 Andreas Rumpf
#
# The two checksums the DEFLATE containers carry: CRC-32 for gzip (RFC 1952)
# and Adler-32 for zlib (RFC 1950).
#
# Both are incremental — `update` takes the previous value and the next piece
# of data — because a stream is checked as it is decoded, not after the whole
# of it has been held in memory:
#
#   var c = initCrc32()
#   c = update(c, part1)
#   c = update(c, part2)
#   assert finish(c) == crc32(part1 & part2)

type
  Crc32* = distinct uint32
    ## A running CRC-32. Kept inverted between updates, as the algorithm wants
    ## it; `finish` undoes that. Distinct so a running value cannot be compared
    ## against a finished one by accident — they differ in every bit.

const CrcTable: array[256, uint32] = [
  0x00000000'u32, 0x77073096'u32, 0xEE0E612C'u32, 0x990951BA'u32, 0x076DC419'u32, 0x706AF48F'u32,
  0xE963A535'u32, 0x9E6495A3'u32, 0x0EDB8832'u32, 0x79DCB8A4'u32, 0xE0D5E91E'u32, 0x97D2D988'u32,
  0x09B64C2B'u32, 0x7EB17CBD'u32, 0xE7B82D07'u32, 0x90BF1D91'u32, 0x1DB71064'u32, 0x6AB020F2'u32,
  0xF3B97148'u32, 0x84BE41DE'u32, 0x1ADAD47D'u32, 0x6DDDE4EB'u32, 0xF4D4B551'u32, 0x83D385C7'u32,
  0x136C9856'u32, 0x646BA8C0'u32, 0xFD62F97A'u32, 0x8A65C9EC'u32, 0x14015C4F'u32, 0x63066CD9'u32,
  0xFA0F3D63'u32, 0x8D080DF5'u32, 0x3B6E20C8'u32, 0x4C69105E'u32, 0xD56041E4'u32, 0xA2677172'u32,
  0x3C03E4D1'u32, 0x4B04D447'u32, 0xD20D85FD'u32, 0xA50AB56B'u32, 0x35B5A8FA'u32, 0x42B2986C'u32,
  0xDBBBC9D6'u32, 0xACBCF940'u32, 0x32D86CE3'u32, 0x45DF5C75'u32, 0xDCD60DCF'u32, 0xABD13D59'u32,
  0x26D930AC'u32, 0x51DE003A'u32, 0xC8D75180'u32, 0xBFD06116'u32, 0x21B4F4B5'u32, 0x56B3C423'u32,
  0xCFBA9599'u32, 0xB8BDA50F'u32, 0x2802B89E'u32, 0x5F058808'u32, 0xC60CD9B2'u32, 0xB10BE924'u32,
  0x2F6F7C87'u32, 0x58684C11'u32, 0xC1611DAB'u32, 0xB6662D3D'u32, 0x76DC4190'u32, 0x01DB7106'u32,
  0x98D220BC'u32, 0xEFD5102A'u32, 0x71B18589'u32, 0x06B6B51F'u32, 0x9FBFE4A5'u32, 0xE8B8D433'u32,
  0x7807C9A2'u32, 0x0F00F934'u32, 0x9609A88E'u32, 0xE10E9818'u32, 0x7F6A0DBB'u32, 0x086D3D2D'u32,
  0x91646C97'u32, 0xE6635C01'u32, 0x6B6B51F4'u32, 0x1C6C6162'u32, 0x856530D8'u32, 0xF262004E'u32,
  0x6C0695ED'u32, 0x1B01A57B'u32, 0x8208F4C1'u32, 0xF50FC457'u32, 0x65B0D9C6'u32, 0x12B7E950'u32,
  0x8BBEB8EA'u32, 0xFCB9887C'u32, 0x62DD1DDF'u32, 0x15DA2D49'u32, 0x8CD37CF3'u32, 0xFBD44C65'u32,
  0x4DB26158'u32, 0x3AB551CE'u32, 0xA3BC0074'u32, 0xD4BB30E2'u32, 0x4ADFA541'u32, 0x3DD895D7'u32,
  0xA4D1C46D'u32, 0xD3D6F4FB'u32, 0x4369E96A'u32, 0x346ED9FC'u32, 0xAD678846'u32, 0xDA60B8D0'u32,
  0x44042D73'u32, 0x33031DE5'u32, 0xAA0A4C5F'u32, 0xDD0D7CC9'u32, 0x5005713C'u32, 0x270241AA'u32,
  0xBE0B1010'u32, 0xC90C2086'u32, 0x5768B525'u32, 0x206F85B3'u32, 0xB966D409'u32, 0xCE61E49F'u32,
  0x5EDEF90E'u32, 0x29D9C998'u32, 0xB0D09822'u32, 0xC7D7A8B4'u32, 0x59B33D17'u32, 0x2EB40D81'u32,
  0xB7BD5C3B'u32, 0xC0BA6CAD'u32, 0xEDB88320'u32, 0x9ABFB3B6'u32, 0x03B6E20C'u32, 0x74B1D29A'u32,
  0xEAD54739'u32, 0x9DD277AF'u32, 0x04DB2615'u32, 0x73DC1683'u32, 0xE3630B12'u32, 0x94643B84'u32,
  0x0D6D6A3E'u32, 0x7A6A5AA8'u32, 0xE40ECF0B'u32, 0x9309FF9D'u32, 0x0A00AE27'u32, 0x7D079EB1'u32,
  0xF00F9344'u32, 0x8708A3D2'u32, 0x1E01F268'u32, 0x6906C2FE'u32, 0xF762575D'u32, 0x806567CB'u32,
  0x196C3671'u32, 0x6E6B06E7'u32, 0xFED41B76'u32, 0x89D32BE0'u32, 0x10DA7A5A'u32, 0x67DD4ACC'u32,
  0xF9B9DF6F'u32, 0x8EBEEFF9'u32, 0x17B7BE43'u32, 0x60B08ED5'u32, 0xD6D6A3E8'u32, 0xA1D1937E'u32,
  0x38D8C2C4'u32, 0x4FDFF252'u32, 0xD1BB67F1'u32, 0xA6BC5767'u32, 0x3FB506DD'u32, 0x48B2364B'u32,
  0xD80D2BDA'u32, 0xAF0A1B4C'u32, 0x36034AF6'u32, 0x41047A60'u32, 0xDF60EFC3'u32, 0xA867DF55'u32,
  0x316E8EEF'u32, 0x4669BE79'u32, 0xCB61B38C'u32, 0xBC66831A'u32, 0x256FD2A0'u32, 0x5268E236'u32,
  0xCC0C7795'u32, 0xBB0B4703'u32, 0x220216B9'u32, 0x5505262F'u32, 0xC5BA3BBE'u32, 0xB2BD0B28'u32,
  0x2BB45A92'u32, 0x5CB36A04'u32, 0xC2D7FFA7'u32, 0xB5D0CF31'u32, 0x2CD99E8B'u32, 0x5BDEAE1D'u32,
  0x9B64C2B0'u32, 0xEC63F226'u32, 0x756AA39C'u32, 0x026D930A'u32, 0x9C0906A9'u32, 0xEB0E363F'u32,
  0x72076785'u32, 0x05005713'u32, 0x95BF4A82'u32, 0xE2B87A14'u32, 0x7BB12BAE'u32, 0x0CB61B38'u32,
  0x92D28E9B'u32, 0xE5D5BE0D'u32, 0x7CDCEFB7'u32, 0x0BDBDF21'u32, 0x86D3D2D4'u32, 0xF1D4E242'u32,
  0x68DDB3F8'u32, 0x1FDA836E'u32, 0x81BE16CD'u32, 0xF6B9265B'u32, 0x6FB077E1'u32, 0x18B74777'u32,
  0x88085AE6'u32, 0xFF0F6A70'u32, 0x66063BCA'u32, 0x11010B5C'u32, 0x8F659EFF'u32, 0xF862AE69'u32,
  0x616BFFD3'u32, 0x166CCF45'u32, 0xA00AE278'u32, 0xD70DD2EE'u32, 0x4E048354'u32, 0x3903B3C2'u32,
  0xA7672661'u32, 0xD06016F7'u32, 0x4969474D'u32, 0x3E6E77DB'u32, 0xAED16A4A'u32, 0xD9D65ADC'u32,
  0x40DF0B66'u32, 0x37D83BF0'u32, 0xA9BCAE53'u32, 0xDEBB9EC5'u32, 0x47B2CF7F'u32, 0x30B5FFE9'u32,
  0xBDBDF21C'u32, 0xCABAC28A'u32, 0x53B39330'u32, 0x24B4A3A6'u32, 0xBAD03605'u32, 0xCDD70693'u32,
  0x54DE5729'u32, 0x23D967BF'u32, 0xB3667A2E'u32, 0xC4614AB8'u32, 0x5D681B02'u32, 0x2A6F2B94'u32,
  0xB40BBE37'u32, 0xC30C8EA1'u32, 0x5A05DF1B'u32, 0x2D02EF8D'u32,
  ]
  ## The reflected polynomial `0xEDB88320`, one byte at a time. A literal and
  ## not computed: it is what every implementation agrees on, and spelling it
  ## out means there is no initialization order to get wrong.

proc initCrc32*(): Crc32 {.inline.} = Crc32(0xFFFFFFFF'u32)

proc update*(c: Crc32; data: openArray[char]): Crc32 =
  var x = uint32(c)
  for ch in data:
    x = CrcTable[int((x xor uint32(ord(ch))) and 0xFF'u32)] xor (x shr 8)
  result = Crc32(x)

proc finish*(c: Crc32): uint32 {.inline.} = uint32(c) xor 0xFFFFFFFF'u32

proc crc32*(data: openArray[char]): uint32 =
  ## The CRC-32 of `data` in one call.
  finish(update(initCrc32(), data))

type
  Adler32* = distinct uint32
    ## A running Adler-32: the two 16-bit sums packed as `b shl 16 or a`,
    ## which is also the finished value — there is no final step.

const
  AdlerBase = 65521'u32
  AdlerNmax = 5552
    ## The most bytes that can be summed before `b` could overflow 32 bits,
    ## so the two `mod`s are paid once per this many bytes rather than per
    ## byte.

proc initAdler32*(): Adler32 {.inline.} = Adler32(1'u32)

proc update*(c: Adler32; data: openArray[char]): Adler32 =
  var a = uint32(c) and 0xFFFF'u32
  var b = uint32(c) shr 16
  var i = 0
  while i < data.len:
    let stop = min(data.len, i + AdlerNmax)
    while i < stop:
      a = a + uint32(ord(data[i]))
      b = b + a
      inc i
    a = a mod AdlerBase
    b = b mod AdlerBase
  result = Adler32((b shl 16) or a)

proc finish*(c: Adler32): uint32 {.inline.} = uint32(c)

proc adler32*(data: openArray[char]): uint32 =
  ## The Adler-32 of `data` in one call.
  finish(update(initAdler32(), data))
