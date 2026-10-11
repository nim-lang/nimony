## Masking the parts of an executable that record WHEN it was linked, so two
## builds of the same code compare (and hash) equal. `boot` compares its stages
## with it; the toolchain build stamps the nifler2 cache with it.

const HeaderSkipBytes = 4096
  ## Bytes at the start of an executable to skip during stage-comparison.
  ## Big enough to cover the volatile header regions of all three common
  ## formats: ELF (.note.gnu.build-id is at ~1 KB), Mach-O (LC_UUID lives
  ## inside the load commands), PE (DOS header + COFF header with
  ## TimeDateStamp). 4 KB also matches the typical page size so the cut
  ## tends to fall on a section boundary.

proc maskBuildStamps*(buf: var string) =
  ## Zero out byte regions whose contents depend on *when* the binary was
  ## linked, not *what* the toolchain produced. Two stages whose code is
  ## identical otherwise still compare equal.
  ##
  ## Approach: skip the first `HeaderSkipBytes` (covers ELF build-id,
  ## Mach-O LC_UUID, PE COFF TimeDateStamp — all live in headers) and
  ## additionally mask `HH:MM:SS` / `Mmm DD YYYY` ASCII strings anywhere
  ## else (mimalloc bakes `__DATE__` / `__TIME__` into .rodata via
  ## `vendor/mimalloc/src/options.c`).
  let skip = min(HeaderSkipBytes, buf.len)
  for k in 0 ..< skip: buf[k] = '\0'

  proc isDigit(c: char): bool {.inline.} = c >= '0' and c <= '9'

  # HH:MM:SS — exact 8 bytes.
  var i = skip
  while i + 8 <= buf.len:
    if buf[i+2] == ':' and buf[i+5] == ':' and
       isDigit(buf[i]) and isDigit(buf[i+1]) and
       isDigit(buf[i+3]) and isDigit(buf[i+4]) and
       isDigit(buf[i+6]) and isDigit(buf[i+7]):
      for k in i ..< i + 8: buf[k] = '\0'
      i += 8
    else:
      inc i

  # `Mmm DD YYYY` — month abbreviation + day (space-padded) + 4-digit year.
  const Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  i = skip
  while i + 11 <= buf.len:
    var matched = false
    if buf[i+3] == ' ' and buf[i+6] == ' ' and
       (buf[i+4] == ' ' or isDigit(buf[i+4])) and isDigit(buf[i+5]) and
       isDigit(buf[i+7]) and isDigit(buf[i+8]) and
       isDigit(buf[i+9]) and isDigit(buf[i+10]):
      let m = buf.substr(i, i + 2)
      for mo in Months:
        if m == mo:
          matched = true
          break
    if matched:
      for k in i ..< i + 11: buf[k] = '\0'
      i += 11
    else:
      inc i

