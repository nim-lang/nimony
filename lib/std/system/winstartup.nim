## The C-style process vectors — `cmdCount`/`cmdLine` and `nimEnviron` — for the
## freestanding Windows target.
##
## The generated `main` stores its own `argc`/`argv`/`envp` into those globals,
## which is the whole story on every OS that hands a process entry point a C-style
## argument and environment vector. Windows hands it nothing at all: the OS keeps
## the command line as one unsplit UTF-16 string behind `GetCommandLineW` and the
## environment as one UTF-16 block behind `GetEnvironmentStringsW`, and the C
## runtime that would normally transcode and split them is exactly what a
## `-d:nimNativeIo` build does without. So under that configuration the runtime
## does the work itself, from `system`'s module init — which runs before any other
## module's, and after `main` stored the placeholders it is correcting.
##
## The result is that both globals hold on Windows what they hold on Linux, and
## every consumer of them works unchanged: `std/parseopt` reads `cmdCount`/
## `cmdLine`, and `std/envvars` reads `nimEnviron` under `nimNativeIo`.
## (`std/cmdline`'s `paramStr`/`paramCount` are the exception — on Windows they
## call `GetCommandLineW` and split it themselves. The splitting rules below are
## the ones they use, deliberately: the two views of the command line must not
## disagree about where the arguments are.)
##
## Nothing here is emitted on any other target: the entire module body is inside
## the `when` below.

when defined(windows) and defined(nimNativeIo):
  # Bare kernel32 imports (no `header: "<windows.h>"`), like `system/panics` and
  # `system/dyncalls`: nimony emits the prototype and the linker binds the symbol
  # from kernel32, keeping the `system` translation unit header-free.
  proc cGetCommandLineW(): ptr UncheckedArray[uint16] {.stdcall,
    importc: "GetCommandLineW".}
  proc cGetEnvironmentStringsW(): ptr UncheckedArray[uint16] {.stdcall,
    importc: "GetEnvironmentStringsW".}
  proc cFreeEnvironmentStringsW(env: ptr UncheckedArray[uint16]): int32 {.stdcall,
    importc: "FreeEnvironmentStringsW".}

  var
    gCmdCount {.importc: "cmdCount".}: int32
    gCmdLine {.importc: "cmdLine".}: ptr UncheckedArray[cstring]
    gEnviron {.importc: "nimEnviron".}: ptr UncheckedArray[cstring]
      ## All three are defined by the generated `main`'s translation unit (hexer's
      ## `genMainProc`) and written here.

  proc utf16ToUtf8(src: ptr UncheckedArray[uint16]; units: int;
                   dest: ptr UncheckedArray[char]): int =
    ## Transcode `units` code units of UTF-16 `src` into `dest`, returning the byte
    ## count written (no terminator appended). `dest` needs 3 bytes per input code
    ## unit: a BMP code point is at most 3 bytes, and a code point above it arrives
    ## as a surrogate PAIR — two units for four bytes.
    ##
    ## The length is explicit rather than NUL-delimited because the environment
    ## block is a run of NUL-SEPARATED strings: transcoding it whole keeps that
    ## structure (a NUL unit is code point 0, one 0x00 byte) so the entries can be
    ## collected from the UTF-8 copy afterwards.
    var i = 0
    var o = 0
    while i < units:
      var cp = uint32(src[i])
      inc i
      if cp >= 0xD800'u32 and cp <= 0xDBFF'u32 and i < units and
         src[i] >= 0xDC00'u16 and src[i] <= 0xDFFF'u16:
        cp = 0x10000'u32 + ((cp - 0xD800'u32) shl 10) + (uint32(src[i]) - 0xDC00'u32)
        inc i
      if cp < 0x80'u32:
        dest[o] = char(cp); inc o
      elif cp < 0x800'u32:
        dest[o] = char(0xC0'u32 or (cp shr 6)); inc o
        dest[o] = char(0x80'u32 or (cp and 0x3F'u32)); inc o
      elif cp < 0x10000'u32:
        dest[o] = char(0xE0'u32 or (cp shr 12)); inc o
        dest[o] = char(0x80'u32 or ((cp shr 6) and 0x3F'u32)); inc o
        dest[o] = char(0x80'u32 or (cp and 0x3F'u32)); inc o
      else:
        dest[o] = char(0xF0'u32 or (cp shr 18)); inc o
        dest[o] = char(0x80'u32 or ((cp shr 12) and 0x3F'u32)); inc o
        dest[o] = char(0x80'u32 or ((cp shr 6) and 0x3F'u32)); inc o
        dest[o] = char(0x80'u32 or (cp and 0x3F'u32)); inc o
    result = o

  proc splitCmdLine(buf: ptr UncheckedArray[char]; len: int;
                    argv: ptr UncheckedArray[cstring]): int =
    ## Split the UTF-8 `buf` IN PLACE into NUL-terminated arguments, recording
    ## each one's start in `argv` and returning the count.
    ##
    ## Windows' rules (the ones `std/cmdline.parseCmdLine` documents): arguments
    ## are separated by unquoted spaces/tabs; `\"` is a literal quote; a run of
    ## backslashes collapses by half only when a quote follows it; `""` inside a
    ## quoted argument is a literal quote.
    ##
    ## In place is safe because every rule either copies a byte or DROPS one (a
    ## delimiting quote, half a backslash run), so the write cursor never passes
    ## the read cursor. Only the quoting characters and whitespace are examined,
    ## and all of those are ASCII — a UTF-8 continuation byte is >= 0x80 and can
    ## never be mistaken for one, so transcoding first and splitting after needs
    ## no second buffer.
    var i = 0                 # read cursor
    var o = 0                 # write cursor, always <= i
    result = 0
    while true:
      while i < len and (buf[i] == ' ' or buf[i] == '\t'): inc i
      if i >= len: break
      argv[result] = cast[cstring](addr buf[o])
      inc result
      var inQuote = false
      while i < len:
        let ch = buf[i]
        if ch == '\\':
          var j = i
          while j < len and buf[j] == '\\': inc j
          if j < len and buf[j] == '"':
            for k in 1 .. (j - i) div 2:
              buf[o] = '\\'; inc o
            if (j - i) mod 2 == 0:
              i = j                       # even run: the quote stays a delimiter
            else:
              buf[o] = '"'; inc o         # odd run: the quote is escaped
              i = j + 1
          else:
            buf[o] = ch; inc o; inc i     # no quote follows: backslashes are literal
        elif ch == '"':
          inc i
          if not inQuote:
            inQuote = true
          elif i < len and buf[i] == '"':
            buf[o] = '"'; inc o; inc i
          else:
            inQuote = false
            break
        elif (ch == ' ' or ch == '\t') and not inQuote:
          break
        else:
          buf[o] = ch; inc o; inc i
      buf[o] = '\0'; inc o

  proc setupCmdLine() =
    let cmd = cGetCommandLineW()
    var units = 0
    while cmd[units] != 0'u16: inc units
    # One block for both the pointer array and the text, never freed: it lives for
    # the process exactly like the kernel-provided `argv` it stands in for.
    # `units + 2` pointers is a safe bound — an argument needs at least one code
    # unit — and 3 bytes per unit covers the widest transcoding (see above).
    let ptrBytes = (units + 2) * sizeof(cstring)
    let mem = alloc(ptrBytes + 3 * units + 1)
    let argv = cast[ptr UncheckedArray[cstring]](mem)
    let text = cast[ptr UncheckedArray[char]](cast[uint](mem) + uint(ptrBytes))
    let n = utf16ToUtf8(cmd, units, text)
    let count = splitCmdLine(text, n, argv)
    argv[count] = cast[cstring](nil)      # the NULL terminator C's `argv` carries
    gCmdCount = int32(count)
    gCmdLine = argv

  proc setupEnviron() =
    ## Build `nimEnviron` — a NULL-terminated `char**` of `"KEY=VALUE"` — from the
    ## environment block, which arrives as UTF-16 `KEY=VALUE\0KEY=VALUE\0…\0\0`.
    let env = cGetEnvironmentStringsW()
    if env == nil: return
    var units = 0                          # up to, not including, the final NUL
    while not (env[units] == 0'u16 and env[units + 1] == 0'u16): inc units
    # One block for both the pointer array and the text, never freed: it lives for
    # the process exactly like the kernel-provided `environ` it stands in for. Each
    # entry ends in a NUL unit, so there can be no more entries than units; 3 bytes
    # per unit covers the widest transcoding (see `utf16ToUtf8`).
    let ptrBytes = (units + 1) * sizeof(cstring)
    let mem = alloc(ptrBytes + 3 * units + 1)
    let vec = cast[ptr UncheckedArray[cstring]](mem)
    let text = cast[ptr UncheckedArray[char]](cast[uint](mem) + uint(ptrBytes))
    # Transcoding the block WHOLE preserves its NUL separators, so the entries are
    # already laid out contiguously and NUL-terminated — only their starts are left
    # to collect. Entries beginning with '=' (Windows' per-drive working-directory
    # records, `=C:=C:\dir`) are kept, matching what `std/envvars` sees when it
    # reads the same block itself on the C backend.
    let n = utf16ToUtf8(env, units, text)
    text[n] = '\0'
    var count = 0
    var i = 0
    while i < n:
      vec[count] = cast[cstring](addr text[i])
      inc count
      while i < n and text[i] != '\0': inc i
      inc i                                # step over this entry's NUL
    vec[count] = cast[cstring](nil)        # the NULL terminator C's `environ` carries
    gEnviron = vec
    discard cFreeEnvironmentStringsW(env)

  setupCmdLine()
  setupEnviron()
