
import std/oserrors

import oscommons
export symlinkExists

import ../[syncio, assertions, widestrs]

when weirdTarget:
  discard
elif defined(windows):
  import ../windows/winlean

  # Byte offsets into a `REPARSE_DATA_BUFFER`.
  const
    reparseHeaderSize = 8
    substituteNameOffsetField = 8
    substituteNameLengthField = 10
    symlinkFlagsField = 16
    mountPointPathBufferOffset = 16
    symlinkPathBufferOffset = 20

  type
    ReparseBuffer = array[MAXIMUM_REPARSE_DATA_BUFFER_SIZE + 2, byte]
      ## Two spare bytes beyond what `DeviceIoControl` may fill, so the
      ## substitute name can always be NUL-terminated in place.

    ReparseLinkInfo = object
      tag: uint32
      flags: uint32
      nameStart: int ## byte offset of the substitute name in the buffer
      nameLen: int   ## its length in bytes

  proc readU16(buf: ReparseBuffer; off: int): int {.inline.} =
    int(buf[off]) or (int(buf[off + 1]) shl 8)

  proc readU32(buf: ReparseBuffer; off: int): uint32 {.inline.} =
    uint32(buf[off]) or (uint32(buf[off + 1]) shl 8) or
      (uint32(buf[off + 2]) shl 16) or (uint32(buf[off + 3]) shl 24)

  func lowerAscii(c: int): int {.inline.} =
    if c >= ord('A') and c <= ord('Z'): c + 32 else: c

  proc startsWithAsciiIgnoreCase(w: WideCString; prefix: string): bool =
    ## Matches an ASCII `prefix` against the NUL-terminated `w`. Only valid
    ## for ASCII prefixes; not a general case-insensitive UTF-16 comparison.
    result = true
    for i in 0 ..< prefix.len:
      let c = int(cast[uint16](w[i]))
      if c > 0x7F or lowerAscii(c) != lowerAscii(ord(prefix[i])):
        return false

  proc decodeWinTarget(w: WideCString): string =
    ## Strips the NT namespace prefix from an absolute substitute name.
    if w.startsWithAsciiIgnoreCase(r"\??\UNC\"):
      result = r"\\" & $cast[WideCString](addr w[8])
    elif w.startsWithAsciiIgnoreCase(r"\??\"):
      result = $cast[WideCString](addr w[4])
    else:
      result = $w

  proc parseReparseLinkInfo(buf: ReparseBuffer;
                            bytesReturned: int): ReparseLinkInfo {.raises.} =
    ## Validates the header of a symlink or junction reparse buffer. Malformed
    ## data raises `DiskCorruption`; any other reparse tag `BadOperation`.
    if bytesReturned < reparseHeaderSize:
      raise DiskCorruption
    let wholeDataLen = reparseHeaderSize + readU16(buf, 4)
    if wholeDataLen > bytesReturned:
      raise DiskCorruption

    result = ReparseLinkInfo(tag: readU32(buf, 0))
    var pathBufOffset = 0
    if result.tag == IO_REPARSE_TAG_SYMLINK:
      pathBufOffset = symlinkPathBufferOffset
      if pathBufOffset > wholeDataLen:
        raise DiskCorruption
      result.flags = readU32(buf, symlinkFlagsField)
    elif result.tag == IO_REPARSE_TAG_MOUNT_POINT:
      pathBufOffset = mountPointPathBufferOffset
      if pathBufOffset > wholeDataLen:
        raise DiskCorruption
    else:
      raise BadOperation

    let nameOffset = readU16(buf, substituteNameOffsetField)
    result.nameLen = readU16(buf, substituteNameLengthField)
    if result.nameLen <= 0 or (nameOffset and 1) != 0 or
        (result.nameLen and 1) != 0:
      raise DiskCorruption
    result.nameStart = pathBufOffset + nameOffset
    if result.nameStart + result.nameLen > wholeDataLen:
      raise DiskCorruption

elif defined(posix):
  import ../posix/posix


when weirdTarget:
  {.pragma: noWeirdTarget, error: "this proc is not available on the NimScript/js target".}
else:
  {.pragma: noWeirdTarget.}


when defined(nimscript):
  # for procs already defined in scriptconfig.nim
  template noNimJs(body): untyped = discard
elif defined(js):
  {.pragma: noNimJs, error: "this proc is not available on the js target".}
else:
  {.pragma: noNimJs.}

## .. importdoc:: os.nim

proc createSymlink*(src, dest: string) {.noWeirdTarget, raises.} =
  ## Create a symbolic link at `dest` which points to the item specified
  ## by `src`. On most operating systems, will fail if a link already exists.
  ##
  ## .. warning:: Some OS's (such as Microsoft Windows) restrict the creation
  ##   of symlinks to root users (administrators) or users with developer mode enabled.
  ##
  ## See also:
  ## * `createHardlink proc`_
  ## * `expandSymlink proc`_


  var src = src
  var dest = dest

  when defined(windows):
    const SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 2'u32
    # allows anyone with developer mode on to create a link
    let flag = dirExists(src).uint32 or SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE

    var wSrc = newWideCString(src).toWideCString()
    var wDst = newWideCString(dest).toWideCString()
    if createSymbolicLinkW(wDst, wSrc, flag) == 0 or getLastError() != 0:
      raiseOSError(osLastError(), "(" & src & ", " & dest & ")")
  else:
    if symlink(src.toCString(), dest.toCString()) != 0:
      raiseOSError(osLastError(), "(" & src & ", " & dest & ")")

proc createHardlink*(src, dest: string) {.noWeirdTarget, raises.} =
  ## Create a hard link at `dest` which points to the item specified
  ## by `src`.
  ##
  ## .. warning:: Some OS's restrict the creation of hard links to
  ##   root users (administrators).
  ##
  ## See also:
  ## * `createSymlink proc`_
  var src = src
  var dest = dest

  when defined(windows):
    var wSrc = newWideCString(src).toWideCString()
    var wDst = newWideCString(dest).toWideCString()
    if createHardLinkW(wDst, wSrc, nil) == 0:
      raiseOSError(osLastError(), "(" & src & ", " & dest & ")")
  else:
    if link(src.toCString(), dest.toCString()) != 0:
      raiseOSError(osLastError(), "(" & src & ", " & dest & ")")

proc expandSymlink*(symlinkPath: string): string {.noWeirdTarget, raises.} =
  ## Returns the stored target of the symbolic link `symlinkPath`.
  ##
  ## This expands exactly one level of indirection, like POSIX `readlink`.
  ## If the target is itself a symbolic link, it is returned as-is rather than
  ## being expanded further.
  ##
  ## On Windows, this supports symbolic links and junctions by reading the
  ## reparse point payload directly; the NT `\??\` prefix of absolute targets
  ## is stripped. Unsupported reparse tags raise `BadOperation`.
  ##
  ## On Nintendo Switch this is currently a noop: `symlinkPath` is simply
  ## returned, without checking whether it is actually a symbolic link.
  ##
  ## See also:
  ## * `createSymlink proc`_
  when defined(windows):
    let handle = openHandle(symlinkPath, followSymlink = false)
    if handle == INVALID_HANDLE_VALUE:
      raiseOSError(osLastError(), symlinkPath)

    var buf = default(ReparseBuffer)
    var bytesReturned = 0'u32
    let ok = deviceIoControl(handle, FSCTL_GET_REPARSE_POINT, nil, 0'u32,
                             addr buf[0], DWORD(MAXIMUM_REPARSE_DATA_BUFFER_SIZE),
                             bytesReturned, nil)
    let err = osLastError()
    discard closeHandle(handle)
    if ok.isFail:
      raiseOSError(err, symlinkPath)

    let info = parseReparseLinkInfo(buf, int(bytesReturned))
    # The substitute name is not NUL-terminated; terminate it in place
    # (`ReparseBuffer` has room past the largest possible payload).
    let nameEnd = info.nameStart + info.nameLen
    buf[nameEnd] = 0'u8
    buf[nameEnd + 1] = 0'u8
    let name = cast[WideCString](addr buf[info.nameStart])
    if info.tag == IO_REPARSE_TAG_SYMLINK and
        (info.flags and SYMLINK_FLAG_RELATIVE) != 0'u32:
      result = $name
    else:
      result = decodeWinTarget(name)
  elif defined(nintendoswitch):
    result = symlinkPath
  else:
    var bufLen = 1024
    while true:
      result = newString(bufLen)
      var symlinkPath = symlinkPath
      let len = readlink(symlinkPath.toCString(), result.toCString(), bufLen)
      if len < 0:
        raiseOSError(osLastError(), symlinkPath)
      if len < bufLen:
        result.setLen(len)
        break
      bufLen = bufLen shl 1
