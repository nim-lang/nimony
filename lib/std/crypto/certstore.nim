#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The operating system's trusted root certificates, as a `TrustStore` for
## `std/crypto/x509` and `std/tls`.
##
## | Target  | Source                                                       |
## | :---    | :---                                                         |
## | Windows | the `ROOT` system store (crypt32), plus a `cacert.pem` next  |
## |         | to the executable or, failing that, on the `PATH`            |
## | POSIX   | the first CA bundle file that exists, else the certificate   |
## |         | directories (one certificate per file, `c_rehash` style)     |
##
## `SSL_CERT_FILE` (a PEM bundle) and `SSL_CERT_DIR` (a directory of PEM
## files) replace those defaults on every platform, as they do for OpenSSL.
##
## On Windows the `ROOT` store holds what has been needed so far: Windows
## fetches further Microsoft-trusted roots only when its own chain engine
## asks for them. A root that has never been fetched on a machine is missing
## here; a `cacert.pem` (curl's Mozilla bundle, for instance) fills the gap.

{.feature: "staticContracts".}

import std/[syncio, envvars, paths, dirs, strutils]
import std/crypto/x509

when defined(windows):
  import std/widestrs
  {.passL: "-lcrypt32".}   # `dynlib` externs are static imports, see std/ioring

  type
    CertContext {.pure.} = object
      ## CERT_CONTEXT: the encoded certificate and bookkeeping we ignore.
      dwCertEncodingType: uint32
      pbCertEncoded: ptr UncheckedArray[byte]
      cbCertEncoded: uint32
      pCertInfo: pointer
      hCertStore: pointer

  proc certOpenSystemStoreW(hProv: nil pointer; subsystem: WideCString): nil pointer {.
    stdcall, importc: "CertOpenSystemStoreW", dynlib: "crypt32.dll", sideEffect.}
  proc certEnumCertificatesInStore(store: pointer;
                                   prev: nil ptr CertContext): nil ptr CertContext {.
    stdcall, importc: "CertEnumCertificatesInStore", dynlib: "crypt32.dll", sideEffect.}
    ## Frees `prev`; `nil` after the last one.
  proc certCloseStore(store: pointer; flags: uint32): int32 {.
    stdcall, importc: "CertCloseStore", dynlib: "crypt32.dll", sideEffect.}
  proc getModuleFileNameW(hModule: nil pointer; filename: WideCString;
                          size: uint32): uint32 {.
    stdcall, importc: "GetModuleFileNameW", dynlib: "kernel32", sideEffect.}

  const X509_ASN_ENCODING = 1'u32

else:
  const
    Bundles =
      when defined(osx): [
        "/etc/ssl/cert.pem",
        "/System/Library/OpenSSL/certs/cert.pem"]
      else: [
        "/etc/ssl/certs/ca-certificates.crt",      # Debian, Ubuntu, Arch, Gentoo, NetBSD
        "/etc/pki/tls/certs/ca-bundle.crt",        # Fedora, RHEL
        "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",  # CentOS/RHEL 7
        "/etc/ssl/ca-bundle.pem",                  # openSUSE
        "/usr/share/ssl/certs/ca-bundle.crt",      # Red Hat 4
        "/usr/local/share/certs/ca-root-nss.crt",  # FreeBSD
        "/etc/ssl/cert.pem",                       # Alpine, OpenBSD, FreeBSD
        "/etc/certs/ca-certificates.crt",          # illumos, Solaris 11.2+
        "/data/data/com.termux/files/usr/etc/tls/cert.pem"]   # Termux
    Dirs = [
      "/etc/ssl/certs",                            # c_rehash layout
      "/etc/pki/tls/certs",                        # Fedora, RHEL
      "/system/etc/security/cacerts",              # Android
      "/usr/local/share/certs",                    # FreeBSD
      "/etc/openssl/certs"]                        # NetBSD

proc addRoot(store: var TrustStore; c: sink Certificate) =
  ## A store built from a directory sees each root several times (the file and
  ## its hash-named links); keep one.
  for r in store.roots:
    if r.der == c.der: return
  store.roots.add c

proc addFile(store: var TrustStore; path: string): int =
  ## The certificates of a PEM file that are new to `store`.
  result = 0
  var pem = ""
  try:
    pem = readFile(path)
  except:
    return
  var parsed = TrustStore(roots: @[])
  if parsed.addPem(pem) > 0:
    let before = store.roots.len
    for c in parsed.roots: store.addRoot c
    result = store.roots.len - before

proc addDir(store: var TrustStore; dir: string): int =
  result = 0
  try:
    for kind, p in walkDir(path(dir)):
      if kind in {pcFile, pcLinkToFile}:
        result += store.addFile($p)
  except:
    discard

when defined(windows):
  proc addSystemStore(store: var TrustStore; name: string): int =
    ## Every certificate of the named system store (`ROOT`, `CA`, ...).
    result = 0
    var n = name
    let s = certOpenSystemStoreW(nil, newWideCString(n.toCString).toWideCString)
    if s == nil: return
    var ctx = certEnumCertificatesInStore(s, nil)
    while ctx != nil:
      if (ctx.dwCertEncodingType and X509_ASN_ENCODING) != 0'u32 and ctx.cbCertEncoded > 0'u32:
        var c = default(Certificate)
        if parseCertificate(toOpenArray(ctx.pbCertEncoded, 0, int(ctx.cbCertEncoded) - 1), c):
          store.addRoot c
          inc result
      ctx = certEnumCertificatesInStore(s, ctx)
    discard certCloseStore(s, 0'u32)

  proc appDir(): string =
    ## The executable's directory, from the loader rather than `argv[0]`.
    var buf = newWideCString(1024)
    let n = getModuleFileNameW(nil, buf.toWideCString, 1024'u32)
    if n == 0'u32 or n >= 1024'u32: return ""
    result = $buf.toWideCString
    let sep = max(result.rfind('\\'), result.rfind('/'))
    result.setLen max(sep, 0)

  proc addCacertPem(store: var TrustStore): int =
    ## Nim's convention: `cacert.pem` next to the program, else on the `PATH`.
    const Name = "cacert.pem"
    let here = appDir()
    if here.len > 0:
      result = store.addFile(here & "\\" & Name)
      if result > 0: return
    result = 0
    for d in getEnv("PATH").split(';'):
      var dir = d
      if dir.len >= 2 and dir[0] == '"' and dir[^1] == '"':
        dir = dir.substr(1, dir.len - 2)
      if dir.len > 0:
        result = store.addFile(dir & "\\" & Name)
        if result > 0: return

proc loadSystemTrustStore*(): TrustStore =
  ## The operating system's trusted roots; see the module documentation for
  ## where they come from. Empty if none could be found, in which case every
  ## chain fails to verify — never the other way round.
  result = TrustStore(roots: @[])
  let certFile = getEnv("SSL_CERT_FILE")
  let certDir = getEnv("SSL_CERT_DIR")
  if certFile.len > 0 or certDir.len > 0:
    if certFile.len > 0: discard result.addFile(certFile)
    if certDir.len > 0:
      for d in certDir.split(when defined(windows): ';' else: ':'):
        if d.len > 0: discard result.addDir(d)
    return
  when defined(windows):
    discard result.addSystemStore("ROOT")
    discard result.addCacertPem()
  else:
    for b in Bundles:
      if result.addFile(b) > 0: return
    for d in Dirs:
      if result.addDir(d) > 0: return
