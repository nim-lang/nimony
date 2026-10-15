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
## On Windows the `ROOT` store holds only what has been needed so far:
## Windows fetches further Microsoft-trusted roots when its own chain engine
## asks for them. So `verifyServerChain`, which `std/tls` uses, lets that
## engine (`CertGetCertificateChain`) judge a chain against the system store
## rather than the roots enumerated here.

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

  # The chain engine: what `verifyServerChain` hands a server's chain to.
  type
    CertEnhkeyUsage {.pure.} = object
      cUsageIdentifier: uint32
      rgpszUsageIdentifier: ptr cstring
    CertUsageMatch {.pure.} = object
      ## A nested struct, so `usage` is pointer-aligned: not flattenable.
      dwType: uint32
      usage: CertEnhkeyUsage
    CertChainPara {.pure.} = object
      ## The original CERT_CHAIN_PARA; `cbSize` tells crypt32 that the later
      ## fields (issuance policy, timeouts, ...) are absent, so all default.
      cbSize: uint32
      requestedUsage: CertUsageMatch
    SslExtraPolicyPara {.pure.} = object
      ## SSL_EXTRA_CERT_CHAIN_POLICY_PARA
      cbSize: uint32
      dwAuthType: uint32
      fdwChecks: uint32
      pwszServerName: WideCString
    CertChainPolicyPara {.pure.} = object
      cbSize: uint32
      dwFlags: uint32
      pvExtraPolicyPara: pointer
    CertChainPolicyStatus {.pure.} = object
      cbSize: uint32
      dwError: uint32
      lChainIndex: int32
      lElementIndex: int32
      pvExtraPolicyStatus: nil pointer

  const
    CERT_STORE_PROV_MEMORY = 2           # an LPCSTR provider id, not a string
    CERT_STORE_ADD_ALWAYS = 4'u32
    CERT_CHAIN_POLICY_SSL = 4            # likewise an id cast to LPCSTR
    USAGE_MATCH_TYPE_AND = 0'u32
    AUTHTYPE_SERVER = 2'u32
    CERT_E_EXPIRED = 0x800B0101'u32
    CERT_E_UNTRUSTEDROOT = 0x800B0109'u32
    CERT_E_CHAINING = 0x800B010A'u32
    CERT_E_CN_NO_MATCH = 0x800B010F'u32
    CERT_E_WRONG_USAGE = 0x800B0110'u32
    TRUST_E_CERT_SIGNATURE = 0x80096004'u32
    CERT_E_INVALID_NAME = 0x800B0114'u32

  proc certCreateCertificateContext(enc: uint32; pb: pointer; cb: uint32): nil ptr CertContext {.
    stdcall, importc: "CertCreateCertificateContext", dynlib: "crypt32.dll", sideEffect.}
  proc certFreeCertificateContext(ctx: ptr CertContext): int32 {.
    stdcall, importc: "CertFreeCertificateContext", dynlib: "crypt32.dll", sideEffect.}
  proc certOpenStore(provider: pointer; enc: uint32; hProv: nil pointer; flags: uint32;
                     para: nil pointer): nil pointer {.
    stdcall, importc: "CertOpenStore", dynlib: "crypt32.dll", sideEffect.}
  proc certAddEncodedCertificateToStore(store: pointer; enc: uint32; pb: pointer;
                                        cb: uint32; disposition: uint32;
                                        added: nil pointer): int32 {.
    stdcall, importc: "CertAddEncodedCertificateToStore", dynlib: "crypt32.dll", sideEffect.}
  proc certGetCertificateChain(engine: nil pointer; ctx: ptr CertContext;
                               time: nil pointer; additional: nil pointer;
                               para: ptr CertChainPara; flags: uint32;
                               reserved: nil pointer; chain: ptr (nil pointer)): int32 {.
    stdcall, importc: "CertGetCertificateChain", dynlib: "crypt32.dll", sideEffect.}
    ## Builds the chain with the system engine: the server's intermediates
    ## from `additional`, missing ones fetched by AIA, roots from the
    ## (auto-updated) system stores, distrust from `Disallowed`.
  proc certVerifyCertificateChainPolicy(policy: pointer; chain: pointer;
                                        para: ptr CertChainPolicyPara;
                                        status: ptr CertChainPolicyStatus): int32 {.
    stdcall, importc: "CertVerifyCertificateChainPolicy", dynlib: "crypt32.dll", sideEffect.}
  proc certFreeCertificateChain(chain: pointer) {.
    stdcall, importc: "CertFreeCertificateChain", dynlib: "crypt32.dll", sideEffect.}

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

  proc toCertError(code: uint32): CertError =
    case code
    of 0'u32: ceNone
    of CERT_E_EXPIRED: ceExpired
    of CERT_E_CN_NO_MATCH, CERT_E_INVALID_NAME: ceHostMismatch
    of CERT_E_WRONG_USAGE: ceUsage
    of TRUST_E_CERT_SIGNATURE: ceBadSignature
    else: ceUntrusted          # untrusted or distrusted root, revoked, ...

  proc systemVerify(chain: openArray[Certificate]; host: string): uint32 =
    ## Windows' own verdict on `chain` for `host`: the chain built by the
    ## system engine, judged by the SSL policy (trust, validity, name, usage).
    ## A Win32 error code; `0` is success.
    result = CERT_E_UNTRUSTEDROOT
    if chain.len == 0: return
    let leafDer = chain[0].der
    if leafDer.len == 0: return
    let leaf = certCreateCertificateContext(X509_ASN_ENCODING, addr leafDer[0],
                                            uint32(leafDer.len))
    if leaf == nil: return
    let extra = certOpenStore(cast[pointer](CERT_STORE_PROV_MEMORY), 0'u32, nil, 0'u32, nil)
    if extra != nil:
      for i in 1 ..< chain.len:
        let der = chain[i].der
        if der.len > 0:
          discard certAddEncodedCertificateToStore(extra, X509_ASN_ENCODING,
            addr der[0], uint32(der.len), CERT_STORE_ADD_ALWAYS, nil)
    var serverAuth = cstring"1.3.6.1.5.5.7.3.1"    # szOID_PKIX_KP_SERVER_AUTH
    var para = CertChainPara(cbSize: uint32(sizeof(CertChainPara)),
      requestedUsage: CertUsageMatch(dwType: USAGE_MATCH_TYPE_AND, usage: CertEnhkeyUsage(
        cUsageIdentifier: 1'u32, rgpszUsageIdentifier: addr serverAuth)))
    var ctx: nil pointer = nil
    if certGetCertificateChain(nil, leaf, nil, extra, addr para, 0'u32, nil,
                               addr ctx) != 0'i32 and ctx != nil:
      var h = host
      var name = newWideCString(h.toCString)
      var ssl = SslExtraPolicyPara(cbSize: uint32(sizeof(SslExtraPolicyPara)),
        dwAuthType: AUTHTYPE_SERVER, fdwChecks: 0'u32,
        pwszServerName: name.toWideCString)
      var policy = CertChainPolicyPara(cbSize: uint32(sizeof(CertChainPolicyPara)),
        dwFlags: 0'u32, pvExtraPolicyPara: addr ssl)
      var status = CertChainPolicyStatus(cbSize: uint32(sizeof(CertChainPolicyStatus)),
        dwError: 0'u32, lChainIndex: 0'i32, lElementIndex: 0'i32,
        pvExtraPolicyStatus: nil)
      if certVerifyCertificateChainPolicy(cast[pointer](CERT_CHAIN_POLICY_SSL), ctx,
                                          addr policy, addr status) != 0'i32:
        result = status.dwError
      certFreeCertificateChain(ctx)
    if extra != nil: discard certCloseStore(extra, 0'u32)
    discard certFreeCertificateContext(leaf)

proc verifyServerChain*(store: TrustStore; chain: openArray[Certificate];
                        host: string; now: int64): CertError =
  ## `x509.verifyChain`, except that on Windows a `system` store lets the
  ## OS's chain engine judge: it knows roots Windows has not yet downloaded,
  ## fetches intermediates a server forgot, and honours the `Disallowed`
  ## store. A chain Windows only fails for want of a root still gets checked
  ## against `store`'s own roots, which is where a `cacert.pem` ends up.
  ##
  ## Building the chain can go to the network (AIA, root updates), so on
  ## Windows this may block for a while.
  when defined(windows):
    if store.system and chain.len > 0:
      let code = systemVerify(chain, host)
      if code == CERT_E_UNTRUSTEDROOT or code == CERT_E_CHAINING:
        return verifyChain(store, chain, host, now)
      return toCertError(code)
  result = verifyChain(store, chain, host, now)

proc loadTrustStore*(certFile, certDir: string): TrustStore =
  ## The roots of a PEM bundle `certFile` and of the PEM files in `certDir`
  ## (a `PATH`-style list of directories); either may be empty. What
  ## `SSL_CERT_FILE` and `SSL_CERT_DIR` select. Each certificate is kept once.
  result = TrustStore(roots: @[])
  if certFile.len > 0: discard result.addFile(certFile)
  if certDir.len > 0:
    for d in certDir.split(when defined(windows): ';' else: ':'):
      if d.len > 0: discard result.addDir(d)

proc loadSystemTrustStore*(): TrustStore =
  ## The operating system's trusted roots; see the module documentation for
  ## where they come from. Empty if none could be found, in which case every
  ## chain fails to verify — never the other way round.
  let certFile = getEnv("SSL_CERT_FILE")
  let certDir = getEnv("SSL_CERT_DIR")
  if certFile.len > 0 or certDir.len > 0:
    return loadTrustStore(certFile, certDir)
  result = TrustStore(roots: @[], system: true)
  when defined(windows):
    discard result.addSystemStore("ROOT")
    discard result.addCacertPem()
  else:
    for b in Bundles:
      if result.addFile(b) > 0: return
    for d in Dirs:
      if result.addDir(d) > 0: return
