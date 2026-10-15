import std/[syncio, envvars]
import std/crypto/[x509, certstore]

const Roots = slurp("certs/roots.pem")

proc main() {.raises.} =
  # whatever the machine trusts: CI runners and desktops alike have roots
  echo loadSystemTrustStore().roots.len > 0

  # SSL_CERT_FILE replaces the defaults
  var expected = TrustStore(roots: @[])
  let n = expected.addPem(Roots)
  putEnv("SSL_CERT_FILE", "tests/nimony/crypto/certs/roots.pem")
  let s = loadSystemTrustStore()
  echo s.roots.len == n
  var same = true
  for i in 0 ..< n:
    if s.roots[i].der != expected.roots[i].der: same = false
  echo same

  # SSL_CERT_DIR adds every file in it, each certificate once
  putEnv("SSL_CERT_FILE", "")
  putEnv("SSL_CERT_DIR", "tests/nimony/crypto/certs")
  let d = loadSystemTrustStore()
  var dup = false
  for i in 0 ..< d.roots.len:
    for j in i + 1 ..< d.roots.len:
      if d.roots[i].der == d.roots[j].der: dup = true
  echo d.roots.len > n, " ", dup

try:
  main()
except ErrorCode as e:
  echo "FAILED: ", e
