import std/syncio
import std/crypto/[x509, certstore]

const Roots = slurp("certs/roots.pem")

proc main() =
  # whatever the machine trusts: CI runners and desktops alike have roots
  echo loadSystemTrustStore().roots.len > 0

  # a bundle file (`SSL_CERT_FILE`): exactly its certificates, in order
  var expected = TrustStore(roots: @[])
  let n = expected.addPem(Roots)
  let s = loadTrustStore("tests/nimony/crypto/certs/roots.pem", "")
  echo s.roots.len == n
  var same = true
  for i in 0 ..< n:
    if s.roots[i].der != expected.roots[i].der: same = false
  echo same

  # a directory (`SSL_CERT_DIR`): every file in it, each certificate once
  let d = loadTrustStore("", "tests/nimony/crypto/certs")
  var dup = false
  for i in 0 ..< d.roots.len:
    for j in i + 1 ..< d.roots.len:
      if d.roots[i].der == d.roots[j].der: dup = true
  echo d.roots.len > n, " ", dup

main()
