import std/syncio
import std/crypto/x509

# Real chains captured 2026-10-13 and the four roots they lead to, judged at a
# fixed moment so the test does not start failing when they expire.
const
  Now = 1791892800'i64          # 2026-10-13 12:00 UTC
  Roots = slurp("certs/roots.pem")
  Example = slurp("certs/example.com.pem")
  Github = slurp("certs/github.com.pem")
  Google = slurp("certs/www.google.com.pem")
  Amazon = slurp("certs/www.amazon.com.pem")

proc chainOf(pem: string): seq[Certificate] =
  var s = TrustStore(roots: @[])
  discard s.addPem(pem)
  result = s.roots


proc check(store: TrustStore; name, pem, host: string) =
  let chain = chainOf(pem)
  echo name, ": ", chain.len, " certs, leaf CN=", chain[0].commonName,
       ", key ", chain[0].publicKey.kind, ", signed ", chain[0].sigAlg
  echo "  ", host, ": ", verifyChain(store, chain, host, Now)

proc main =
  var store = TrustStore(roots: @[])
  echo "roots: ", store.addPem(Roots)
  check(store, "example", Example, "example.com")
  check(store, "github", Github, "github.com")
  check(store, "google", Google, "www.google.com")
  check(store, "amazon", Amazon, "www.amazon.com")

  # the right chain for the wrong host
  echo "  evil.com: ", verifyChain(store, chainOf(Github), "evil.com", Now)
  # a wildcard covers exactly one label
  let g = chainOf(Google)
  echo "  google.com.evil.com: ", verifyChain(store, g, "google.com.evil.com", Now)
  # far in the future
  echo "  in 2040: ", verifyChain(store, chainOf(Github), "github.com", 2208988800'i64)
  # no trust anchors at all
  echo "  no roots: ", verifyChain(TrustStore(roots: @[]), chainOf(Github), "github.com", Now)
  # a single flipped bit in the leaf's signature
  var bad = chainOf(Amazon)
  bad[0].signature[10] = bad[0].signature[10] xor 1'u8
  echo "  tampered: ", verifyChain(store, bad, "www.amazon.com", Now)
  # leaf only, without the intermediate the server is supposed to send
  echo "  leaf only: ", verifyChain(store, [chainOf(Amazon)[0]], "www.amazon.com", Now)

main()
