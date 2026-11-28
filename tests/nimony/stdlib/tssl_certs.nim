import std/[algorithm, assertions, dirs, envvars, os, paths, ssl_certs,
            syncio]

proc collect(useEnvVars: bool): seq[string] =
  result = @[]
  for fn in scanSSLCertificates(useEnvVars):
    result.add fn

proc main() {.raises.} =
  let base = path(getTempDir()) / path("nimony_ssl_certs_test")
  removeDir(base)
  createDir(base)
  let certFile = base / path("ca.pem")
  let certFile2 = base / path("other.crt")
  writeFile($certFile, "cert")
  writeFile($certFile2, "cert")
  createDir(base / path("subdir"))
  writeFile($(base / path("subdir") / path("ignored.pem")), "cert")

  let hadFile = existsEnv("SSL_CERT_FILE")
  let oldFile = if hadFile: getEnv("SSL_CERT_FILE") else: ""
  let hadDir = existsEnv("SSL_CERT_DIR")
  let oldDir = if hadDir: getEnv("SSL_CERT_DIR") else: ""

  try:
    # SSL_CERT_FILE takes precedence over SSL_CERT_DIR.
    putEnv("SSL_CERT_FILE", $certFile)
    putEnv("SSL_CERT_DIR", $base)
    assert collect(true) == @[$certFile]

    # SSL_CERT_DIR yields the files directly in the directory, not
    # subdirectories or their contents.
    delEnv("SSL_CERT_FILE")
    var got = collect(true)
    sort(got)
    var expected = @[$certFile, $certFile2]
    sort(expected)
    assert got == expected

    # useEnvVars = false ignores both environment variables.
    putEnv("SSL_CERT_FILE", $certFile)
    for fn in scanSSLCertificates(false):
      assert fn != $certFile
      assert fn != $base
  finally:
    if hadFile: putEnv("SSL_CERT_FILE", oldFile) else: delEnv("SSL_CERT_FILE")
    if hadDir: putEnv("SSL_CERT_DIR", oldDir) else: delEnv("SSL_CERT_DIR")
    removeDir(base)

  echo "ssl_certs: ok"

try:
  main()
except:
  quit "ssl_certs test failed"
