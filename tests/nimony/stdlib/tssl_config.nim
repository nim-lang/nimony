import std/[assertions, ssl_config, syncio]

# Keep these exact: the module is generated data, so a typo is a real
# interoperability regression rather than a formatting issue.
# Mozilla guidelines 6.0 dropped the legacy `old` profile, so there is no
# `CiphersOld` anymore.
assert CiphersModern ==
  "TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256"

assert CiphersIntermediate ==
  "TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:" &
  "ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:" &
  "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:" &
  "ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305"

echo "ssl_config: ok"
