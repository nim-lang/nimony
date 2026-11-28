## This module contains SSL configuration parameters obtained from
## `Mozilla OpSec <https://wiki.mozilla.org/Security/Server_Side_TLS>`_.
##
## The configuration file used to generate this module: https://ssl-config.mozilla.org/guidelines/6.0.json

const CiphersModern* = "TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256"
  ## An OpenSSL-compatible list of secure ciphers for `modern` compatibility
  ## per Mozilla's recommendations.
  ##
  ## Oldest clients supported by this list:
  ## * Firefox 63
  ## * Android 10.0
  ## * Chrome 70
  ## * Edge 75
  ## * Java 11
  ## * OpenSSL 1.1.1
  ## * Opera 57
  ## * Safari 12.1

const CiphersIntermediate* = "TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305"
  ## An OpenSSL-compatible list of secure ciphers for `intermediate` compatibility
  ## per Mozilla's recommendations.
  ##
  ## Oldest clients supported by this list:
  ## * Firefox 31.3.0
  ## * Android 4.4.2
  ## * Chrome 49
  ## * Edge 15 on Windows 10
  ## * IE 11 on Windows 10
  ## * Java 8u161
  ## * OpenSSL 1.0.1l
  ## * Opera 20
  ## * Safari 9
