#!/usr/bin/env python3
# Writes the three throwaway keys of tctbignum.nim's `CrtKeys` (2048a, 3072,
# 4096) back to PEM files, for ctbignum_vectors.py (not run by the suite).
#
#   python3 -P ctbignum_keys.py ../tctbignum.nim KEYDIR
#
# The test holds n, e, p, q, dP, dQ and qInv; d is recomputed as
# e^-1 mod lcm(p-1, q-1), as `openssl genpkey` makes it.
import math
import os
import re
import sys

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import rsa

src, keydir = sys.argv[1:3]
os.makedirs(keydir, exist_ok=True)
pattern = (r'\(name: "(\w+)", n: "(\w+)", e: "(\w+)",\s*p: "(\w+)",\s*q: "(\w+)",'
           r'\s*dP: "(\w+)",\s*dQ: "(\w+)",\s*qInv: "(\w+)"')
for m in re.finditer(pattern, open(src).read()):
    name = m.group(1)
    n, e, p, q, dp, dq, qi = (int(x, 16) for x in m.groups()[1:])
    d = pow(e, -1, math.lcm(p - 1, q - 1))
    key = rsa.RSAPrivateNumbers(p, q, d, dp, dq, qi, rsa.RSAPublicNumbers(e, n)).private_key()
    with open(os.path.join(keydir, name + ".pem"), "wb") as f:
        f.write(key.private_bytes(serialization.Encoding.PEM,
                                  serialization.PrivateFormat.PKCS8,
                                  serialization.NoEncryption()))
    print(name)
