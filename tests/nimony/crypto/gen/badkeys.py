#!/usr/bin/env python3
# Writes the deliberately broken keys of rsakeys/bad/*.der, each one rule of
# parseRsaPrivateKeyDer broken in test2048 (not run by the suite).
#
#   python3 -P badkeys.py RSAKEYS_DIR
#
# Reads RSAKEYS_DIR/test2048.pkcs1.der and writes RSAKEYS_DIR/bad/NAME.der.
# Standard library only. The output is deterministic: running it again must
# leave `git status` clean.
import os
import sys

FIELDS = ["version", "n", "e", "d", "p", "q", "dP", "dQ", "qInv"]


def length(n, width=0):
    # DER length octets; `width` forces a long form of that many octets
    if width == 0 and n < 0x80:
        return bytes([n])
    body = n.to_bytes(max(width, (n.bit_length() + 7) // 8), "big")
    return bytes([0x80 | len(body)]) + body


def tlv(tag, content, width=0):
    return bytes([tag]) + length(len(content), width) + content


def integer(x):
    # minimal two's complement content of a non-negative INTEGER
    return x.to_bytes(x.bit_length() // 8 + 1, "big")


def read_tlv(d, p):
    # tag, contents and end of the TLV at `p`
    tag, n = d[p], d[p + 1]
    p += 2
    if n & 0x80:
        k = n & 0x7F
        n = int.from_bytes(d[p:p + k], "big")
        p += k
    return tag, d[p:p + n], p + n


def read_key(der):
    # the nine INTEGERs of an RSAPrivateKey, as Python ints
    tag, body, end = read_tlv(der, 0)
    assert tag == 0x30 and end == len(der)
    values, p = {}, 0
    for name in FIELDS:
        tag, content, p = read_tlv(body, p)
        assert tag == 0x02
        values[name] = int.from_bytes(content, "big")
    assert p == len(body)
    return values


def key(v, raw=None, extra=b"", drop=None, seqwidth=0):
    # an RSAPrivateKey from the ints `v`; `raw` replaces a field's whole TLV
    raw = raw or {}
    body = b""
    for name in FIELDS:
        if name == drop:
            continue
        body += raw.get(name, tlv(0x02, integer(v[name])))
    return tlv(0x30, body + extra, seqwidth)


def changed(v, **fields):
    w = dict(v)
    w.update(fields)
    return w


def main():
    keydir = sys.argv[1]
    der = open(os.path.join(keydir, "test2048.pkcs1.der"), "rb").read()
    v = read_key(der)
    assert key(v) == der, "the encoder does not reproduce test2048"
    n, e, p, q = v["n"], v["e"], v["p"], v["q"]
    bad = {
        # version 1: a multi-prime key (OtherPrimeInfos), not supported
        "version1": key(changed(v, version=1)),
        "even-e": key(changed(v, e=e + 1)),
        "e-one": key(changed(v, e=1)),
        "e-too-big": key(changed(v, e=n + 2)),        # odd, but not below n
        "modulus-small": key(changed(v, n=n >> 1100)),  # 948 bits
        "modulus-large": key(changed(v, n=(n << 2049) | 1)),  # 4097 bits, odd
        "p-even": key(changed(v, p=p + 1)),
        "p-wide": key(changed(v, p=(p << 40) + 1)),    # 40 bits past half of n
        "q-narrow": key(changed(v, q=(q >> 40) | 1)),  # odd, 40 bits short
        "pq-not-n": key(changed(v, p=p + 2)),          # odd and right-sized
        "dp-wide": key(changed(v, dP=v["dP"] + (0x10 << (137 * 8)))),
        "qi-wide": key(changed(v, qInv=v["qInv"] + (0x10 << (137 * 8)))),
        # a 0xFF sign byte makes dQ negative
        "negative-dq": key(v, raw={"dQ": tlv(0x02, b"\xff" + integer(v["dQ"]))}),
        # 65537 with a redundant leading zero octet
        "nonminimal-e": key(v, raw={"e": tlv(0x02, b"\x00" + integer(e))}),
        "missing-qi": key(v, drop="qInv"),
        "extra-field": key(v, extra=tlv(0x02, b"\x05")),
        "trailing-byte": der + b"\x00",
        # the outer length in 4 octets instead of 2
        "long-length": key(v, seqwidth=4),
        # n's length 0x101 as 83 00 01 01: a leading zero length octet
        "nonminimal-length": key(v, raw={"n": tlv(0x02, integer(n), 3)}),
        # parses (dP is in range) but signs wrongly: the fault check must refuse
        "dp-corrupt": key(changed(v, dP=v["dP"] + 2)),
    }
    os.makedirs(os.path.join(keydir, "bad"), exist_ok=True)
    for name, data in bad.items():
        with open(os.path.join(keydir, "bad", name + ".der"), "wb") as f:
            f.write(data)
    print(len(bad), "keys")


main()
