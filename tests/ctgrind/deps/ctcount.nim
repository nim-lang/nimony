# Parses one PEM private key and signs one message with it, for
# `tests/ctgrind/setup.nim`, which runs this under callgrind, counting only
# inside the secret-dependent routines (`--toggle-collect`), and demands the
# same instruction, memory-access and branch counts for every key and
# message: a count that depends on a secret is a timing leak. The blinding
# factors are fresh random numbers in every run, so equal counts also show
# that the blinding does not steer the computation.
#
#   printf '%s\n%s\n' KEY.pem MESSAGE | ctcount
#
# The key path and the message come on stdin, not on the command line: the
# length of the arguments moves the initial stack, and with it the alignment
# of the stack buffers that glibc's `memcpy` copies, which picks its code
# path by alignment and so changes the count by a few hundred instructions
# (seen with gcc and clang). Every run then starts from the same stack.

import std / syncio
import std / crypto / [rsa, rsasign]

proc main() =
  var path = ""
  var text = ""
  if not readLine(stdin, path) or not readLine(stdin, text):
    quit "usage: printf '%s\\n%s\\n' KEY.pem MESSAGE | ctcount"
  var pemText = ""
  try:
    pemText = readFile(path)
  except:
    quit "cannot read " & path
  var key = default(RsaPrivateKey)
  if parseRsaPrivateKeyPem(pemText, key) != rkeNone: quit "key refused"
  var msg: seq[byte] = @[]
  for c in text: msg.add byte(c)
  var sig: seq[byte] = @[]
  if rsaSignPkcs1v15(key, rhSha256, msg, sig) != rseNone: quit "signing failed"
  if not rsaVerifyPkcs1v15(publicKey(key), rhSha256, msg, sig): quit "wrong signature"
  echo "ok"

main()
