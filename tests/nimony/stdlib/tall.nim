# Every module in `lib/std`, imported into one program. Two things depend on
# this list being complete, which is why `hastur` fails the run when it drifts
# from the directory (`src/hastur/coverage.nim`):
#
# * it is the one test that says the stdlib modules still compile *together* —
#   every other stdlib test compiles a handful of them;
# * `dagon` walks it as the aggregator driver for the website's documentation,
#   so a module missing here is a module missing from the docs.
#
# Sorted, one import per line, nothing else: the check is a line scan.

import std/algorithm
import std/appdirs
import std/assertions
import std/atomics
import std/base64
import std/bitops
import std/cmdline
import std/compilation
import std/complex
import std/cpuinfo
import std/deques
import std/dirs
import std/editdistance
import std/encodings
import std/envvars
import std/fenv
import std/formatfloat
import std/hashes
import std/heapqueue
import std/http/httpconn
import std/http/httpmsg
import std/http/httpparse
import std/http/httpwire
import std/intsets
import std/ioring
import std/json
import std/lexbase
import std/locks
import std/macros
import std/math
import std/md5
import std/memfiles
import std/monotimes
import std/nativesocket
import std/nifply
import std/opt
import std/options
import std/os
import std/oserrors
import std/osproc
import std/packedsets
import std/parfor
import std/parsejson
import std/parseopt
import std/parseutils
import std/pathnorm
import std/paths
import std/random
import std/rawthreads
import std/result
import std/rlocks
import std/sequtils
import std/sets
import std/setutils
import std/sha1
import std/smartcli
import std/socket
import std/stacktraces
import std/streams
import std/stripes
import std/strtabs
import std/strutils
import std/syncio
import std/system
import std/tables
import std/terminal
import std/threadpool
import std/ticketlocks
import std/times
import std/typetraits
import std/unicode
import std/varints
import std/volatile
import std/widestrs
import std/wordwrap
import std/writenif

echo "ok"
