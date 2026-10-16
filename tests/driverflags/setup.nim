import std/assertions
import "../../src/nimony/nifconfig"
import "../../src/lib/platform"

# Driver defaults follow the target, independently of the compiler's host.
var config = initNifConfig("")
for targetOS in TSystemOS:
  for targetCPU in TSystemCPU:
    config.targetOS = targetOS
    config.targetCPU = targetCPU
    let flags = targetDriverFlags(config)
    if targetOS in {osSolaris, osIllumos} and targetCPU == cpuAmd64:
      assert flags == @["-m64"]
    else:
      assert flags.len == 0
