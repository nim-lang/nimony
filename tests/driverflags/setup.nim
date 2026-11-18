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

# nifasm's PE writer replaces the output's extension with `.exe`, including
# evaluator outputs whose temporary module name ends in `.p`.
config.backend = backendNative
config.targetOS = osWindows
assert config.executableName("nim123.p") == "nim123.exe"
assert config.executableName("nim123") == "nim123.exe"
config.targetOS = osLinux
assert config.executableName("nim123.p") == "nim123.p"
