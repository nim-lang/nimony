#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a proc to determine the number of CPUs / cores.

# runnableExamples:
#   import std/assertions
#   asssert countProcessors() > 0


when defined(js):
  import std/jsffi
  proc countProcessorsImpl(): int =
    when defined(nodejs):
      let jsOs = require("os")
      let jsObj = jsOs.cpus().length
    else:
      # `navigator.hardwareConcurrency`
      # works on browser as well as deno.
      let navigator{.importcpp.}: JsObject
      let jsObj = navigator.hardwareConcurrency
    result = jsObj.to int
else:
  when defined(posix) and not (defined(macosx) or defined(bsd)):
    import posix/posix

  when defined(linux):
    type
      CpuAffinityMask = object  ## cpu_set_t (glibc: a 1024-bit mask)
        abi: array[16, uint64]
    proc schedGetaffinity(pid: cint; setsize: csize_t; mask: pointer): cint {.
      importc: "sched_getaffinity".}
      ## Bare name: glibc's wrapper where libc is linked, and under `nimNoLibc`
      ## arkham lowers it to the raw syscall. The wrapper returns 0 on success,
      ## the syscall the number of BYTES written; both are negative on failure.

  when defined(windows):
    type
      SystemInfo = object
        u1: uint32
        dwPageSize: uint32
        lpMinimumApplicationAddress: nil pointer
        lpMaximumApplicationAddress: nil pointer
        dwActiveProcessorMask: nil ptr uint32
        dwNumberOfProcessors: uint32
        dwProcessorType: uint32
        dwAllocationGranularity: uint32
        wProcessorLevel: uint16
        wProcessorRevision: uint16

    proc getSystemInfo(lpSystemInfo: ptr SystemInfo) {.stdcall,
        dynlib: "kernel32", importc: "GetSystemInfo".}


  when defined(macosx):
    proc sysctlbyname(name: cstring,
      oldp: pointer, oldlenp: var csize_t,
      newp: nil pointer, newlen: csize_t): cint {.importc: "sysctlbyname".}

  when defined(genode):
    import genode/env

    proc affinitySpaceTotal(env: GenodeEnvPtr): cuint {.
      importcpp: "@->cpu().affinity_space().total()".}

  when defined(haiku):
    type
      SystemInfo {.importc: "system_info", header: "<OS.h>".} = object
        cpuCount {.importc: "cpu_count".}: uint32

    proc getSystemInfo(info: ptr SystemInfo): int32 {.importc: "get_system_info",
                                                      header: "<OS.h>".}

  proc countProcessorsImpl(): int {.inline.} =
    when defined(windows):
      var
        si: SystemInfo = default(SystemInfo)
      getSystemInfo(addr si)
      result = int(si.dwNumberOfProcessors)
    elif defined(macosx):
      result = 0
      let dest = addr result
      var len = sizeof(result).csize_t
      # alias of "hw.activecpu"
      if sysctlbyname("hw.logicalcpu", dest, len, nil, 0) == 0:
        return
    elif defined(hpux):
      result = mpctl(MPC_GETNUMSPUS, nil, nil)
    elif defined(irix):
      var SC_NPROC_ONLN {.importc: "_SC_NPROC_ONLN", header: "<unistd.h>".}: cint
      result = sysconf(SC_NPROC_ONLN)
    elif defined(genode):
      result = runtimeEnv.affinitySpaceTotal().int
    elif defined(haiku):
      var sysinfo: SystemInfo
      if getSystemInfo(addr sysinfo) == 0:
        result = sysinfo.cpuCount.int
      else:
        result = 0
    elif defined(linux):
      # Ask the kernel which CPUs this thread may run on and count the bits.
      # It is what `nproc` reports, and it respects a cpuset or a `taskset`,
      # which a count of installed CPUs (`sysconf`) does not: a thread pool
      # sized from the latter oversubscribes every container that has a cpuset.
      var mask = default(CpuAffinityMask)
      let n = schedGetaffinity(0.cint, csize_t(sizeof(mask)), addr mask)
      result = 0
      if n >= 0:
        # The mask starts zeroed and the kernel writes only its own cpumask
        # size, so every word can be counted.
        for i in 0 ..< mask.abi.len:
          var w = mask.abi[i]
          while w != 0'u64:
            inc result
            w = w and (w - 1'u64)     # clear the lowest set bit
      when not defined(nimNoLibc):
        if result == 0:
          result = sysconf(SC_NPROCESSORS_ONLN)
    else:
      result = sysconf(SC_NPROCESSORS_ONLN)
    if result < 0: result = 0



proc countProcessors*(): int =
  ## Returns the number of processors this process may run on. On Linux that is
  ## its CPU affinity, so a cpuset or `taskset` narrows it; elsewhere it is the
  ## number the machine has. Returns 0 if it cannot be detected.
  countProcessorsImpl()
