import std / syncio

# -----------------------------------------------------------------------
# Tier 1: try/except/finally with NO passive calls inside the try body.
# Validates that the NJ guard mechanism works inside passive procs at all.
# -----------------------------------------------------------------------

proc raiser(x: int) {.raises.} =
  if x < 0:
    raise SyntaxError

proc tier1a() {.passive.} =
  try:
    raiser(-1)
  except:
    echo "tier1a: caught"

proc tier1b() {.passive.} =
  try:
    echo "tier1b: try"
  finally:
    echo "tier1b: finally"

proc tier1c() {.passive.} =
  try:
    raiser(-1)
    echo "unreachable"
  except:
    echo "tier1c: caught"
  finally:
    echo "tier1c: finally"

proc tier1d() {.passive.} =
  try:
    raiser(1)
    echo "tier1d: ok"
  finally:
    echo "tier1d: finally"

proc main1() {.passive.} =
  tier1a()
  tier1b()
  tier1c()
  tier1d()

main1()

# -----------------------------------------------------------------------
# Tier 2: passive calls INSIDE the try body — the guard/error-tracker
# variables must survive across CPS state transitions.
# -----------------------------------------------------------------------

proc work(msg: string) {.passive.} =
  echo msg

proc tier2a() {.passive.} =
  # Passive call inside try, no exception raised
  try:
    work("tier2a: work")
    echo "tier2a: after work"
  except:
    echo "tier2a: should not be reached"

proc tier2b() {.passive.} =
  # Passive call inside try, then raiser (non-passive)
  try:
    work("tier2b: work")
    raiser(-1)
    echo "tier2b: unreachable"
  except:
    echo "tier2b: caught after passive call"

proc tier2c() {.passive.} =
  # Passive call inside try, finally always runs
  try:
    work("tier2c: work")
    echo "tier2c: after work"
  finally:
    echo "tier2c: finally"

proc tier2d() {.passive.} =
  # Passive call, then raise, except + finally
  try:
    work("tier2d: work")
    raiser(-1)
    echo "tier2d: unreachable"
  except:
    echo "tier2d: caught"
  finally:
    echo "tier2d: finally"

proc tier2e() {.passive.} =
  # Two passive calls inside try (s1 and s2 must both see the guard)
  try:
    work("tier2e: first")
    work("tier2e: second")
    echo "tier2e: done"
  except:
    echo "tier2e: should not be reached"

proc main2() {.passive.} =
  tier2a()
  tier2b()
  tier2c()
  tier2d()
  tier2e()

main2()

# -----------------------------------------------------------------------
# Tier 3: Passive calls in except and finally handlers
# -----------------------------------------------------------------------

proc tier3a() {.passive.} =
  try:
    raiser(-1)
  except:
    work("tier3a: passive in except")

proc tier3b() {.passive.} =
  try:
    echo "tier3b: try"
  finally:
    work("tier3b: passive in finally")

proc main3() {.passive.} =
  tier3a()
  tier3b()

main3()
