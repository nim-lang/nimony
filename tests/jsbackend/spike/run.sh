#!/usr/bin/env bash
# Runs the Typed-Array linear-memory spike (M2 direction, PR #2043). Needs only
# `node` (v20+ for resizable ArrayBuffer; the repo uses v25). This is a spike, so
# it is NOT part of the shipping `run.sh` golden suite — it validates the model
# the codegen will target, not the current codegen.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
if ! command -v node >/dev/null 2>&1; then
  echo "node not found; skipping spike"; exit 0
fi
node "$here/model.spike.js"
echo "spike: OK"
