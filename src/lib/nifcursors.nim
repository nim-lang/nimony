## Facade kept for import compatibility: the classic token/cursor library has
## been replaced by nifcore. See nifcore_compat.nim while the remaining
## call-site renames are dissolved; new code should import nifcore directly.
import nifcore_compat
export nifcore_compat
