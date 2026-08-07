## Two modules whose closure environments are named alike must stay distinct.
##
## Lambda lifting derives an environment type from the proc it belongs to, so a
## proc called `shared` in two modules yields two environments distinguished only
## by their module suffix — today `shared`env.0.menvclash_a`.
##
## It used to be `shared.0.env.menvclash_a`. nif-spec.md gives a global symbol as
## `<ident>.<disamb>.<moduleSuffix>` or `<ident>.<disamb>.<key>.<moduleSuffix>`,
## so that spelling put `env` in the KEY slot — the slot that says "which
## instantiation of `shared.0` is this", and whose answer every module derives
## identically. Backends collapse copies on it, so nifasm COMDAT-merged the two
## environments onto whichever it saw first and one closure then read its captures
## out of the other's layout. An environment is private to its module and has no
## business claiming a key at all.
##
## The two environments below deliberately differ in BOTH field count and field
## names, so a merge cannot go unnoticed.

import deps/menvclash_a
import deps/menvclash_b

menvclash_a.shared()
menvclash_b.shared()
