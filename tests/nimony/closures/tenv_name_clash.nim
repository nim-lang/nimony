## Two modules whose closure environments are named alike must stay distinct.
##
## Lambda lifting names an environment type `<proc>.<counter>.env.<module>`, so a
## proc called `shared` in two modules yields `shared.0.env.menvclash_a` and
## `shared.0.env.menvclash_b`. Those carry three dots, the same shape as a generic
## instantiation — and nifasm used to treat every three-dot symbol as COMDAT and
## merge the two environments onto whichever it saw first, after which one closure
## read its captures out of the other's layout.

import deps/menvclash_a
import deps/menvclash_b

menvclash_a.shared()
menvclash_b.shared()
