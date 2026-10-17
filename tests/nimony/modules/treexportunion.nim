# A module reachable through several export paths exports the union of
# what each path allows.

import deps/[mreexportunion_full, mreexportunion_fullrev, mreexportunion_onlys,
  mreexportunion_exceptonly, mreexportunion_excepts]

# full + only ua = all
mreexportunion_full.ua()
mreexportunion_full.ub()
mreexportunion_full.ud()
mreexportunion_fullrev.ua()
mreexportunion_fullrev.ub()
mreexportunion_fullrev.ud()

# only ua + only ub = {ua, ub}
mreexportunion_onlys.ua()
mreexportunion_onlys.ub()

# except ua + only ua = all
mreexportunion_exceptonly.ua()
mreexportunion_exceptonly.ub()
mreexportunion_exceptonly.ud()

# except ua + except ua, ud = all except ua
mreexportunion_excepts.ub()
mreexportunion_excepts.ud()

# unqualified
ua()
ub()
ud()
