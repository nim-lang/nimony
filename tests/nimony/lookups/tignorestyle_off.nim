## Without `.feature: "ignoreStyle".` style-insensitive lookup must NOT kick in.
## The helper module exports `myProcName`, so the differently-spelled call here
## must report `undeclared identifier`.

import deps / mignorestyle

discard my_proc_name(0)
