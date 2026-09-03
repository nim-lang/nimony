# cross-module closure ABI shape: hook var + template callee defined in
# one module, closure assigned and fired from the importing module
{.feature: "lenientnils".}
import deps/mclosurehook

setup()
fire()
