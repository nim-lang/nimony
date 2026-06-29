## The `{.gpu.}` color: a custom pragma template. Marking a proc `{.gpu.}` makes
## it device-ELIGIBLE — it must obey the GPU restricted subset (enforced by the
## `gpucheck` module plugin) and may later be lowered to SPIR-V by `ghast`.
## Custom pragmas survive into the semmed decl as `(pragma <sym>)`, so the
## checker (and any backend) can introspect the color.
template gpu*() {.pragma.}
