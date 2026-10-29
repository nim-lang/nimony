# Imported only by the plugin `mplugin_bindsym_import`, never by its user.

type Id* = distinct int
func `==`*(a, b: Id): bool {.borrow.}

let base = 40 # needs this module's initialization to run

proc hello*(): int = base + 2
