import deps / mdeclared
import deps / modexcept except exceptA, exceptC
from deps / modfrom import fromA, fromC

let allImported = [exceptB, fromA, fromC]
let exceptDeclared = [declared(exceptA), declared(exceptB), declared(exceptC)]
let fromDeclared = [declared(fromA), declared(fromB), declared(fromC)]
