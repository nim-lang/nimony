# Re-exporting a symbol does not bring its module's name along: what the
# re-export filter hides stays hidden.
import deps/mreexportsymbol

discard mreexportsymbol_source.newConn()
