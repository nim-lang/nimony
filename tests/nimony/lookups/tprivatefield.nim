
import deps / mprivatefield

var x = importedProc() #: ImportedObject
x.public = "valid"
x.private = 0

var y = ImportedObject(public: "my", private: 45)

let z = GenObj[int](public: 12, private: 89)
discard z.private
