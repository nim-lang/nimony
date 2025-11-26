
type
  ImportedObject* = object
    private: int
    public*: string

  GenObj*[T] = object
    private: T
    public*: T

proc importedProc*(): ImportedObject = ImportedObject(private: 4, public: "abc")
