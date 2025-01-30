// C header file for testing.

typedef int CArray[10];

struct CStruct {
  int field[10];
};

struct CStruct2 {
  struct CStruct cs;
  struct CStruct cs2[2];
};
