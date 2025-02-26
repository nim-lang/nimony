// C header file for testing.

void setCArray(int* arr) {
  arr[0] = 1;
  arr[1] = 2;
}

struct CStruct {
  int field[10];
};

struct CStruct2 {
  struct CStruct cs;
  struct CStruct cs2[2];
};

void setCStruct(struct CStruct* cs) {
  cs->field[0] = 567;
  cs->field[1] = 8910;
}

void setCStruct2(struct CStruct2* cs2) {
  cs2->cs.field[0] = 111;
  cs2->cs.field[1] = 222;
  cs2->cs2[0].field[0] = 333;
  cs2->cs2[0].field[1] = 4444;
  cs2->cs2[1].field[0] = 55555;
  cs2->cs2[1].field[1] = 666666;
}
