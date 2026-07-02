
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

// pair type defitions
typedef struct Pair_Int_Int {
  int fst;
  int snd;
} Pair_Int_Int;

Pair_Int_Int* makePair_Int_Int(int fst, int snd) {
  Pair_Int_Int* p = malloc(sizeof(Pair_Int_Int));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
int v0(List* v1);
List* v15(List* v16, List* v17);
bool v29(Pair_Int_Int *v30, List* v31);
List* v20(int v21, int v22, List* v23, int v24);
List* v9(int v10, int v11, List* v12);
List* v6(void* env6, void* v7_raw, void* v8_raw);

// closure defitions
typedef struct {
    int v5;
} Env_v6;

typedef struct {
} Env_v216;

// function implementations
int v0(List* v1) {
  if (((v1) == NULL)) return 0;
  return (1 + v0((v1)->tail));
}

List* v15(List* v16, List* v17) {
  if (((v16) == NULL)) return v17;
  return cons((v16)->head, v15((v16)->tail, v17));
}

bool v29(Pair_Int_Int *v30, List* v31) {
  if (((v31) == NULL)) return true;
  Pair_Int_Int v39 = *(Pair_Int_Int*)((v31)->head);
  return (!((((v30)->snd == (v39).snd) || (abs(((v30)->snd - (v39).snd)) == abs(((v30)->fst - (v39).fst))))) && v29(v30, (v31)->tail));
}

List* v20(int v21, int v22, List* v23, int v24) {
  if ((v24 == v21)) return NULL;
  Pair_Int_Int *v26 = makePair_Int_Int(v22, v24);
  List* v28 = v20(v21, v22, v23, (v24 + 1));
  return ((v29(v26, v23)) ? (cons(cons(v26, v23), v28)) : (v28));
}

List* v9(int v10, int v11, List* v12) {
  if (((v12) == NULL)) return NULL;
  return v15(v20(v10, v11, (v12)->head, 0), v9(v10, v11, (v12)->tail));
}

List* v6(void* env6, void* v7_raw, void* v8_raw) {
  int v7 = *(int*)v7_raw;
  List* v8 = (List*)v8_raw;
  if ((v7 == ((Env_v6*)env6)->v5)) return v8;
  return v6(env6, box_int((v7 + 1)), (void*)(v9(((Env_v6*)env6)->v5, v7, v8)));
}

// main
int main(void) {
  int v5 = 4;
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v5 = v5;
  printInt(v0(v6(env6, box_int(0), (void*)(cons(NULL, NULL)))));
  return 0;
}

