
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

typedef struct Pair_Int_PairPtr {
  int fst;
  Pair_Int_Int* snd;
} Pair_Int_PairPtr;

Pair_Int_PairPtr* makePair_Int_PairPtr(int fst, Pair_Int_Int* snd) {
  Pair_Int_PairPtr* p = malloc(sizeof(Pair_Int_PairPtr));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
Pair_Int_Int* v11(void* env11, void* v9_raw);
Pair_Int_Int* v13(void* env13, void* v6_raw);
Pair_Int_Int* v15(void* env15, void* v4_raw);
Pair_Int_Int* v1(Pair_Int_PairPtr *v2);

// closure defitions
typedef struct {
} Env_v1;

typedef struct {
    int v4;
    Pair_Int_Int *v6;
} Env_v11;

typedef struct {
    int v4;
} Env_v13;

typedef struct {
    Pair_Int_PairPtr *v2;
} Env_v15;

// function implementations
Pair_Int_Int* v11(void* env11, void* v9_raw) {
  int v9 = *(int*)v9_raw;
  if ((((Env_v11*)env11)->v4 == 0)) return ((Env_v11*)env11)->v6;
  return v1(makePair_Int_PairPtr((((Env_v11*)env11)->v4 - 1), makePair_Int_Int(v9, ((((Env_v11*)env11)->v6)->fst + v9))));
}

Pair_Int_Int* v13(void* env13, void* v6_raw) {
  Pair_Int_Int *v6 = (Pair_Int_Int*)v6_raw;
  Env_v11* env11 = malloc(sizeof(Env_v11));
  env11->v4 = ((Env_v13*)env13)->v4;
  env11->v6 = v6;
  return v11(env11, box_int((v6)->snd));
}

Pair_Int_Int* v15(void* env15, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v13* env13 = malloc(sizeof(Env_v13));
  env13->v4 = v4;
  return v13(env13, (void*)((((Env_v15*)env15)->v2)->snd));
}

Pair_Int_Int* v1(Pair_Int_PairPtr *v2) {
  Env_v15* env15 = malloc(sizeof(Env_v15));
  env15->v2 = v2;
  return v15(env15, box_int((v2)->fst));
}

// main
int main(void) {
  printInt((v1(makePair_Int_PairPtr(6, makePair_Int_Int(0, 1))))->fst);
  return 0;
}

