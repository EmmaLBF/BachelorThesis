
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
int v6(void* env6, void* v5_raw);
int v8(void* env8, void* v3_raw);
int v0(Pair_Int_Int *v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
    int v3;
} Env_v6;

typedef struct {
    Pair_Int_Int *v1;
} Env_v8;

// function implementations
int v6(void* env6, void* v5_raw) {
  int v5 = *(int*)v5_raw;
  if ((v5 == 0)) return ((Env_v6*)env6)->v3;
  return v0(makePair_Int_Int(v5, (((Env_v6*)env6)->v3 % v5)));
}

int v8(void* env8, void* v3_raw) {
  int v3 = *(int*)v3_raw;
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v3 = v3;
  return v6(env6, box_int((((Env_v8*)env8)->v1)->snd));
}

int v0(Pair_Int_Int *v1) {
  Env_v8* env8 = malloc(sizeof(Env_v8));
  env8->v1 = v1;
  return v8(env8, box_int((v1)->fst));
}

// main
int main(void) {
  printInt(v0(makePair_Int_Int(30, 10)));
  return 0;
}

