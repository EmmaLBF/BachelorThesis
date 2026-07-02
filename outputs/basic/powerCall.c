
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
int v8(void* env8, void* v5_raw);
int v10(void* env10, void* v3_raw);
int v0(Pair_Int_Int *v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
    int v3;
} Env_v8;

typedef struct {
    Pair_Int_Int *v1;
} Env_v10;

// function implementations
int v8(void* env8, void* v5_raw) {
  int v5 = *(int*)v5_raw;
  if ((v5 == 0)) return 1;
  return (((Env_v8*)env8)->v3 * v0(makePair_Int_Int(((Env_v8*)env8)->v3, (v5 - 1))));
}

int v10(void* env10, void* v3_raw) {
  int v3 = *(int*)v3_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  env8->v3 = v3;
  return v8(env8, box_int((((Env_v10*)env10)->v1)->snd));
}

int v0(Pair_Int_Int *v1) {
  Env_v10* env10 = malloc(sizeof(Env_v10));
  env10->v1 = v1;
  return v10(env10, box_int((v1)->fst));
}

// main
int main(void) {
  printInt(v0(makePair_Int_Int(4, 2)));
  return 0;
}

