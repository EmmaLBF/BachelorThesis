
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
int v15(void* env15, void* v5_raw);
int v17(void* env17, void* v3_raw);
int v0(Pair_Int_Int *v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
    int v3;
} Env_v15;

typedef struct {
    Pair_Int_Int *v1;
} Env_v17;

// function implementations
int v15(void* env15, void* v5_raw) {
  int v5 = *(int*)v5_raw;
  if ((((Env_v15*)env15)->v3 == 0)) return (v5 + 1);
  if ((v5 == 0)) return v0(makePair_Int_Int((((Env_v15*)env15)->v3 - 1), 1));
  return v0(makePair_Int_Int((((Env_v15*)env15)->v3 - 1), v0(makePair_Int_Int(((Env_v15*)env15)->v3, (v5 - 1)))));
}

int v17(void* env17, void* v3_raw) {
  int v3 = *(int*)v3_raw;
  Env_v15* env15 = malloc(sizeof(Env_v15));
  env15->v3 = v3;
  return v15(env15, box_int((((Env_v17*)env17)->v1)->snd));
}

int v0(Pair_Int_Int *v1) {
  Env_v17* env17 = malloc(sizeof(Env_v17));
  env17->v1 = v1;
  return v17(env17, box_int((v1)->fst));
}

// main
int main(void) {
  printInt(v0(makePair_Int_Int(2, 6)));
  return 0;
}

