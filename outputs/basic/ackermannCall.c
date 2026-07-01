
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
int v7(void* env7, void* v5_raw);
int v9(void* env9, void* v3_raw);
int v0(Pair_Int_Int *v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
    int v3;
} Env_v7;

typedef struct {
    Pair_Int_Int *v1;
} Env_v9;

// function implementations
int v7(void* env7, void* v5_raw) {
  int v5 = *(int*)v5_raw;
  if ((((Env_v7*)env7)->v3 == 0)) return (v5 + 1);
  if ((v5 == 0)) return v0(makePair_Int_Int((((Env_v7*)env7)->v3 - 1), 1));
  return v0(makePair_Int_Int((((Env_v7*)env7)->v3 - 1), v0(makePair_Int_Int(((Env_v7*)env7)->v3, (v5 - 1)))));
}

int v9(void* env9, void* v3_raw) {
  int v3 = *(int*)v3_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v3 = v3;
  return v7(env7, box_int((((Env_v9*)env9)->v1)->snd));
}

int v0(Pair_Int_Int *v1) {
  Env_v9* env9 = malloc(sizeof(Env_v9));
  env9->v1 = v1;
  return v9(env9, box_int((v1)->fst));
}

// main
int main(void) {
  printInt(v0(makePair_Int_Int(2, 6)));
  return 0;
}

