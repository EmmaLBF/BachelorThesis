
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
Pair_Int_Int* v1(Pair_Int_PairPtr v2);

// closure defitions
// function implementations
Pair_Int_Int* v1(Pair_Int_PairPtr v2) {
  Pair_Int_Int *v11 = NULL;
  if (((v2).fst == 0)) {
    v11 = (v2).snd;
  } else {
    v11 = v1((Pair_Int_PairPtr){ .fst = ((v2).fst - 1), .snd = makePair_Int_Int(((v2).snd)->snd, (((v2).snd)->fst + ((v2).snd)->snd)) });
  }
  return v11;
}

// main
int main(void) {
  printInt((v1((Pair_Int_PairPtr){ .fst = 6, .snd = makePair_Int_Int(0, 1) }))->fst);
  return 0;
}

