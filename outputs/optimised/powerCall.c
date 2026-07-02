
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
int v0(Pair_Int_Int v1);

// closure defitions
// function implementations
int v0(Pair_Int_Int v1) {
  int v8 = 0;
  if (((v1).snd == 0)) {
    v8 = 1;
  } else {
    v8 = ((v1).fst * v0((Pair_Int_Int){ .fst = (v1).fst, .snd = ((v1).snd - 1) }));
  }
  return v8;
}

// main
int main(void) {
  printInt(v0((Pair_Int_Int){ .fst = 4, .snd = 2 }));
  return 0;
}

