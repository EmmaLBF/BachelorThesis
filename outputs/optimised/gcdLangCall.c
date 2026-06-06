
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

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
int v0(Pair_Int_Int *v1);

// closure defitions
// function implementations
int v0(Pair_Int_Int *v1) {
  if (((v1)->snd == 0)) {
    return (v1)->fst;
  } else {
    return v0(makePair_Int_Int((v1)->snd, ((v1)->fst % (v1)->snd)));
  }
}

// main
int main(void) {
  printInt(v0(makePair_Int_Int(30, 10)));
  return 0;
}

