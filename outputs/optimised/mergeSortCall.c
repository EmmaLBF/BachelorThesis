
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
typedef struct Pair_Int_NodeInt {
  int fst;
  NodeInt* snd;
} Pair_Int_NodeInt;

Pair_Int_NodeInt* makePair_Int_NodeInt(int fst, NodeInt* snd) {
  Pair_Int_NodeInt* p = malloc(sizeof(Pair_Int_NodeInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

typedef struct Pair_NodeInt_NodeInt {
  NodeInt* fst;
  NodeInt* snd;
} Pair_NodeInt_NodeInt;

Pair_NodeInt_NodeInt* makePair_NodeInt_NodeInt(NodeInt* fst, NodeInt* snd) {
  Pair_NodeInt_NodeInt* p = malloc(sizeof(Pair_NodeInt_NodeInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
NodeInt* v8(NodeInt* v9, NodeInt* v10);
Pair_NodeInt_NodeInt v19(Pair_Int_NodeInt v20);
int v29(NodeInt* v30);
NodeInt* v0(NodeInt* v1);

// closure defitions
// function implementations
NodeInt* v8(NodeInt* v9, NodeInt* v10) {
  if (((v9) == NULL)) return v10;
  NodeInt* v41 = NULL;
  if (((v10) == NULL)) {
    v41 = v9;
  } else {
    NodeInt* v38 = NULL;
    if (((v9)->head < (v10)->head)) {
      v38 = consInt((v9)->head, v8((v9)->tail, v10));
    } else {
      v38 = consInt((v10)->head, v8((v10)->tail, v9));
    }
    v41 = v38;
  }
  return v41;
}

Pair_NodeInt_NodeInt v19(Pair_Int_NodeInt v20) {
  Pair_NodeInt_NodeInt v56 = { .fst = NULL, .snd = NULL};
  if (((v20).fst == 0)) {
    v56 = (Pair_NodeInt_NodeInt){ .fst = NULL, .snd = (v20).snd };
  } else {
    if ((((v20).snd) == NULL)) {
      v56 = (Pair_NodeInt_NodeInt){ .fst = NULL, .snd = NULL };
    } else {
      Pair_NodeInt_NodeInt v28 = v19((Pair_Int_NodeInt){ .fst = ((v20).fst - 1), .snd = ((v20).snd)->tail });
      v56 = (Pair_NodeInt_NodeInt){ .fst = consInt(((v20).snd)->head, (v28).fst), .snd = (v28).snd };
    }
  }
  return v56;
}

int v29(NodeInt* v30) {
  if (((v30) == NULL)) return 0;
  return (1 + v29((v30)->tail));
}

NodeInt* v0(NodeInt* v1) {
  if (((v1) == NULL)) return NULL;
  NodeInt* v74 = NULL;
  if ((((v1)->tail) == NULL)) {
    v74 = consInt((v1)->head, NULL);
  } else {
    Pair_NodeInt_NodeInt v7 = v19((Pair_Int_NodeInt){ .fst = (v29(v1) / 2), .snd = v1 });
    v74 = v8(v0((v7).fst), v0((v7).snd));
  }
  return v74;
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

