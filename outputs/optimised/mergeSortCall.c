
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
NodeInt* v7(NodeInt* v8, NodeInt* v9);
Pair_NodeInt_NodeInt* v17(Pair_Int_NodeInt* v18);
int v24(NodeInt* v25);
NodeInt* v0(NodeInt* v1);

// closure defitions
// function implementations
NodeInt* v7(NodeInt* v8, NodeInt* v9) {
  if (((v8) == NULL)) {
    return v9;
  } else {
    NodeInt* v29 = NULL;
    if (((v8)->head < (v9)->head)) {
      v29 = consInt((v8)->head, v7((v8)->tail, v9));
    } else {
      v29 = consInt((v9)->head, v7((v9)->tail, v8));
    }
    return ((((v9) == NULL)) ? (v8) : (v29));
  }
}

Pair_NodeInt_NodeInt* v17(Pair_Int_NodeInt* v18) {
  int v19 = (v18)->fst;
  NodeInt* v20 = (v18)->snd;
  Pair_NodeInt_NodeInt* v55 = NULL;
  if ((v19 == 0)) {
    v55 = makePair_NodeInt_NodeInt(NULL, v20);
  } else {
    Pair_NodeInt_NodeInt* v23 = v17(makePair_Int_NodeInt((v19 - 1), (v20)->tail));
    v55 = ((((v20) == NULL)) ? (makePair_NodeInt_NodeInt(NULL, NULL)) : (makePair_NodeInt_NodeInt(consInt((v20)->head, (v23)->fst), (v23)->snd)));
  }
  return v55;
}

int v24(NodeInt* v25) {
  if (((v25) == NULL)) {
    return 0;
  } else {
    return (1 + v24((v25)->tail));
  }
}

NodeInt* v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return NULL;
  } else {
    if ((((v1)->tail) == NULL)) {
      return consInt((v1)->head, NULL);
    } else {
      Pair_NodeInt_NodeInt* v6 = v17(makePair_Int_NodeInt((v24(v1) / 2), v1));
      return v7(v0((v6)->fst), v0((v6)->snd));
    }
  }
}

// main
int main(void) {
  printListInt(v0(LIST(1000, 42)));
  return 0;
}

