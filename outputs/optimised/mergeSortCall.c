
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
Pair_NodeInt_NodeInt v17(Pair_Int_NodeInt v18);
int v24(NodeInt* v25);
NodeInt* v0(NodeInt* v1);

// closure defitions
// function implementations
NodeInt* v7(NodeInt* v8, NodeInt* v9) {
  if (((v8) == NULL)) {
    return v9;
  } else {
    NodeInt* v33 = NULL;
    if (((v9) == NULL)) {
      v33 = v8;
    } else {
      NodeInt* v29 = NULL;
      if (((v8)->head < (v9)->head)) {
        v29 = consInt((v8)->head, v7((v8)->tail, v9));
      } else {
        v29 = consInt((v9)->head, v7((v9)->tail, v8));
      }
      v33 = v29;
    }
    return v33;
  }
}

Pair_NodeInt_NodeInt v17(Pair_Int_NodeInt v18) {
  Pair_NodeInt_NodeInt v55 = { .fst = NULL, .snd = NULL};
  if (((v18).fst == 0)) {
    v55 = (Pair_NodeInt_NodeInt){ .fst = NULL, .snd = (v18).snd };
  } else {
    if ((((v18).snd) == NULL)) {
      v55 = (Pair_NodeInt_NodeInt){ .fst = NULL, .snd = NULL };
    } else {
      Pair_NodeInt_NodeInt v23 = v17((Pair_Int_NodeInt){ .fst = ((v18).fst - 1), .snd = ((v18).snd)->tail });
      v55 = (Pair_NodeInt_NodeInt){ .fst = consInt(((v18).snd)->head, (v23).fst), .snd = (v23).snd };
    }
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
    NodeInt* v85 = NULL;
    if ((((v1)->tail) == NULL)) {
      v85 = consInt((v1)->head, NULL);
    } else {
      Pair_NodeInt_NodeInt v6 = v17((Pair_Int_NodeInt){ .fst = (v24(v1) / 2), .snd = v1 });
      v85 = v7(v0((v6).fst), v0((v6).snd));
    }
    return v85;
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

