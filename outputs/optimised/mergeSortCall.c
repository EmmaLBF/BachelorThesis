
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

// pair type defitions
typedef struct Pair_Int_ListInt {
  int fst;
  ListInt* snd;
} Pair_Int_ListInt;

Pair_Int_ListInt* makePair_Int_ListInt(int fst, ListInt* snd) {
  Pair_Int_ListInt* p = malloc(sizeof(Pair_Int_ListInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

typedef struct Pair_ListInt_ListInt {
  ListInt* fst;
  ListInt* snd;
} Pair_ListInt_ListInt;

Pair_ListInt_ListInt* makePair_ListInt_ListInt(ListInt* fst, ListInt* snd) {
  Pair_ListInt_ListInt* p = malloc(sizeof(Pair_ListInt_ListInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
ListInt* v8(ListInt* v9, ListInt* v10);
Pair_ListInt_ListInt v19(Pair_Int_ListInt v20);
int v29(ListInt* v30);
ListInt* v0(ListInt* v1);

// closure defitions
// function implementations
ListInt* v8(ListInt* v9, ListInt* v10) {
  if (((v9) == NULL)) {
    return v10;
  } else {
    ListInt* v41 = NULL;
    if (((v10) == NULL)) {
      v41 = v9;
    } else {
      ListInt* v38 = NULL;
      if (((v9)->head < (v10)->head)) {
        v38 = consInt((v9)->head, v8((v9)->tail, v10));
      } else {
        v38 = consInt((v10)->head, v8((v10)->tail, v9));
      }
      v41 = v38;
    }
    return v41;
  }
}

Pair_ListInt_ListInt v19(Pair_Int_ListInt v20) {
  Pair_ListInt_ListInt v56 = { .fst = NULL, .snd = NULL};
  if (((v20).fst == 0)) {
    v56 = (Pair_ListInt_ListInt){ .fst = NULL, .snd = (v20).snd };
  } else {
    if ((((v20).snd) == NULL)) {
      v56 = (Pair_ListInt_ListInt){ .fst = NULL, .snd = NULL };
    } else {
      Pair_ListInt_ListInt v28 = v19((Pair_Int_ListInt){ .fst = ((v20).fst - 1), .snd = ((v20).snd)->tail });
      v56 = (Pair_ListInt_ListInt){ .fst = consInt(((v20).snd)->head, (v28).fst), .snd = (v28).snd };
    }
  }
  return v56;
}

int v29(ListInt* v30) {
  if (((v30) == NULL)) {
    return 0;
  } else {
    return (1 + v29((v30)->tail));
  }
}

ListInt* v0(ListInt* v1) {
  if (((v1) == NULL)) {
    return NULL;
  } else {
    ListInt* v74 = NULL;
    if ((((v1)->tail) == NULL)) {
      v74 = consInt((v1)->head, NULL);
    } else {
      Pair_ListInt_ListInt v7 = v19((Pair_Int_ListInt){ .fst = (v29(v1) / 2), .snd = v1 });
      v74 = v8(v0((v7).fst), v0((v7).snd));
    }
    return v74;
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

