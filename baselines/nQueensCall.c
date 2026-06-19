// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/lib.c"

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

// lenList
int v0(List* v1) {
    int len = 0;
    while (v1) {
        len++;
        v1 = v1->tail;
    }
    return len;
}

// appendList
List* v14(List* v15, List* v16) {
    List* acc = v16;
    while (v15) {
        acc = cons(v15->head, acc);
        v15 = v15->tail;
    }
    return acc;
}

// queenSafe
bool v26(Pair_Int_Int* v27, List* v28) {
    if (v28 == NULL) return true;
    Pair_Int_Int v34 = *(Pair_Int_Int*)v28->head;
    return (!(v27->snd == v34.snd || (abs(v27->snd - v34.snd) == abs(v27->fst - v34.fst))) && (v26(v27, v28->tail)));
}

// tryCols
List* v19(int v20, int v21, List* v22, int v23) {
    if (v23 == v20) return NULL;
    Pair_Int_Int* v24 = makePair_Int_Int(v21, v23);
    List* rest = v19(v20, v21, v22, (v23 + 1));
    if (v26(v24, v22)) return (cons(cons(v24, v22), rest));
    return rest;
}

// extendAll
List* v8(int n, int v10, List* v11) {
    if (v11 == NULL) return NULL;
    return v14(v19(n, v10, v11->head, 0), (v8(n, v10, v11->tail)));
}

// nQueens
List* v55(int n, int v6, List* v7) {
    while (v6 != n) {
        v6++;
        v7 = v8(n, v6, v7);
    }
    return v7;
}

int main() {
  int v5 = 10;
    printInt(v0(v55(v5, 0, cons(NULL, NULL))));
    return 0;
}



