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
int lenList(List* l) {
    if (l == NULL) return 0;
    return 1 + lenList(l->tail);
}

// appendList
List* appendList(List* l1, List* l2) {
    if ((l1 == NULL)) return l2;
    return cons(l1->head, appendList(l1->tail, l2));
}

// queenSafe
bool queenSafe(Pair_Int_Int* q, List* v28) {
    if (v28 == NULL) return true;
    Pair_Int_Int v34 = *(Pair_Int_Int*)v28->head;
    return (!(q->snd == v34.snd || (abs(q->snd - v34.snd) == abs(q->fst - v34.fst))) && (queenSafe(q, v28->tail)));
}

// tryCols
List* tryCols(int v20, int v21, List* v22, int v23) {
    if (v23 == v20) return NULL;
    Pair_Int_Int* v24 = makePair_Int_Int(v21, v23);
    List* rest = tryCols(v20, v21, v22, (v23 + 1));
    if (queenSafe(v24, v22)) return (cons(cons(v24, v22), rest));
    return rest;
}

// extendAll
List* extendAll(int n, int row, List* partials) {
    if (partials == NULL) return NULL;
    return appendList(tryCols(n, row, partials->head, 0), (extendAll(n, row, partials->tail)));
}

// nQueens
List* nQueens(int n, int row, List* partials) {
    if (row == n) return partials;
    return nQueens(n, row + 1, extendAll(n, row, partials));
}

int main() {
  int v5 = 10;
    printInt(lenList(nQueens(v5, 0, cons(NULL, NULL))));
    return 0;
}



