// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/lib.c"

// pair type defitions
typedef struct Pair_Int_ListInt {
  int fst;
  ListInt* snd;
} Pair_Int_ListInt;

typedef struct Pair_ListInt_ListInt {
  ListInt* fst;
  ListInt* snd;
} Pair_ListInt_ListInt;

Pair_ListInt_ListInt splitN(Pair_Int_ListInt p) {
    if (p.fst == 0) return (Pair_ListInt_ListInt){.fst = NULL, .snd = p.snd};
    if (p.snd == NULL) return (Pair_ListInt_ListInt){.fst = NULL, .snd = NULL};
    Pair_ListInt_ListInt recur = splitN((Pair_Int_ListInt){ .fst = p.fst - 1, .snd = p.snd->tail});
    return (Pair_ListInt_ListInt){ .fst = consInt(p.snd->head, (recur.fst)), .snd = recur.snd};
}

ListInt* merge(ListInt* list1, ListInt* list2) {
    if (list1 == NULL) return list2;
    if (list2 == NULL) return list1;
    if (list1->head <= list2->head)
        return consInt(list1->head, merge(list1->tail, list2));
    else
        return consInt(list2->head, merge(list1, list2->tail));
}

int sizeList(ListInt* l) {
    if (l == NULL) return 0;
    return 1 + sizeList(l->tail);
}

ListInt* v0(ListInt* list) { // mergeSort
    int size = sizeList(list);
    if (size <= 1) return list;
    Pair_ListInt_ListInt split = splitN((Pair_Int_ListInt){ .fst = size / 2, .snd = list});
    ListInt* leftMerged = v0(split.fst);
    ListInt* rightMerged = v0(split.snd);
    return merge(leftMerged, rightMerged);
}

int main() {
  printListInt(v0(LIST(30000, 42)));
    return 0;
}

