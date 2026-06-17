// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/listLib.c"

// pair type defitions
typedef struct Pair_Int_NodeInt {
  int fst;
  NodeInt* snd;
} Pair_Int_NodeInt;

typedef struct Pair_NodeInt_NodeInt {
  NodeInt* fst;
  NodeInt* snd;
} Pair_NodeInt_NodeInt;

Pair_NodeInt_NodeInt splitN(Pair_Int_NodeInt p) {
    if (p.fst == 0) return (Pair_NodeInt_NodeInt){.fst = NULL, .snd = p.snd};
    if (p.snd == NULL) return (Pair_NodeInt_NodeInt){.fst = NULL, .snd = NULL};
    Pair_NodeInt_NodeInt recur = splitN((Pair_Int_NodeInt){ .fst = p.fst - 1, .snd = p.snd->tail});
    return (Pair_NodeInt_NodeInt){ .fst = consInt(p.snd->head, (recur.fst)), .snd = recur.snd};
}

NodeInt* merge(NodeInt* list1, NodeInt* list2) {
    if (list1 == NULL) return list2;
    if (list2 == NULL) return list1;
    if (list1->head <= list2->head)
        return consInt(list1->head, merge(list1->tail, list2));
    else
        return consInt(list2->head, merge(list1, list2->tail));
}

int sizeList(NodeInt* l) {
    NodeInt* temp = l;
    int len = 0;
    while (temp) {
        len++;
        temp = temp->tail;
    }
    return len;
}

NodeInt* v0(NodeInt* list) { // mergeSort
    int size = sizeList(list);
    if (size <= 1) return list;
    Pair_NodeInt_NodeInt split = splitN((Pair_Int_NodeInt){ .fst = size / 2, .snd = list});
    NodeInt* leftMerged = v0(split.fst);
    NodeInt* rightMerged = v0(split.snd);
    return merge(leftMerged, rightMerged);
}

int main() {
  printListInt(v0(LIST(10, 42)));
    return 0;
}

