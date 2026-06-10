// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/listLib.c"

NodeInt* merge(NodeInt* list1, NodeInt* list2) {
    if (list1 == NULL) return list2;
    if (list2 == NULL) return list1;
    if (list1->head <= list2->head)
        return consInt(list1->head, merge(list1->tail, list2));
    else
        return consInt(list2->head, merge(list1, list2->tail));
}


void splitHalf(NodeInt* list, size_t size, NodeInt** res) {
    for (size_t i = 0; i < size / 2; i++) {
        res[0] = consInt(list->head, res[0]);
        list = list->tail;
    }
    for (size_t i = size / 2; i < size; i++) {
        res[1] = consInt(list->head, res[1]);
        list = list->tail;
    }
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
    NodeInt* split[2] = {NULL, NULL};
    splitHalf(list, size, split);
    size_t half = size / 2;
    NodeInt* leftMerged = v0(split[0]);
    NodeInt* rightMerged = v0(split[1]);
    return merge(leftMerged, rightMerged);
}

int main() {
  printListInt(v0(LIST(10000, 42)));
    return 0;
}

