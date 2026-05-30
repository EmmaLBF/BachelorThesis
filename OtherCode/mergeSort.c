// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/listLib.c"

// int* merge(int* list, int start, int end) {
//     int mid = start + (end - start) / 2;
//     int n1 = mid - start + 1;
//     int n2 = end - mid;

//     // make list for each half and put numbers in
//     int* left = (int*)malloc(n1 * sizeof(int));
//     int* right = (int*)malloc(n2 * sizeof(int));
//     for (int i = 0; i < n1; i++) left[i] = list[start + i];
//     for (int j = 0; j < n2; j++) right[j] = list[mid + 1 + j];

//     // put elements into the given list in the right order
//     int i = 0, j = 0, k = start;
//     while (i < n1 && j < n2) {
//         if (left[i] <= right[j]) list[k++] = left[i++];
//         else list[k++] = right[j++];
//     }

//     // put remaining elements into list if we finished inserting all element of one before the other
//     while (i < n1) list[k++] = left[i++];
//     while (j < n2) list[k++] = right[j++];

//     // free space and return
//     // fair to free here since I don't in generated C?
//     free(left);
//     free(right);
//     return list;
// }

// int* mergeSort(int* list, int start, int end) {
//     if (start >= end) return list;
//     int mid = start + (end - start) / 2;
//     mergeSort(list, start, mid);
//     mergeSort(list, mid + 1, end);
//     return merge(list, start, end);
// }

// int main() {
//    int* list = LIST1000_ARRAY();
//     size_t total_size = sizeof(list);
//     size_t element_size = sizeof(list[0]);
//     size_t len = total_size / element_size;
//     int* sorted = mergeSort(list, 0, len - 1);
//     for (size_t i = 0; i < len; i++) printf("%d ", sorted[i]);
//     return 0;
// }

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

NodeInt* mergeSort(NodeInt* list, size_t size) {
    if (size <= 1) return list;
    NodeInt* split[2] = {NULL, NULL};
    splitHalf(list, size, split);
    size_t half = size / 2;
    NodeInt* leftMerged = mergeSort(split[0], half);
    NodeInt* rightMerged = mergeSort(split[1], size - half);
    return merge(leftMerged, rightMerged);
}

int main() {
    int seed = 42;
   int len = 1000;
    NodeInt* list = LIST(len, seed);
    NodeInt* sorted = mergeSort(list, len);
    printListInt(sorted);
    return 0;
}
