// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/listLib.c"

int* merge(int* list, int start, int end) {
    int mid = start + (end - start) / 2;
    int n1 = mid - start + 1;
    int n2 = end - mid;

    // make list for each half and put numbers in
    int* left = (int*)malloc(n1 * sizeof(int));
    int* right = (int*)malloc(n2 * sizeof(int));
    for (int i = 0; i < n1; i++) left[i] = list[start + i];
    for (int j = 0; j < n2; j++) right[j] = list[mid + 1 + j];

    // put elements into the given list in the right order
    int i = 0, j = 0, k = start;
    while (i < n1 && j < n2) {
        if (left[i] <= right[j]) list[k++] = left[i++];
        else list[k++] = right[j++];
    }

    // put remaining elements into list if we finished inserting all element of one before the other
    while (i < n1) list[k++] = left[i++];
    while (j < n2) list[k++] = right[j++];

    // free space and return
    // fair to free here since I don't in generated C?
    free(left);
    free(right);
    return list;
}

int* mergeSort(int* list, int start, int end) {
    if (start >= end) return list;
    int mid = start + (end - start) / 2;
    mergeSort(list, start, mid);
    mergeSort(list, mid + 1, end);
    return merge(list, start, end);
}

int main() {
   int* list = LIST1000_ARRAY();
    size_t total_size = sizeof(list);
    size_t element_size = sizeof(list[0]);
    size_t len = total_size / element_size;
    int* sorted = mergeSort(list, 0, len - 1);
    for (size_t i = 0; i < len; i++) printf("%d ", sorted[i]);
    return 0;
}