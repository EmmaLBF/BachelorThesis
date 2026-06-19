// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/lib.c"

ListInt* map(int (*func)(int), ListInt* l) {
  if (!l) return NULL;
  return consInt(func(l->head), map(func, l->tail));
}

int f(int input) {
    return input * 2;
}

// main
int main(void) {
    int seed = 42;
    int len = 5;
    ListInt* list = LIST(len, seed);
    printListInt(map(f, list));
    return 0;
}