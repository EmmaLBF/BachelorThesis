
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
NodeInt* v0(int (*v1)(int), NodeInt* v2);
int v11(int v5);

// closure defitions
// function implementations
NodeInt* v0(int (*v1)(int), NodeInt* v2) {
  if (((v2) == NULL)) {
    return NULL;
  } else {
    return consInt(v1((v2)->head), v0(v1, (v2)->tail));
  }
}

int v11(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  printListInt(v0(v11, consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

