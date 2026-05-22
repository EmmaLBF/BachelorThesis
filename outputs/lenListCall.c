
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
int v0(Node* v1);

// closure defitions
// function implementations
int v0(Node* v1) {
  if (isEmpty(v1)) {
    return 0;
  } else {
    int v2 = *(int*)((head(v1)));
    Node* v3 = tail(v1);
    return (1 + v0(v3));
  }
}

// main
int main(void) {
  printInt(v0(cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL)))));
  return 0;
}

