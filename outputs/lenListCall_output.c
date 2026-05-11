
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
int v4(Node* v3);
int (*v5(int v2))(Node*);
int v0(Node* v1);

// closure defitions
// function implementations
int v4(Node* v3) {
  return (1 + v0(v3));
}

int (*v5(int v2))(Node*) {
  return v4;
}

int v0(Node* v1) {
  Node* v6 = v1;
  return (isEmpty(v6)) ? (0) : (v5(*(int*)(head(v6)))(tail(v6)));
}

// main
int main(void) {
  printf("%d\n", v0(cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL)))));
  return 0;
}

