// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// Function Definitions
int v4(Node* v3);
int (*v5(int v2))(Node*);
int v0(Node* v1);

// Compiled Program
int v4(Node* v3) {
  return (1 + v0(v3));
}

int (*v5(int v2))(Node*) {
  return v4;
}

int v0(Node* v1) {
  return (isEmpty(v1)) ? (0) : (v5(*(int*)head(v1))(tail(v1)));
}

int main(void) {
  printf("%d\n", v0(cons(mk_int((int)(1)), cons(mk_int((int)(2)), cons(mk_int((int)(3)), NULL)))));
  return 0;
}

