
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
Node* v0(int (*v1)(int), Node* v2);
int v11(int v5);

// closure defitions
typedef struct {
    int (*v1)(int);
} Env_v7;

// function implementations
Node* v0(int (*v1)(int), Node* v2) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v1 = v1;
  if (isEmpty(v2)) {
    return NULL;
  } else {
    int v3 = *(int*)(head(v2));
    Node* v4 = tail(v2);
    return (Node*)cons(box_int(((Env_v7*)env7)->v1(v3)), v0(((Env_v7*)env7)->v1, v4));
  }
}

int v11(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  printList(v0(v11, cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL)))));
  return 0;
}

