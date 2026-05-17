
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
int v4(void* env, void* v3_raw);
Closure* v5(int v2);
int v0(Node* v1);

// closure defitions
typedef struct {
    int v2;
} Env_v4;

// function implementations
int v4(void* env, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  return (((Env_v4*)env)->v2 + v0(v3));
}

Closure* v5(int v2) {
  Env_v4* env4 = malloc(sizeof(Env_v4));
  env4->v2 = v2;
  Closure* c = malloc(sizeof(Closure));
  c->env = env4;
  c->fn = (void* (*)(void*, void*))v4;
  return c;
}

int v0(Node* v1) {
  Closure* v6 = v5;
  Node* v7 = v1;
  return (isEmpty(v7)) ? (0) : ((int)(intptr_t)apply((Closure*)(int)(intptr_t)apply((Closure*)v6, box_int(*(int*)(head(v7)))), (void*)(tail(v7))));
}

// main
int main(void) {
  int (*v8)(Node*) = (int (*)(Node*))v0;
  Node* v9 = cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL)));
  printInt(v8(v9));
  return 0;
}

