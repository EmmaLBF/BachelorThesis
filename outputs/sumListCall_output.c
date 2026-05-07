
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
int v4(void* env, Node* v3);
Closure* v5(int v2);
int v0(Node* v1);

// closure defitions
typedef struct {
    int v2;
} Env_v4;

// function implementations
int v4(void* env, Node* v3) {
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
  Node* v6 = v1;
  return (isEmpty(v6)) ? (0) : ((int)(intptr_t)apply(v5(*(int*)(head(v6))), tail(v6)));
}

// main
int main(void) {
  printf("%d\n", v0(cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL)))));
  return 0;
}

