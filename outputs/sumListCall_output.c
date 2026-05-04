// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// Function Definitions
int v4(void* env, Node* v3);
Closure* v5(int v2);
int v0(Node* v1);

// Compiled Program
typedef struct {
    int v2;
} Env_v4;

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
  return (isEmpty(v1)) ? (0) : ((int)(intptr_t)apply(v5(*(int*)head(v1)), tail(v1)));
}

int main(void) {
  printf("%d\n", v0(cons(mk_int((int)(1)), cons(mk_int((int)(2)), cons(mk_int((int)(3)), NULL)))));
  return 0;
}

