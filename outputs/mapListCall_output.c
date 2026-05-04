// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// Function Definitions
Node* v6(void* env, Node* v4);
Closure* v7(void* env, int v3);
Closure* v8(void* env, Node* v2);
Closure* v0(int (*v1)(int));
int v9(int v5);

// Compiled Program
typedef struct {
    int (*v1)(int);
    int v3;
} Env_v6;

typedef struct {
    int (*v1)(int);
} Env_v7;

typedef struct {
    int (*v1)(int);
} Env_v8;

Node* v6(void* env, Node* v4) {
  return cons(&(int){v1(((Env_v6*)env)->v3)}, (Node*)apply(v0(((Env_v6*)env)->v1), v4));
}

Closure* v7(void* env, int v3) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v3 = v3;
  env6->v1 = ((Env_v6*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env6;
  c->fn = (void* (*)(void*, void*))v6;
  return c;
}

Closure* v8(void* env, Node* v2) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v2 = v2;
  env7->v1 = ((Env_v7*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env7;
  c->fn = (void* (*)(void*, void*))v7;
  return c;
}

Closure* v0(int (*v1)(int)) {
  Env_v8* env8 = malloc(sizeof(Env_v8));
  env8->v1 = v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env8;
  c->fn = (void* (*)(void*, void*))v8;
  return c;
}

int v9(int v5) {
  return (v5 * 2);
}

int main(void) {
  printf("%d\n", (Node*)apply(v0(v9), cons(&(int){1}, cons(&(int){2}, cons(&(int){3}, NULL)))));
  return 0;
}

