// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// Function Definitions
Node* v6(void* env, Node* v4);
Closure* v7(void* env, int v3);
Node* v8(void* env, Node* v2);
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
  return {int* tmp0 = malloc(sizeof(int));
*tmp0 = ((Env_v6*)env)->v1(((Env_v6*)env)->v3);
}cons(tmp0, NULL);
}

Closure* v7(void* env, int v3) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v3 = v3;
  env6->v1 = ((Env_v7*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env6;
  c->fn = (void* (*)(void*, void*))v6;
  return c;
}

Node* v8(void* env, Node* v2) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v1 = ((Env_v8*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env7;
  c->fn = (void* (*)(void*, void*))v7;
  return (isEmpty(v2)) ? (NULL) : ((Node*)(Closure*)apply((Closure*)apply(c, *(int*)head(v2)), tail(v2)));
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
  printList((Node*)apply(v0(v9), {int* tmp0 = malloc(sizeof(int));
*tmp0 = 3;
int* tmp1 = malloc(sizeof(int));
*tmp1 = 2;
int* tmp2 = malloc(sizeof(int));
*tmp2 = 1;
}cons(tmp2, cons(tmp1, cons(tmp0, NULL)))));
  return 0;
}

