
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
NodeInt* v6(void* env6, void* v4_raw);
Closure* v7(void* env7, void* v3_raw);
NodeInt* v10(void* env10, void* v2_raw);
Closure* v0(int (*v1)(int));
int v11(int v5);

// closure defitions
typedef struct {
    int (*v1)(int);
    int v3;
} Env_v6;

typedef struct {
    int (*v1)(int);
} Env_v7;

typedef struct {
    int (*v1)(int);
} Env_v10;

// function implementations
NodeInt* v6(void* env6, void* v4_raw) {
  NodeInt* v4 = (NodeInt*)v4_raw;
  return consInt(((Env_v6*)env6)->v1(((Env_v6*)env6)->v3), (NodeInt*)apply((Closure*)v0(((Env_v6*)env6)->v1), v4));
}

Closure* v7(void* env7, void* v3_raw) {
  int v3 = *(int*)v3_raw;
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v3 = v3;
  env6->v1 = ((Env_v7*)env7)->v1;
  Closure* c6 = malloc(sizeof(Closure));
  c6->env = env6;
  c6->fn = (void* (*)(void*, void*))v6;
  return c6;
}

NodeInt* v10(void* env10, void* v2_raw) {
  NodeInt* v2 = (NodeInt*)v2_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v1 = ((Env_v10*)env10)->v1;
  Closure* c7 = malloc(sizeof(Closure));
  c7->env = env7;
  c7->fn = (void* (*)(void*, void*))v7;
  return ((isEmptyInt(v2)) ? (NULL) : ((NodeInt*)apply((Closure*)apply((Closure*)c7, box_int((headInt(v2)))), tailInt(v2))));
}

Closure* v0(int (*v1)(int)) {
  Env_v10* env10 = malloc(sizeof(Env_v10));
  env10->v1 = v1;
  Closure* c10 = malloc(sizeof(Closure));
  c10->env = env10;
  c10->fn = (void* (*)(void*, void*))v10;
  return c10;
}

int v11(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  printListInt((NodeInt*)apply((Closure*)v0(v11), consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

