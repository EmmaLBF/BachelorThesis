
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
int v4(void* env4, void* v3_raw);
Closure* v5(int v2);
int v0(NodeInt* v1);

// closure defitions
typedef struct {
    NodeInt* v3;
    int v2;
} Env_v4;

typedef struct {
    int v2;
} Env_v5;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
int v4(void* env4, void* v3_raw) {
  NodeInt* v3 = (NodeInt*)v3_raw;
  return (((Env_v4*)env4)->v2 + v0(v3));
}

Closure* v5(int v2) {
  Env_v4* env4 = malloc(sizeof(Env_v4));
  env4->v2 = v2;
  Closure* c4 = malloc(sizeof(Closure));
  c4->env = env4;
  c4->fn = (void* (*)(void*, void*))v4;
  return c4;
}

int v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return 0;
  } else {
    return (int)(intptr_t)((Closure*)v5((v1)->head))->fn(((Closure*)v5((v1)->head))->env, (v1)->tail);
  }
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

